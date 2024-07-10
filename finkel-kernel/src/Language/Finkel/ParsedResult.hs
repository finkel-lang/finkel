{-# LANGUAGE CPP #-}

module Language.Finkel.ParsedResult (
  fnkParsedResultAction
  ) where


#if MIN_VERSION_ghc(8,6,0)

#include "ghc_modules.h"

-- base
import Control.Exception                 (displayException, throwIO)
import Control.Monad.IO.Class            (MonadIO (..))
import Data.Maybe                        (fromMaybe)
import System.Console.GetOpt             (ArgOrder (..), getOpt)
import System.Environment                (getProgName)
import System.Exit                       (exitFailure, exitSuccess)

#if !MIN_VERSION_base(4,20,0)
import Data.List                         (foldl')
#endif

-- ghc
import GHC_Driver_Env                    (Hsc (..), HscEnv (..))
import GHC_Driver_Main                   (getHscEnv)
import GHC_Plugins                       (CommandLineOption)
import GHC_Runtime_Context               (InteractiveContext (..))
import GHC_Types_SourceError             (throwOneError)
import GHC_Types_SrcLoc                  (noLoc, noSrcSpan)
import GHC_Unit_Module                   (ModLocation (..))
import GHC_Unit_Module_ModSummary        (ModSummary (..), ms_mod_name)
import GHC_Utils_Outputable              (text)

#if MIN_VERSION_ghc(9,4,0)
import GHC.Plugins                       (ParsedResult)
#endif

#if MIN_VERSION_ghc(9,2,0)
import GHC.Hs                            (HsParsedModule (..))
#else
import GHC_Driver_Types                  (HsParsedModule (..))
#endif

-- Internal
import Language.Finkel.Error             (mkPlainWrappedMsg)
import Language.Finkel.Exception         (FinkelException (..),
                                          finkelExceptionLoc,
                                          handleFinkelException)
import Language.Finkel.Fnk               (FnkEnv (..), FnkInvokedMode (..),
                                          initFnkEnv, runFnk')
import Language.Finkel.Make              (mkParsedResult)
import Language.Finkel.Make.Summary      (TargetSummary (..), compileFnkFile,
                                          dumpParsedAST)
import Language.Finkel.Make.TargetSource (TargetSource (..),
                                          findTargetSourceWithPragma)
import Language.Finkel.Options           (FnkPluginOptions (..),
                                          defaultFnkPluginOptions,
                                          fnkPluginOptions, fpoIgnore,
                                          fpoPragma, printPluginUsage)


-- ------------------------------------------------------------------------
--
-- Exported
--
-- ------------------------------------------------------------------------

#if MIN_VERSION_ghc(9,4,0)
fnkParsedResultAction
  :: String -> FnkEnv -> [CommandLineOption] -> ModSummary -> ParsedResult
  -> Hsc ParsedResult
#else
fnkParsedResultAction
  :: String -> FnkEnv -> [CommandLineOption] -> ModSummary -> HsParsedModule
  -> Hsc HsParsedModule
#endif

fnkParsedResultAction mod_name fnk_env0 args0 ms pm = do
  fnk_env1  <- liftIO $ initFnkEnv fnk_env0
  case getOpt Permute fnkPluginOptions args1 of
    (_,    _, es@(_:_)) -> liftIO (exitWithBriefUsage mod_name es)
    (os, _ls,       []) -> do
      let fpo = foldl' (flip id) (defaultFnkPluginOptions fnk_env1) os
      if fpoHelp fpo
        then liftIO (printPluginUsage mod_name >> exitSuccess)
        else if fpoIgnore fpo
          then pure pm
          else case ml_hs_file (ms_location ms) of
            Nothing -> pure pm
            Just path -> do
              let pragma = fpoPragma fpo
                  lpath = noLoc path
                  fnk_env2 = fpoFnkEnv fpo
              ts <- findTargetSourceWithPragma pragma dflags lpath
              case ts of
                FnkSource {} -> mkPR <$> parseFnkModule fnk_env2 path ms
                _            -> pure pm
  where
    args1 = concatMap words args0
    dflags = ms_hspp_opts ms
    mkPR = mkParsedResult


-- ------------------------------------------------------------------------
--
-- Internal
--
-- ------------------------------------------------------------------------

parseFnkModule :: FnkEnv -> FilePath -> ModSummary -> Hsc HsParsedModule
parseFnkModule fenv0 path ms = do
  henv <- getHscEnv

  let mb_loc = fromMaybe noSrcSpan . finkelExceptionLoc
      mname = ms_mod_name ms
      dflags = hsc_dflags henv

      -- Setting the default DynFlags of FnkEnv to the DynFlags from interactive
      -- context, since the DynFlags from 'hsc_dflags' field of HscEnv is
      -- already updated with file local options at this point. This will
      -- prevent redundant recompilation when requireing home package modules.
      fenv1 = fenv0 { envInvokedMode = GhcPluginMode
                    , envDefaultDynFlags = Just (ic_dflags (hsc_IC henv)) }
      handler e = throwOneError (mkPlainWrappedMsg dflags (mb_loc e)
                                  (text (displayException e)))
      fnk = handleFinkelException handler $ compileFnkFile path mname

  summary <- liftIO $ runFnk' fnk fenv1 henv

  case summary of
    EMS ems _mb_sp _reqs | Just pm <- ms_parsed_mod ems -> do
      dumpParsedAST henv (ms_hspp_opts ems) ems
      pure pm
    _ -> liftIO (throwIO (FinkelException ("Failed to parse " ++ path)))

exitWithBriefUsage :: String -> [String] -> IO a
exitWithBriefUsage mod_name errs = do
  me <- getProgName
  let msgs =
        [ "Try:"
        , ""
        , "    " ++ me ++
          " -fplugin=" ++ mod_name ++
          " -fplugin-opt=" ++ mod_name ++ ":--help" ++
          " ..."
        , ""
        , "to see available options."
        ]
  putStr (unlines (errs ++ msgs))
  exitFailure

-- Note: [Workaround to support "-fno-code" option in ghc 9.6]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- When compiling with "-fno-code", home package module might required from
-- other home package modules. In 'GHC.Driver.Make.enableCodeGenWhen', byte
-- codes are generated when the TemplateHaskell language extension is turned
-- on. However, at the moment finkel does not understand TemplateHaskell, so
-- manually updating the 'HscEnv' with 'driverPlugin' plugin action. The three
-- updates done to the 'DynFlags' are same as passing "-fprefer-byte-code",
-- "-fwrite-if-simplified-core", and "-XTemplateHaskell" from command line.
--
-- XXX: haddock generation is not working with ghc 9.6 yet. Seems like the
-- GHC plugins are not initialized before module dependency analysis.

-- fnkNoCodePlugin :: [CommandLineOption] -> HscEnv -> IO HscEnv
-- fnkNoCodePlugin _ hsc_env = do
--   let dflags0 = hsc_dflags hsc_env
--       generates_code = backendGeneratesCode (backend dflags0)
--   if generates_code
--     then pure hsc_env
--     else do
--       let update df = setGeneralFlag' Opt_UseBytecodeRatherThanObjects $
--                       setGeneralFlag' Opt_WriteIfSimplifiedCore $
--                       xopt_set df TemplateHaskell
--           dflags1 = update dflags0
--       pure (hscSetFlags dflags1 hsc_env)

#else /* ghc < 8.6.0 */

fnkParsedResultAction :: a
fnkParsedResultAction = error "Unsupported GHC version, requires >= 8.6.0"

#endif /* ghc < 8.6.0 */

