{-# LANGUAGE CPP #-}
-- | Plugin version of the finkel compiler.
module Language.Finkel.Plugin
  ( -- * Finkel plugin
    plugin
  , pluginWith
  )
where

#include "ghc_modules.h"

-- Requires parsedResultaction field in the 'Plugin' data type, which is
-- supported from ghc 8.6.0.
#if MIN_VERSION_ghc(8,6,0)

-- base
import Control.Exception                 (displayException, throwIO)
import Control.Monad.IO.Class            (MonadIO (..))
import Data.IORef                        (newIORef)
import Data.List                         (foldl')
import Data.Maybe                        (fromMaybe)
import System.Console.GetOpt             (ArgDescr (..), ArgOrder (..),
                                          OptDescr (..), getOpt, usageInfo)
import System.Environment                (getProgName)
import System.Exit                       (exitFailure, exitSuccess)

-- ghc
import GHC_Driver_Env                    (Hsc (..), HscEnv (..))
import GHC_Driver_Main                   (getHscEnv)
import GHC_Driver_Monad                  (Ghc (..), Session (..))
import GHC_Plugins                       (CommandLineOption, Plugin (..),
                                          defaultPlugin, flagRecompile)
import GHC_Runtime_Context               (InteractiveContext (..))
import GHC_Types_SourceError             (throwOneError)
import GHC_Types_SrcLoc                  (noLoc, noSrcSpan)
import GHC_Unit_Module                   (ModLocation (..))
import GHC_Unit_Module_ModSummary        (ModSummary (..), ms_mod_name)
import GHC_Utils_Outputable              (text)

#if MIN_VERSION_ghc(9,6,0)
import GHC.Driver.Backend                (backendGeneratesCode)
import GHC.Driver.Env                    (hscSetFlags)
import GHC.Driver.Session                (GeneralFlag (..), backend,
                                          setGeneralFlag', xopt_set)
import GHC.LanguageExtensions            (Extension (..))
#endif

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
import Language.Finkel.Fnk               (FnkEnv (..), FnkEnvRef (..),
                                          FnkInvokedMode (..),
                                          fromFnkEnvOptions, initFnkEnv, toGhc)
import Language.Finkel.Make              (mkParsedResult)
import Language.Finkel.Make.Summary      (TargetSummary (..), compileFnkFile,
                                          dumpParsedAST)
import Language.Finkel.Make.TargetSource (TargetSource (..),
                                          findTargetSourceWithPragma)
import Language.Finkel.Preprocess        (FnkSrcOptions (..),
                                          defaultFnkSrcOptions,
                                          fromFnkSrcOptions)
import Language.Finkel.SpecialForms      (defaultFnkEnv)


-- ------------------------------------------------------------------------
--
-- Exported
--
-- ------------------------------------------------------------------------

-- | Finkel compiler plugin.
plugin :: Plugin
plugin = pluginWith "Language.Finkel.Plugin" defaultFnkEnv

-- | Finkel compiler plugin with given 'FnkEnv'.
pluginWith
  :: String -- ^ Plugin module name.
  -> FnkEnv -- ^ The environment used by the plugin.
  -> Plugin
pluginWith mod_name fnk_env =
  defaultPlugin
    { parsedResultAction = fnkParsedResultAction mod_name fnk_env
#if MIN_VERSION_ghc(9,6,0)
    , driverPlugin = fnkNoCodePlugin
#endif
    , pluginRecompile = flagRecompile
    }


-- ------------------------------------------------------------------------
--
-- Internal
--
-- ------------------------------------------------------------------------

data FnkPluginOptions = FnkPluginOptions
  { fpoHelp       :: Bool
  , fpoSrcOptions :: FnkSrcOptions
  , fpoFnkEnv     :: FnkEnv
  }

defaultFnkPluginOptions :: FnkEnv -> FnkPluginOptions
defaultFnkPluginOptions fnk_env = FnkPluginOptions
  { fpoHelp = False
  , fpoSrcOptions = defaultFnkSrcOptions
  , fpoFnkEnv = fnk_env
  }

fpoPragma :: FnkPluginOptions -> String
fpoPragma = fsrcPragma . fpoSrcOptions

fpoIgnore :: FnkPluginOptions -> Bool
fpoIgnore = fsrcIgnore . fpoSrcOptions

fnkPluginOptions :: [OptDescr (FnkPluginOptions -> FnkPluginOptions)]
fnkPluginOptions = help : sopts ++ eopts
 where
   help = Option [] ["help"]
          (NoArg (\o -> o {fpoHelp = True}))
          "Show this help and exit."
   eopts = adjustFnkEnvOptions (fromFnkEnvOptions wenv)
   wenv f o = o {fpoFnkEnv = f (fpoFnkEnv o)}
   sopts = fromFnkSrcOptions wsrc
   wsrc f o = o {fpoSrcOptions = f (fpoSrcOptions o)}

adjustFnkEnvOptions :: [OptDescr a] -> [OptDescr a]
adjustFnkEnvOptions = foldr f []
  where
    f opt@(Option so lo ad descr) acc =
      if is_removed_option opt
        then acc
        else Option so (map dropFnk lo) ad descr : acc
    dropFnk = drop (length "fnk-")
    is_removed_option (Option so lo _ _) =
      so == ['B'] ||
      any (`elem` lo) ["fnk-dump-hs", "fnk-dump-dflags", "fnk-hsdir"]

#if MIN_VERSION_ghc(9,4,0)
fnkParsedResultAction
  :: String -> FnkEnv -> [CommandLineOption] -> ModSummary -> ParsedResult
  -> Hsc ParsedResult
#else
fnkParsedResultAction
  :: String -> FnkEnv -> [CommandLineOption] -> ModSummary -> HsParsedModule
  -> Hsc HsParsedModule
#endif

fnkParsedResultAction mod_name fnk_env args0 ms pm =
  case getOpt Permute fnkPluginOptions args1 of
    (_,    _, es@(_:_)) -> liftIO (exitWithBriefUsage mod_name es)
    (os, _ls,       []) -> do
      let fpo = foldl' (flip id) (defaultFnkPluginOptions fnk_env) os
      if fpoHelp fpo
        then liftIO (printUsage mod_name >> exitSuccess)
        else if fpoIgnore fpo
          then pure pm
          else case ml_hs_file (ms_location ms) of
            Nothing -> pure pm
            Just path -> do
              let pragma = fpoPragma fpo
                  lpath = noLoc path
                  fnk_env' = fpoFnkEnv fpo
              ts <- findTargetSourceWithPragma pragma dflags lpath
              case ts of
                FnkSource {} -> mkPR <$> parseFnkModule fnk_env' path ms
                _            -> pure pm
  where
    args1 = concatMap words args0
    dflags = ms_hspp_opts ms
    mkPR = mkParsedResult

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

  summary <- liftIO $ do
    fer <- initFnkEnv fenv1 >>= newIORef
    session <- Session <$> newIORef henv
    unGhc (toGhc fnk (FnkEnvRef fer)) session

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

printUsage :: String -> IO ()
printUsage mod_name = do
  prog <- getProgName
  let fplugin_opt = "-fplugin-opt=" ++ mod_name ++ ":OPTION"
      header = unlines
        [ "USAGE:"
        , ""
        , "    " ++ prog ++ " ... [" ++ fplugin_opt ++ "]"
        , ""
        , "OPTIONS:"
        ]
  putStrLn (usageInfo header fnkPluginOptions)

#if MIN_VERSION_ghc(9,6,0)

-- Note: [Workaround to support "-fno-code" option in ghc 9.6]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- When compiling with "-fno-code", home package module might rquired from other
-- home package modules. In 'GHC.Driver.Make.enableCodeGenWhen', byte codes are
-- generated when the TemplateHaskell language extension is turned on. However,
-- at the moment finkel does not understand TemplateHaskell, so manually
-- updating the 'HscEnv' with 'driverPlugin' plugin action. The three updates
-- done to the 'DynFlags' are same as passing "-fprefer-byte-code",
-- "-fwrite-if-simplified-core", and "-XTemplateHaskell" from command line.
--
-- XXX: haddock generation is not working with ghc 9.6 yet. Seems like the
-- modified HscEnv is not passed to the static plugin used by haddock.

fnkNoCodePlugin :: [CommandLineOption] -> HscEnv -> IO HscEnv
fnkNoCodePlugin _ hsc_env = do
  let dflags0 = hsc_dflags hsc_env
      generates_code = backendGeneratesCode (backend dflags0)
  if generates_code
    then pure hsc_env
    else do
      let update df = setGeneralFlag' Opt_UseBytecodeRatherThanObjects $
                      setGeneralFlag' Opt_WriteIfSimplifiedCore $
                      xopt_set df TemplateHaskell
          dflags1 = update dflags0
      pure (hscSetFlags dflags1 hsc_env)
#endif

#else /* ghc < 8.6.0 */

plugin :: a
plugin = unsupportedGhcVersion

pluginWith :: a
pluginWith = unsupportedGhcVersion

unsupportedGhcVersion :: a
unsupportedGhcVersion = error "Unsupported GHC version, requires >= 8.6.0"

#endif /* ghc < 8.6.0 */
