{-# LANGUAGE CPP #-}
-- Module header preprocessor

module Language.Finkel.Preprocess
  (
    -- Preprocessor functions
    defaultPreprocess
  , defaultPreprocessEnv
  , defaultPreprocessWith

    -- Auxiliary
  , preprocessOrCopy
  , PpOptions(..)
  , ppOptions
  , mkPpOptions
  ) where

#include "ghc_modules.h"

-- base
import Control.Exception                 (Exception (..), throw)
import Control.Monad                     (when)
import Control.Monad.IO.Class            (MonadIO (..))
import Data.Char                         (toLower)
import Data.Maybe                        (fromMaybe)
import System.Console.GetOpt             (ArgDescr (..), ArgOrder (..),
                                          OptDescr (..), getOpt, usageInfo)
import System.Environment                (getArgs, getProgName)
import System.Exit                       (exitFailure)
import System.IO                         (IOMode (..), hPutStrLn, stderr,
                                          stdout, withFile)

#if !MIN_VERSION_base(4,20,0)
import Data.List                         (foldl')
#endif

#if MIN_VERSION_base(4,11,0)
import Prelude                           hiding ((<>))
#endif

-- directory
import System.Directory                  (copyFile)

-- ghc
import GHC_Data_Bag                      (unitBag)
import GHC_Data_FastString               (fsLit)
import GHC_Data_StringBuffer             (hGetStringBuffer)
import GHC_Driver_Env                    (HscEnv (..))
import GHC_Types_SrcLoc                  (GenLocated (..))
import GHC_Utils_Outputable              (text, ($$), (<>))

#if MIN_VERSION_ghc(9,4,0)
import GHC.Driver.Config.Diagnostic      (initDiagOpts)
import GHC.Driver.Errors.Types           (ghcUnknownMessage)
import GHC.Types.Error                   (DiagnosticReason (..), mkMessages,
                                          mkPlainDiagnostic, noHints)
import GHC.Utils.Error                   (mkPlainMsgEnvelope)
import GHC_Driver_Monad                  (logDiagnostics)
#else
import GHC                               (getPrintUnqual)
import GHC_Driver_Flags                  (WarnReason (..))
import GHC_Driver_Monad                  (logWarnings)
import GHC_Utils_Error                   (makeIntoWarning, mkWarnMsg)
#endif

#if !MIN_VERSION_ghc(9,2,0) || MIN_VERSION_ghc(9,4,0)
import GHC_Driver_Session                (HasDynFlags (..))
#endif

-- finkel-kernel
import Language.Finkel.Emit              (Hsrc (..), putHsSrc)
import Language.Finkel.Exception         (FinkelException (..),
                                          handleFinkelException,
                                          printFinkelException,
                                          readOrFinkelException)
import Language.Finkel.Fnk               (FnkEnv (..), Macro (..), addMacro,
                                          lookupMacro, macroFunction,
                                          makeEnvMacros, mergeMacros,
                                          modifyFnkEnv, runFnk, runFnk')
import Language.Finkel.Form              (Form (..), LForm (..), aSymbol,
                                          unCode)
import Language.Finkel.Make.Cache
import Language.Finkel.Make.Session      (expandContents)
import Language.Finkel.Make.Summary      (buildHsSyn, withTiming')
import Language.Finkel.Make.TargetSource (findPragmaString)
import Language.Finkel.SpecialForms      (defaultFnkEnv, emptyForm,
                                          specialForms)
import Language.Finkel.Syntax            (parseHeader, parseModule)

import Language.Finkel.Options           (FnkSrcOptions (..),
                                          defaultFnkSrcOptions,
                                          fromFnkSrcOptions)


-- ------------------------------------------------------------------------
--
-- Exported
--
-- ------------------------------------------------------------------------

-- | Default main function for preprocessor.
defaultPreprocess :: IO ()
defaultPreprocess = getArgs >>= defaultPreprocessWith defaultPreprocessEnv

-- | 'FnkEnv' used in 'defaultPreprocess'.
defaultPreprocessEnv :: FnkEnv
defaultPreprocessEnv = defaultFnkEnv {envMacros=myMacros}
  where
    -- Adding ":require", ":with-macro", and ":eval-when-compile" special forms
    -- to empty macros with dummy contents, because the preprocessor does not
    -- know the module lookup paths from the command line argument.
    myMacros = foldr f z interpMacros
    f name = addMacro (fsLit name) emptyFormMacro
    z = addMacro (fsLit "defmodule") defmoduleForDownsweep specialForms

interpMacros :: [String]
interpMacros =
  [ ":eval-when-compile"
  , ":require"
  , ":with-macro"
  ]

-- | Default main function for preprocessor, with given 'FnkEnv' and command
-- line arguments.
defaultPreprocessWith
  :: FnkEnv -- ^ Environment for running 'Fnk'.
  -> [String] -- ^ Command line arguments.
  -> IO ()
defaultPreprocessWith fnk_env args =
  case getOpt Permute ppOptions args of
    (   _,     _, errs@(_:_)) -> exitWithErrors errs
    (opts, files, _         ) -> handleFinkelException handler $ do
      me <- getProgName
      let ppo = foldl' (flip id) myPpOptions opts
          myPpOptions = mkPpOptions me fnk_env
          go = preprocessOrCopy Nothing ppo
      if ppoHelp ppo
        then printUsage
        else do
          debug ppo 2 ("args: " ++ show args)
          case files of
            [isrc]           -> go isrc Nothing
            [isrc, opath]    -> go isrc (Just opath)
            [_, isrc, opath] -> go isrc (Just opath)
            _                -> exitWithErrors []
  where
    handler e = do
      me <- getProgName
      hPutStrLn stderr (me ++ ": " ++ displayException e)
      exitFailure
    exitWithErrors es = do
      mapM_ putStrLn es
      printUsage
      exitFailure

-- | Preprocess Finkel source code file with given 'FnkEnv', or copy
-- the file if the file was a Haskell source code.
preprocessOrCopy
  :: Maybe HscEnv
  -- ^ Environment used for expanding macros.
  -> PpOptions
  -- ^ Pre-processor options.
  -> FilePath
  -- ^ Path of input Finkel source code.
  -> Maybe FilePath
  -- ^ 'Just' path to write preprocessed output, or 'Nothing' for 'stdout'.
  -> IO ()
preprocessOrCopy mb_hsc_env ppo isrc mb_opath = do
  buf <- hGetStringBuffer isrc
  if not (ppoIgnore ppo) && findPragmaString (ppoPragma ppo) buf
    then do
      let opath = fromMaybe "stdout" mb_opath
      debug ppo 2 ("Preprocessing " ++ isrc ++ " to " ++ opath)
      writeModule mb_hsc_env ppo isrc mb_opath
      debug ppo 2 ("Finished wriitng " ++ isrc ++ " to " ++ opath)
    else do
      debug ppo 2 ("Skipping " ++ isrc)
      mapM_ (copyFile isrc) mb_opath


-- ------------------------------------------------------------------------
--
-- Internal
--
-- ------------------------------------------------------------------------

printUsage :: IO ()
printUsage = do
  me <- getProgName
  let header = unlines
        [ me ++ ": Finkel source code preprocessor"
        , ""
        , "USAGE:"
        , ""
        , "    " ++ me ++ " [OPTIONS] INPATH"
        , "    " ++ me ++ " [OPTIONS] INPATH OUTPATH"
        , "    " ++ me ++ " [OPTIONS] ORIGPATH INPATH OUTPATH"
        , ""
        , "OPTIONS:" ]
  putStrLn (usageInfo header ppOptions)

writeModule
  :: Maybe HscEnv -> PpOptions -> FilePath -> Maybe FilePath -> IO ()
writeModule mb_hsc_env ppo ipath mb_opath =
  case mb_opath of
    Nothing    -> run stdout
    Just opath -> withFile opath WriteMode run
  where
    run hdl =
      case mb_hsc_env of
        -- When hsc_env is given, assuming that given FnkEnv is already
        -- initialized.
        Just hsc_env -> runFnk' (go hdl) fnk_env hsc_env
        Nothing      -> runFnk (go hdl) fnk_env
    fnk_env = (ppoFnkEnv ppo) {envVerbosity=ppoVerbosity ppo}
    parser = if ppoFull ppo
                then parseModule
                else parseHeader
    warn_interp_macros = 0 < ppoVerbosity ppo && ppoWarnInterp ppo
    go hdl = withTiming' "writeModule" $ handleFinkelException handler $ do
      when warn_interp_macros $
        modifyFnkEnv (replaceWithWarnings interpMacros)
      ExpandedCode {ec_sp=sp,ec_forms=forms1} <- expandContents ipath
      mdl <- buildHsSyn parser forms1
      putHsSrc hdl sp (Hsrc mdl)
    handler e = do
      printFinkelException e
      liftIO exitFailure

debug :: MonadIO m => PpOptions -> Int -> String -> m ()
debug ppo level msg =
  when (level < ppoVerbosity ppo) $
    liftIO (hPutStrLn stderr (ppoExecName ppo ++ ": " ++ msg))


-- ------------------------------------------------------------------------
--
-- Macros
--
-- ------------------------------------------------------------------------

-- Macro constantly returning empty form.
emptyFormMacro :: Macro
emptyFormMacro = Macro (const (pure emptyForm))

-- Variant of defmodule macro to support 'downsweep' function in ghc's make
-- function. Actual implementation of 'defmodule' macro is written in
-- finkel-core package.
defmoduleForDownsweep :: Macro
defmoduleForDownsweep = Macro (pure . f)
  where
    f (LForm (L l0 lst)) = case lst of
      List (_:name:rest) -> go name rest
      _                  -> emptyForm
      where
        go name rest =
          if null rest
            then moduleForm name
            else begin (moduleForm name : foldr accImportForm [] rest)
        begin xs = mkL0 (List (sym ":begin" : xs))
        accImportForm x acc =
          case unCode x of
            List (y : ys) | y == sym "import" -> foldr accModule acc ys
                          | y == sym "import_when"
                          , ps:ys' <- ys
                          , hasLoadPhase ps   -> foldr accModule acc ys'
            _                                 -> acc
        accModule x acc =
          case x of
            LForm (L l1 (List ys)) -> importForm l1 ys : acc
            _                      -> acc
        hasLoadPhase xs =
          case unCode (curve xs) of
            List ys -> sym ":load" `elem` ys
            _       -> False
        moduleForm n = mkL0 (List [sym "module", n])
        importForm l xs = LForm (L l (List (sym "import" : map curve xs)))
        curve x = case x of
          LForm (L l (HsList ys)) -> LForm (L l (List ys))
          _                       -> x
        sym = mkL0 . Atom . aSymbol
        mkL0 = LForm . L l0

-- Replace given macro names with original macro with warning message.
replaceWithWarnings :: [String] -> FnkEnv -> FnkEnv
replaceWithWarnings names fnk_env = fnk_env {envMacros=added}
  where
    added = mergeMacros replaced orig_macros
    orig_macros = envMacros fnk_env
    replaced = makeEnvMacros (foldr f [] names)
    f name acc = case lookupMacro (fsLit name) fnk_env of
      Just macro -> (name, Macro (addWarning name macro)) : acc
      Nothing    -> acc
    addWarning name macro form@(LForm (L loc _)) = do
      let msg =
            text "Preprocessor does not interpret during macro expansion." $$
            text "Replacing '(" <> text name <> text " ...)' with '(:begin)'." $$
            text "Use \"--warn-interp=False\" to suppress this message."

      -- XXX: See "GHC.SysTools.Process.{builderMainLoop,readerProc}".
      --
      -- Until ghc 9.4, the messages from the Finkel preprocessor command are
      -- parsed, and then hard coded "SevError" message is printed by the
      -- logger. Although the below "logWarnings" is using "SevWarning", the
      -- parsed message is shown with "SevError".

#if MIN_VERSION_ghc(9,4,0)
      dflags <- getDynFlags
      let diag = mkPlainDiagnostic WarningWithoutFlag noHints msg
          warning = mkPlainMsgEnvelope (initDiagOpts dflags) loc diag
      logDiagnostics (mkMessages (unitBag (fmap ghcUnknownMessage warning)))
#elif MIN_VERSION_ghc(9,2,0)
      unqual <- getPrintUnqual
      let wmsg = mkWarnMsg loc unqual msg
          warning = makeIntoWarning NoReason wmsg
      logWarnings (unitBag warning)
#else
      dflags <- getDynFlags
      unqual <- getPrintUnqual
      let wmsg = mkWarnMsg dflags loc unqual msg
          warning = makeIntoWarning NoReason wmsg
      logWarnings (unitBag warning)
#endif

      -- Deleget to the original function
      macroFunction macro form

-- ------------------------------------------------------------------------
--
-- Options for preprocessor
--
-- ------------------------------------------------------------------------

-- | Preprocessor options
data PpOptions = PpOptions
  { ppoHelp          :: Bool
    -- ^ Flag for showing help message.
  , ppoVerbosity     :: !Int
    -- ^ Verbosity level.
  , ppoFull          :: !Bool
    -- ^ Preprocess full module if 'True', otherwise parse module header only.
  , ppoWarnInterp    :: !Bool
    -- ^ Flag for showing warning message for macros using interpreter.
  , ppoFnkSrcOptions :: !FnkSrcOptions
    -- ^ Finkel source code option for preprocessor.
  , ppoFnkEnv        :: FnkEnv
    -- ^ The 'FnkEnv' to run 'Fnk'.
  , ppoExecName      :: String
    -- ^ Executable name shown in debug message.
  }

-- | Make 'PpOption' with some fields set to default value.
mkPpOptions :: String -> FnkEnv -> PpOptions
mkPpOptions exec_name fnk_env = PpOptions
  { ppoHelp = False
  , ppoVerbosity = 1
  , ppoFull = False
  , ppoWarnInterp = True
  , ppoFnkSrcOptions = defaultFnkSrcOptions
  , ppoFnkEnv = fnk_env
  , ppoExecName = exec_name
  }

ppoPragma :: PpOptions -> String
ppoPragma = fsrcPragma . ppoFnkSrcOptions

ppoIgnore :: PpOptions -> Bool
ppoIgnore = fsrcIgnore . ppoFnkSrcOptions

ppOptions :: [OptDescr (PpOptions -> PpOptions)]
ppOptions =
  [ Option [] ["help"]
    (NoArg (\o -> o {ppoHelp = True}))
    "Show this help and exit."
  , Option [] ["verbose"]
    (ReqArg (\n o -> o {ppoVerbosity=readInt n}) "INT")
    "Set verbosity level to INT."
  , Option [] ["warn-interp"]
    (OptArg (\mb o -> o {ppoWarnInterp=maybe True parseBoolish mb}) "BOOL")
    ("Show warning in macros using interpreter.\n" ++
     "(default: True)")
  , Option [] ["no-warn-interp"]
    (NoArg (\o -> o {ppoWarnInterp=False}))
    "Do not show warning in macros using interpreter."
  , Option [] ["full"]
    (NoArg (\o -> o {ppoFull = True}))
    "Parse full module instead of module header."
  ] ++ fnk_src_opts
  where
    fnk_src_opts = fromFnkSrcOptions wrap
    wrap f o = o {ppoFnkSrcOptions = f (ppoFnkSrcOptions o)}
    readInt = readOrFinkelException "INT" "verbosity"

parseBoolish :: String -> Bool
parseBoolish str
  | low_str `elem` trueish = True
  | low_str `elem` falsish = False
  | otherwise = throw (FinkelException msg)
  where
     low_str = map toLower str
     trueish = ["true", "yes", "1"]
     falsish = ["false", "no", "0"]
     msg = "Expecting boolean value but got \"" ++ str ++ "\""
