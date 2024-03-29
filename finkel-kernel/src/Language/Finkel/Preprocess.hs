{-# LANGUAGE CPP #-}
-- Module header preprocessor

module Language.Finkel.Preprocess
  ( -- Options
    FnkSrcOptions(..)
  , defaultFnkSrcOptions
  , fromFnkSrcOptions

    -- Preprocessor functions
  , defaultPreprocess
  , defaultPreprocessEnv
  , defaultPreprocessWith

    -- Auxiliary
  , preprocessOrCopy
  ) where

#include "ghc_modules.h"

-- base
import Control.Exception                 (Exception (..), throw)
import Control.Monad                     (when)
import Control.Monad.IO.Class            (MonadIO (..))
import Data.Char                         (toLower)
import Data.List                         (foldl')
import Data.Maybe                        (fromMaybe)
import System.Console.GetOpt             (ArgDescr (..), ArgOrder (..),
                                          OptDescr (..), getOpt, usageInfo)
import System.Environment                (getArgs, getProgName)
import System.Exit                       (exitFailure)

#if MIN_VERSION_base(4,11,0)
import Prelude                           hiding ((<>))
#endif

-- directory
import System.Directory                  (copyFile)
import System.IO                         (IOMode (..), hPutStrLn, stderr,
                                          stdout, withFile)

-- ghc
import GHC_Data_Bag                      (unitBag)
import GHC_Data_FastString               (fsLit)
import GHC_Data_StringBuffer             (StringBuffer, hGetStringBuffer)
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
                                          readOrFinkelException)
import Language.Finkel.Expand            (expands)
import Language.Finkel.Fnk               (FnkEnv (..), Macro (..), addMacro,
                                          lookupMacro, macroFunction,
                                          makeEnvMacros, mergeMacros,
                                          modifyFnkEnv, printFinkelException,
                                          runFnk)
import Language.Finkel.Form              (Form (..), LForm (..), aSymbol,
                                          unCode)
import Language.Finkel.Make.Summary      (buildHsSyn)
import Language.Finkel.Make.TargetSource (findPragmaString)
import Language.Finkel.Reader            (parseSexprs)
import Language.Finkel.SpecialForms      (defaultFnkEnv, emptyForm,
                                          specialForms)
import Language.Finkel.Syntax            (parseHeader, parseModule)


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
          go = preprocessOrCopy ppo
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
  :: PpOptions
  -- ^ Pre-processor options.
  -> FilePath
  -- ^ Path of input Finkel source code.
  -> Maybe FilePath
  -- ^ 'Just' path to write preprocessed output, or 'Nothing' for 'stdout'.
  -> IO ()
preprocessOrCopy ppo isrc mb_opath = do
  buf <- hGetStringBuffer isrc
  if not (ppoIgnore ppo) && findPragmaString (ppoPragma ppo) buf
    then do
      let opath = fromMaybe "stdout" mb_opath
      debug ppo 2 ("Preprocessing " ++ isrc ++ " to " ++ opath)
      writeModule ppo buf isrc mb_opath
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
  :: PpOptions -> StringBuffer -> FilePath -> Maybe FilePath -> IO ()
writeModule ppo buf0 ipath mb_opath =
  case mb_opath of
    Just opath -> withFile opath WriteMode run
    Nothing    -> run stdout
  where
    run hdl = runFnk (go hdl) fnk_env
    fnk_env = (ppoFnkEnv ppo) {envVerbosity=ppoVerbosity ppo}
    parser = if ppoFull ppo
                then parseModule
                else parseHeader
    warn_interp_macros = 0 < ppoVerbosity ppo && ppoWarnInterp ppo
    go hdl = handleFinkelException handler $ do
      when warn_interp_macros $
        modifyFnkEnv (replaceWithWarnings interpMacros)
      (forms0, sp) <- parseSexprs (Just ipath) buf0
      forms1 <- expands forms0
      mdl <- buildHsSyn parser forms1
      putHsSrc hdl sp (Hsrc mdl)
    handler e = do
      printFinkelException e
      liftIO exitFailure

debug :: MonadIO m => PpOptions -> Int -> String -> m ()
debug ppo level msg =
  when (level < ppoVerbosity ppo) $
    liftIO (putStrLn (ppoExecName ppo ++ ": " ++ msg))


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
            List ys -> elem (sym ":load") ys
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
-- Options for finkel source code
--
-- ------------------------------------------------------------------------

data FnkSrcOptions = FnkSrcOptions
  { fsrcPragma :: !String
    -- ^ String to be searched at the beginning section of a file to detect
    -- Finkel source code.
  , fsrcIgnore :: !Bool
    -- ^ Flag for ignoring the given file.
  }

defaultFnkSrcOptions :: FnkSrcOptions
defaultFnkSrcOptions = FnkSrcOptions
  { fsrcPragma = ";;;"
  , fsrcIgnore = False
  }

fnkSrcOptions :: [OptDescr (FnkSrcOptions -> FnkSrcOptions)]
fnkSrcOptions =
  [ Option [] ["pragma"]
    (ReqArg (\i o -> o {fsrcPragma = i}) "STR")
    (unlines [ "Searched string to detect Finkel source file."
             , "(default: " ++ fsrcPragma defaultFnkSrcOptions ++ ")" ])
  , Option [] ["ignore"]
    (NoArg (\o -> o {fsrcIgnore = True}))
    "Ignore this file."
  ]

fromFnkSrcOptions :: ((FnkSrcOptions -> FnkSrcOptions) -> a) -> [OptDescr a]
fromFnkSrcOptions f = map (fmap f) fnkSrcOptions


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
  | elem low_str trueish = True
  | elem low_str falsish = False
  | otherwise = throw (FinkelException msg)
  where
     low_str = map toLower str
     trueish = ["true", "yes", "1"]
     falsish = ["false", "no", "0"]
     msg = "Expecting boolean value but got \"" ++ str ++ "\""
