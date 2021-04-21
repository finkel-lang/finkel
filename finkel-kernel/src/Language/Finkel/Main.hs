{-# LANGUAGE CPP #-}
-- | Main function for Finkel compiler.
--
-- This module contains 'main' function, which does similar and simplified works
-- done in @"ghc/Main.hs"@ found in ghc source.
--
module Language.Finkel.Main
  ( defaultMain
  , defaultMainWith
  ) where

#include "ghc_modules.h"

-- base
import           Control.Monad                (unless)
import           Control.Monad.IO.Class       (MonadIO (..))
import           Data.List                    (intercalate, partition)
import           Data.Version                 (showVersion)
import           System.Console.GetOpt        (ArgDescr (..), ArgOrder (..),
                                               OptDescr (..), getOpt, usageInfo)
import           System.Environment           (getArgs, getProgName)
import           System.Exit                  (exitFailure, exitWith)
import           System.FilePath              (normalise)
import           System.IO                    (BufferMode (..), hSetBuffering,
                                               stderr, stdout)
import           System.Process               (rawSystem)

-- ghc
import           GHC                          (defaultErrorHandler,
                                               parseDynamicFlags)
import           GHC_Driver_Monad             (printException)
import           GHC_Driver_Phases            (isDynLibFilename,
                                               isObjectFilename)
import           GHC_Driver_Session           (DynFlags (..), GeneralFlag (..),
                                               HasDynFlags (..), compilerInfo,
                                               defaultFatalMessager,
                                               defaultFlushOut, gopt)
import           GHC_Driver_Types             (handleFlagWarnings,
                                               handleSourceError)
import           GHC_Types_Basic              (SuccessFlag (..))
import           GHC_Types_SrcLoc             (mkGeneralLocated, unLoc)
import           GHC_Utils_CliOption          (Option (FileOption))
import           GHC_Utils_Misc               (looksLikeModuleName)
import           GHC_Utils_Panic              (GhcException (..),
                                               throwGhcException)

#if MIN_VERSION_ghc(8,10,1) && !MIN_VERSION_ghc(8,10,3)
import           GHC_Driver_Session           (HscTarget (..), gopt_set)
#endif

#if !MIN_VERSION_ghc(8,10,0)
import           GHC_Driver_Session           (targetPlatform)
#endif

-- ghc-boot
#if MIN_VERSION_ghc(8,6,0)
import           GHC.HandleEncoding           (configureHandleEncoding)
#endif

-- internal
import           Language.Finkel.Fnk
import           Language.Finkel.Make
import           Language.Finkel.Reader       (supportedLangExts)
import           Language.Finkel.SpecialForms (defaultFnkEnv)
import qualified Paths_finkel_kernel

#include "finkel_kernel_config.h"


-- ---------------------------------------------------------------------
--
-- The main function
--
-- ---------------------------------------------------------------------

-- [Main entry point]
-- ~~~~~~~~~~~~~~~~~~
--
-- Formerly, the Finkel compiler executable was written as ghc frontend
-- plugin. However, passing conflicting options used in ghc's "--make" to the
-- Finkel compiler executable was cumbersome, since frontend option cannot be
-- used when ghc is invoked in /make/ mode.
--
-- Functions exported from this module is doing almost the same work done in the
-- "Main" module of the ghc executable, but command line argument handling works
-- are simplified, since Finkel compiler delegates works done in non-make mode
-- to the ghc executable.

-- | Function used by the Finkel kernel compiler.
defaultMain :: IO ()
defaultMain = defaultMainWith []

-- | Make a main compiler function from given list of macros.
--
-- This functions does simplified command line argument parsing done in default
-- @ghc@ mode (i.e., @make@ mode).
defaultMainWith :: [(String, Macro)]
                -- ^ List of pairs of macro name and 'Macro' value loaded to
                -- macro expander.
                -> IO ()
defaultMainWith macros = do
  args0 <- getArgs
  if any (`elem` rawGhcOptions) args0
     then rawGhc args0
     else do
       (fnk_opts, args1) <-
         handleFinkelException
           (\(FinkelException msg) ->
              putStrLn msg >> printBriefUsage >> exitFailure)
           (parseFinkelOption defaultFnkEnv args0)

       let fnk_env0 = finkelEnv fnk_opts

           -- Filtering out `--make' flag when exist. Otherwise make flag would be
           -- treated as input file, and Finkel flags as unknown flags from this
           -- point.
           args3 = filter (/= "--make") args1

           -- Using the `macros' from argument as first argument to the
           -- 'mergeMacros' function, so that the caller of this function can
           -- have a chance to override the behaviour of special forms in
           -- 'defaultFnkEnv'.
           macros' = mergeMacros (makeEnvMacros macros)
                                 (envMacros defaultFnkEnv)

           fnk_env2 = fnk_env0 { envDefaultMacros = macros'
                               , envMacros = macros' }
           next = maybe (main1 fnk_env2 args1 args3)
                        printFinkelHelp
                        (finkelHelp fnk_opts)

       next

main1 :: FnkEnv -> [String] -> [String] -> IO ()
main1 fnk_env orig_args ghc_args = do
  initGCStatistics
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  configureHandleEncoding'
  defaultErrorHandler
    defaultFatalMessager
    defaultFlushOut
    (runFnk (handleFinkelException
              (\(FinkelException se) -> liftIO (do putStrLn se
                                                   exitFailure))
              (handleSourceError (\se -> do printException se
                                            liftIO exitFailure)
                                 (main2 orig_args ghc_args)))
             fnk_env)

main2 :: [String] -> [String] -> Fnk ()
main2 orig_arg ghc_args =
  if "--info" `elem` ghc_args
     -- Show info and exit. Using the 'DynFlags' from the finkel compiler
     -- executable, not the delegated "ghc" executable.
     then getDynFlags >>= liftIO . showInfo
     else main3 orig_arg ghc_args

main3 :: [String] -> [String] -> Fnk ()
main3 orig_args ghc_args = do
  dflags0 <- getDynFlags

  let largs = map on_the_cmdline ghc_args
      on_the_cmdline = mkGeneralLocated "on the commandline"
      dflags1 = dflags0 {verbosity = 1}
#if MIN_VERSION_ghc(8,10,3)
      dflags1b = dflags1
#elif MIN_VERSION_ghc(8,10,1)
      -- Workaround for "-fbyte-code" command line option handling in ghc
      -- 8.10.1 and 8.10.2.  The use of `noArgM' and `pure $ gopt_set ...' for
      -- "-fbyte-code" option in "compiler/main/DynFlags.hs" is ignoring the
      -- updated hscTarget ...
      dflags1b =
        if "-fbyte-code" `elem` ghc_args
           then gopt_set (dflags1 {hscTarget=HscInterpreted}) Opt_ByteCode
           else dflags1
#else
      dflags1b = dflags1
#endif

  -- From ghc 9.0, "interpretPackageEnv" is called from "parseDynamicFlags". In
  -- older versions, package environment initialization works were done by
  -- "setSessionDynFlags" via "initPackages".
  (dflags2, lfileish, warnings) <- parseDynamicFlags dflags1b largs

  let fileish = map unLoc lfileish
      platform = targetPlatform dflags2
      isObjeish x = isObjectFilename platform x || isDynLibFilename platform x

      -- Partition source-code-ish from object-ish in file-ish arguments.
      (objish, srcish) = partition isObjeish fileish

      -- Partition Finkel and Haskell source codes in args. Delegate to raw ghc
      -- when source codes were null. Don't bother with ldInput, delegate the
      -- linking work to raw ghc.
      (srcs, non_srcs) = partition isSourceTarget srcish

  case srcs of
     [] -> liftIO (rawGhc orig_args)
     _  -> do
       -- Update ld inputs with object file inputs, as done in Main.hs of ghc.
       let ld_inputs = map (FileOption "") objish ++ ldInputs dflags2
           dflags3 = dflags2 {ldInputs = ld_inputs}

       -- Using 'setDynFlags' instead of 'setSessionDynFlags', since
       -- 'setSessionDynFlags' will be called from 'initSessionForMake' below.
       setDynFlags dflags3

       -- Some IO works. Check unknown flags, and update uniq supply. See Note
       -- [Initialization of UniqSupply] in 'Language.Finkel.Fnk'.
       liftIO (do checkUnknownFlags fileish
                  initUniqSupply' (initialUnique dflags3)
                                  (uniqueIncrement dflags3))

       -- Show DynFlags warnings.
       handleSourceError
         (\e -> do printException e
                   liftIO exitFailure)
         (liftIO (handleFlagWarnings dflags3 warnings))

       -- Initialization works for Finkel.
       initSessionForMake

       -- At the moment, compiling with phase specification are not supported,
       -- phase is always set to 'Nothing'.
       let phased_srcs = map phase_it srcs
           phased_non_srcs = map phase_it non_srcs
           phased_inputs = phased_srcs ++ phased_non_srcs
           phase_it path = (on_the_cmdline (normalise path), Nothing)
           force_recomp = gopt Opt_ForceRecomp dflags3

       -- Do the `make' work.
       success_flag <- make phased_inputs force_recomp (outputFile dflags3)
       case success_flag of
         Succeeded -> return ()
         Failed    -> liftIO exitFailure


-- ---------------------------------------------------------------------
--
-- Finkel specific options
--
-- ---------------------------------------------------------------------

data FinkelOption = FinkelOption
  { finkelHelp :: Maybe FinkelHelp
  , finkelEnv  :: FnkEnv
  }

data FinkelHelp
  = Languages
  | Usage
  | Version

defaultFinkelOption :: FnkEnv -> FinkelOption
defaultFinkelOption fnk_env = FinkelOption
  { finkelHelp = Nothing
  , finkelEnv = fnk_env
  }

parseFinkelOption :: FnkEnv -> [String] -> IO (FinkelOption, [String])
parseFinkelOption fnk_env args0 = do
  let (fnk_args, other_args) = partitionFnkEnvOptions args0
  case getOpt Permute finkelOptDescrs fnk_args of
    (o,_,[]) -> pure (foldl (flip id) (defaultFinkelOption fnk_env) o,
                      other_args)
    (_,_,es) -> do
      me <- getProgName
      throwFinkelExceptionIO (FinkelException (me ++ ": " ++ concat es))

finkelOptDescrs :: [OptDescr (FinkelOption -> FinkelOption)]
finkelOptDescrs = helpOptDescrs ++ debugOptDescrs

helpOptDescrs :: [OptDescr (FinkelOption -> FinkelOption)]
helpOptDescrs =
  [ opt ["fnk-help"]
         (NoArg (\o -> o {finkelHelp = Just Usage}))
         "Show this help and exit."
  , opt ["fnk-languages"]
        (NoArg (\o -> o {finkelHelp = Just Languages}))
        "Show supported language extensions and exit."
  , opt ["fnk-version"]
        (NoArg (\o -> o {finkelHelp = Just Version}))
        "Show Finkel version and exit."
  ]
  where
    opt = Option []

debugOptDescrs :: [OptDescr (FinkelOption -> FinkelOption)]
debugOptDescrs = fromFnkEnvOptions (\f o -> o {finkelEnv = f (finkelEnv o)})

printFinkelHelp :: FinkelHelp -> IO ()
printFinkelHelp fh =
  case fh of
    Languages -> printLanguages
    Usage     -> printFinkelUsage
    Version   -> printFinkelVersion

printFinkelUsage :: IO ()
printFinkelUsage = do
  name <- getProgName
  putStrLn (unlines (message name))
  where
    message name =
      [ "USAGE: " ++ name ++ " [command-line-options-and-files]"
      , ""
      , usageInfo "HELP OPTIONS:\n" helpOptDescrs
      , fnkEnvOptionsUsage "DEBUG OPTIONS:\n"
      , "  Other options are passed to ghc." ]

printBriefUsage :: IO ()
printBriefUsage =
  putStrLn "Usage: For basic information, try the `--fnk-help' option."

printLanguages :: IO ()
printLanguages =
  mapM_ (putStrLn . snd) supportedLangExts

printFinkelVersion :: IO ()
printFinkelVersion = putStrLn v
  where
    v = "finkel kernel compiler, version " ++
        showVersion Paths_finkel_kernel.version


-- ---------------------------------------------------------------------
--
-- Auxiliary
--
-- ---------------------------------------------------------------------

rawGhc :: [String] -> IO ()
rawGhc args = rawSystem ghc args >>= exitWith
  where
    -- CPP macro defined in "finkel_kernel_config.h", see "Setup.hs" for detail.
    ghc = FINKEL_KERNEL_GHC

-- | When any of options listed here were found, invoke raw @ghc@ without using
-- Finkel compiler. Otherwise @ghc@ will complain with error message. These
-- options are listed in "ghc/Main.hs" as `mode_flags'.
rawGhcOptions :: [String]
rawGhcOptions =
  [ "-?"
  , "--help"
  , "-V"
  , "--version"
  , "--numeric-version"
  , "--show-options"
  , "--supported-languages"
  , "--supported-extensions"
  , "--show-packages"
  , "--show-iface"
  , "--backpack"
  , "--interactive"
  , "--abi-hash"
  , "-e"
  , "--frontend"
  ]

-- | THrow 'UsageError' when unknown flag were found.
checkUnknownFlags :: [String] -> IO ()
checkUnknownFlags fileish = do
  let unknowns = [f | f@('-':_) <- fileish]
      oneErr f = "unrecognised flag: " ++ f ++ "\n"
  unless (null unknowns)
         (throwGhcException (UsageError (concatMap oneErr unknowns)))

-- | True if given 'String' was module name, Finkel source file, or
-- Haskell source file.
isSourceTarget :: String -> Bool
isSourceTarget str = looksLikeModuleName str || isFnkFile str || isHsFile str

-- | Show the information of given 'DynFlags', doing the same thing as done in
-- the @Main.hs@ found in ghc-bin.
showInfo :: DynFlags -> IO ()
showInfo dflags = do
  let sq x = " [" ++ x ++ "\n ]"
  putStrLn (sq (intercalate "\n ," (map show (compilerInfo dflags))))

-- | Until ghc-8.6.0, 'configureHandleEncoding' did not exist.
configureHandleEncoding' :: IO ()
#if MIN_VERSION_ghc(8,6,0)
configureHandleEncoding' = configureHandleEncoding
#else
configureHandleEncoding' = return ()
#endif

foreign import ccall safe "initGCStatistics"
  initGCStatistics :: IO ()
