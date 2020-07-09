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

-- base
import           Control.Monad                (unless)
import           Control.Monad.IO.Class       (MonadIO (..))
import           Data.List                    (isPrefixOf, partition)
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
import           DriverPhases                 (isDynLibFilename,
                                               isObjectFilename)
import           DynFlags                     (DynFlags (..), GeneralFlag (..),
                                               HasDynFlags (..),
                                               defaultFatalMessager,
                                               defaultFlushOut, gopt,
                                               parseDynamicFlagsCmdLine)
import           GHC                          (defaultErrorHandler)
import           GhcMonad                     (printException)
import           HscTypes                     (handleFlagWarnings,
                                               handleSourceError)
import           Panic                        (GhcException (..),
                                               throwGhcException)
import           SrcLoc                       (mkGeneralLocated, unLoc)
import           Util                         (looksLikeModuleName)

#if MIN_VERSION_ghc(8,10,0)
import           CliOption                    (Option (FileOption))
import           DynFlags                     (HscTarget (..), gopt_set)
#else
import           DynFlags                     (Option (FileOption),
                                               targetPlatform)
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
import           Language.Finkel.TargetSource
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
       -- Filter out Finkel flags and `--make' flag when exist, otherwise make
       -- flag would be treated as input file, and Finkel flags as unknown flags
       -- from this point.
       let (finkelopts, args1) = partitionFinkelOptions args0
           args2 = filter (/= "--make") args1

       fnk_opts <- handleFinkelException
                     (\(FinkelException msg) ->
                         putStrLn msg >> printBriefUsage >> exitFailure)
                     (parseFinkelOption finkelopts)

       let fnk_env0 = opt2env fnk_opts

           -- Using macros from argument as first argument to 'mergeMacros', so
           -- that the caller of this function can have a chance to override the
           -- behaviour of specialforms in 'defaultFnkEnv'.
           macros' = mergeMacros (makeEnvMacros macros)
                                 (envMacros defaultFnkEnv)

           fnk_env1 = fnk_env0 { envDefaultMacros = macros'
                               , envMacros = macros' }
           next = maybe (main' fnk_env1 args1 args2)
                        printFinkelHelp
                        (finkelHelp fnk_opts)

       -- XXX: Handle '-B' option properly.
       next

main' :: FnkEnv -> [String] -> [String] -> IO ()
main' fnk_env orig_args ghc_args = do
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
                                 (main'' orig_args ghc_args)))
             fnk_env)

main'' :: [String] -> [String] -> Fnk ()
main'' orig_args ghc_args = do
  dflags0 <- getDynFlags
  let largs = map on_the_cmdline ghc_args
      on_the_cmdline = mkGeneralLocated "on the commandline"
      dflags1 = dflags0 {verbosity = 1}
#if MIN_VERSION_ghc(8,10,0)
      -- Workaround for "-fbyte-code" command line option handling in ghc
      -- 8.10.1.  The use of `noArgM' and `pure $ gopt_set ...' for
      -- "-fbyte-code" option in "compiler/main/DynFlags.hs" is ignoring the
      -- updated hscTarget ...
      dflags1b =
        if "-fbyte-code" `elem` ghc_args
           then gopt_set (dflags1 {hscTarget=HscInterpreted}) Opt_ByteCode
           else dflags1
#else
      dflags1b = dflags1
#endif

  (dflags2, lfileish, warnings) <- parseDynamicFlagsCmdLine dflags1b largs

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
       make phased_inputs False force_recomp (outputFile dflags3)


-- ---------------------------------------------------------------------
--
-- Finkel specific options
--
-- ---------------------------------------------------------------------

data FinkelOption = FinkelOption
  { finkelDebug  :: Bool
  , finkelDumpHs :: Bool
  , finkelHelp   :: Maybe FinkelHelp
  , finkelHsDir  :: Maybe FilePath
  }

data FinkelHelp
  = Languages
  | Usage
  | Version

defaultFinkelOption :: FinkelOption
defaultFinkelOption = FinkelOption
  { finkelDebug = False
  , finkelDumpHs = False
  , finkelHelp = Nothing
  , finkelHsDir = Nothing
  }

partitionFinkelOptions :: [String] -> ([String], [String])
partitionFinkelOptions = partition ("--fnk-" `isPrefixOf`)

parseFinkelOption :: [String] -> IO FinkelOption
parseFinkelOption args =
  case getOpt Permute finkelOptDescrs args of
    (o,_,[]) -> pure $ foldl (flip id) defaultFinkelOption o
    (_,_,es) -> do
      me <- getProgName
      throwFinkelExceptionIO (FinkelException (me ++ ": " ++ concat es))

finkelOptDescrs :: [OptDescr (FinkelOption -> FinkelOption)]
finkelOptDescrs =
  [ opt ["fnk-debug"]
        (NoArg (\o -> o {finkelDebug = True}))
        "Show debug messages."
  , opt ["fnk-dump-hs"]
        (NoArg (\o -> o {finkelDumpHs = True}))
        "Dump Haskell source code."
  , opt ["fnk-help"]
        (NoArg (\o -> o {finkelHelp = Just Usage}))
        "Show this help."
  , opt ["fnk-hsdir"]
        (ReqArg (\path o -> o {finkelHsDir = Just path}) "DIR")
        "Save Haskell source code to DIR."
  , opt ["fnk-languages"]
        (NoArg (\o -> o {finkelHelp = Just Languages}))
        "Show supported language extensions."
  , opt ["fnk-version"]
        (NoArg (\o -> o {finkelHelp = Just Version}))
        "Show Finkel version and exit."
  ]
  where
    opt = Option []

opt2env :: FinkelOption -> FnkEnv
opt2env opt = defaultFnkEnv
  { envDebug  = finkelDebug opt
  , envDumpHs = finkelDumpHs opt
  , envHsDir  = finkelHsDir opt }

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
      , usageInfo "OPTIONS:\n" finkelOptDescrs
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

-- XXX: Add option to specify path of ghc executable?
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
  , "--info"
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
  let unknowns = [f | (f@ ('-':_)) <- fileish]
      oneErr f = "unrecognised flag: " ++ f ++ "\n"
  unless (null unknowns)
         (throwGhcException (UsageError (concatMap oneErr unknowns)))

-- | True if given 'String' was module name, Finkel source file, or
-- Haskell source file.
isSourceTarget :: String -> Bool
isSourceTarget str = looksLikeModuleName str || isFnkFile str || isHsFile str

-- | Until ghc-8.6.0, 'configureHandleEncoding' did not exist.
configureHandleEncoding' :: IO ()
#if MIN_VERSION_ghc(8,6,0)
configureHandleEncoding' = configureHandleEncoding
#else
configureHandleEncoding' = return ()
#endif

foreign import ccall safe "initGCStatistics"
  initGCStatistics :: IO ()
