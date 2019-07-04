{-# LANGUAGE CPP #-}
-- | Main function for sk compiler.
--
-- This module contains 'main' function, which does similar and
-- simplified works done in @"ghc/Main.hs"@ found in ghc source.
--
module Language.SK.Main
  ( defaultMain
  , defaultMainWith
  ) where

-- base
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO(..))
import Data.List (isPrefixOf, partition)
import Data.Version (showVersion)
import System.Console.GetOpt ( ArgDescr(..), ArgOrder(..), OptDescr(..)
                             , getOpt, usageInfo )
import System.IO (BufferMode(..), hSetBuffering, stdout, stderr)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitWith)
import System.FilePath (normalise)
import System.Process (rawSystem)

-- ghc
import DynFlags ( DynFlags(..), GeneralFlag(..), HasDynFlags(..)
                , defaultFatalMessager, defaultFlushOut
                , gopt, parseDynamicFlagsCmdLine)
import GHC (defaultErrorHandler)
import GhcMonad (printException)
import HscTypes (handleFlagWarnings, handleSourceError)
import Panic (GhcException(..), throwGhcException)
import SrcLoc (mkGeneralLocated, unLoc)
import UniqSupply (initUniqSupply)
import Util (looksLikeModuleName)

-- ghc-boot
#if MIN_VERSION_ghc(8,6,0)
import GHC.HandleEncoding (configureHandleEncoding)
#endif

-- ghc-paths
import qualified GHC.Paths

-- internal
import Language.SK.Make
import Language.SK.SKC
import Language.SK.TargetSource
import qualified Paths_sk_kernel


-- ---------------------------------------------------------------------
--
-- The main function
--
-- ---------------------------------------------------------------------

-- [Main entry point]
-- ~~~~~~~~~~~~~~~~~~
--
-- Formerly, sk compiler executable was written as ghc frontend
-- plugin. However, passing conflicting options used in ghc's "--make"
-- to the sk compiler executable was cumbersome, since frontend option
-- cannot be used when ghc is invoked as make mode.
--
-- Functions exported from this module is doing almost same work done in
-- the "Main" module of ghc executable, but command line argument
-- handling is more simple, since sk compiler delegates works done in
-- non-make mode to ghc executable.

-- | Function used by sk kernel compiler.
defaultMain :: IO ()
defaultMain = defaultMainWith []

-- | Make a main compiler function from given list of macros.
--
-- This functions does simplified command line argument parsing done in
-- default @ghc@ mode (i.e., @make@ mode).
--
defaultMainWith :: [(String, Macro)]
                -- ^ List of pairs of macro name and 'Macro' value loaded
                -- to macro expander.
                -> IO ()
defaultMainWith macros = do
  args0 <- getArgs
  if any (`elem` rawGhcOptions) args0
     then rawGhc args0
     else do
       -- Filter out sk flags and `--make' flag when exist, otherwise
       -- make flag would be treated as input file, and sk flags as
       -- unknown flags from this point.
       let (skopts, args1) = partitionSkOptions args0
           args2 = filter (/= "--make") args1
           sk_opts = parseSkOption skopts
           sk_env0 = opt2env sk_opts

           -- Using macros from argument as first argument to
           -- 'mergeMacros', so that the caller of this function can
           -- have a chance to override the behaviour of specialforms in
           -- 'defaultSkEnv'.
           macros' = mergeMacros (makeEnvMacros macros)
                                 (envMacros defaultSkEnv)

           sk_env1 = sk_env0 { envDefaultMacros = macros'
                             , envMacros = macros' }
           next | skHelp sk_opts    = printSkHelp
                | skVersion sk_opts = printSkVersion
                | otherwise         = main' sk_env1 args1 args2

       -- XXX: Handle '-B' option properly.
       next

main' :: SkEnv -> [String] -> [String] -> IO ()
main' sk_env orig_args args = do
  initGCStatistics
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  configureHandleEncoding'
  defaultErrorHandler
    defaultFatalMessager
    defaultFlushOut
    (runSkc (handleSkException
              (\(SkException se) -> liftIO (do putStrLn se
                                               exitFailure))
              (handleSourceError (\se -> do printException se
                                            liftIO exitFailure)
                                 (main'' orig_args args)))
            sk_env)

main'' :: [String] -> [String] -> Skc ()
main'' orig_args args = do
  dflags0 <- getDynFlags
  let largs = map onTheCmdLine args
      onTheCmdLine = mkGeneralLocated "on the commandline"
      dflags1 = dflags0 {verbosity = 1}
  (dflags2, lfileish, warnings) <- parseDynamicFlagsCmdLine dflags1 largs

  let fileish = map unLoc lfileish

      -- Partition objects and source codes in args. Delegate to raw ghc
      -- when source codes were null. Don't bother with ldInput,
      -- delegate the linking work to raw ghc.
      (srcs, _non_srcs) = partition isSourceTarget fileish

  case srcs of
     [] -> liftIO (rawGhc orig_args)
     _  -> do
       -- Using 'setDynFlags' instead of 'setSessionDynFlags', since
       -- 'setSessionDynFlags' will be called from 'initSessionForMake'
       -- below.
       setDynFlags dflags2

       -- Some IO works. Check unknown flags, update uniq supply ...
       liftIO (do checkUnknownFlags fileish
                  initUniqSupply (initialUnique dflags2)
                                 (uniqueIncrement dflags2))

       -- Show DynFlags warnings.
       handleSourceError
         (\e -> do printException e
                   liftIO exitFailure)
         (liftIO (handleFlagWarnings dflags2 warnings))

       -- Initialization works for SK.
       initSessionForMake

       -- At the moment, compiling with phase specification are not
       -- supported, phase is always set to 'Nothing'.
       let phased_srcs = map phase_it srcs
           phase_it path = (onTheCmdLine (normalise path), Nothing)
           force_recomp = gopt Opt_ForceRecomp dflags2

       -- Do the `make' work.
       make phased_srcs False force_recomp (outputFile dflags2)


-- ---------------------------------------------------------------------
--
-- Sk specific options
--
-- ---------------------------------------------------------------------

data SkOption = SkOption
  { skDebug :: Bool
  , skDumpHs :: Bool
  , skHelp :: Bool
  , skHsDir :: Maybe FilePath
  , skVersion :: Bool
  }

defaultSkOption :: SkOption
defaultSkOption = SkOption
  { skDebug = False
  , skDumpHs = False
  , skHelp = False
  , skHsDir = Nothing
  , skVersion = False
  }

partitionSkOptions :: [String] -> ([String], [String])
partitionSkOptions = partition ("--sk-" `isPrefixOf`)

parseSkOption :: [String] -> SkOption
parseSkOption args =
  case getOpt Permute skOptDescrs args of
    (o,_,[]) -> foldl (flip id) defaultSkOption o
    (_,_,es) -> error (show es)

skOptDescrs :: [OptDescr (SkOption -> SkOption)]
skOptDescrs =
  [ opt ["sk-debug"]
        (NoArg (\o -> o {skDebug = True}))
        "Show debug messages."
  , opt ["sk-dump-hs"]
        (NoArg (\o -> o {skDumpHs = True}))
        "Dump Haskell source code."
  , opt ["sk-help"]
        (NoArg (\o -> o {skHelp = True}))
        "Show this help."
  , opt ["sk-hsdir"]
        (ReqArg (\path o -> o {skHsDir = Just path}) "DIR")
        "Save Haskell source code to DIR."
  , opt ["sk-version"]
        (NoArg (\o -> o {skVersion = True}))
        "Dump sk version and exit."
  ]
  where
    opt = Option []

opt2env :: SkOption -> SkEnv
opt2env opt = defaultSkEnv
  { envDebug = skDebug opt
  , envDumpHs = skDumpHs opt
  , envHsDir = skHsDir opt }

printSkHelp :: IO ()
printSkHelp = do
  name <- getProgName
  putStrLn (unlines (message name))
  where
    message name =
      [ "USAGE: " ++ name ++ " [command-line-options-and-files]"
      , ""
      , usageInfo "OPTIONS:\n" skOptDescrs
      , "  Other options are passed to ghc." ]

printSkVersion :: IO ()
printSkVersion = putStrLn v
  where
    v = "sk kernel version " ++ showVersion Paths_sk_kernel.version


-- ---------------------------------------------------------------------
--
-- Auxiliary
--
-- ---------------------------------------------------------------------

-- XXX: Add option to specify path of ghc executable?
rawGhc :: [String] -> IO ()
rawGhc args = rawSystem GHC.Paths.ghc args >>= exitWith

-- | When any of options listed here were found, invoke raw @ghc@
-- without using SK frontend plugin. Otherwise @ghc@ will complain with
-- error message. These options are listed in "ghc/Main.hs" as
-- `mode_flags'.
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

-- | True if given 'String' was module name, sk source file, or haskell
-- source file.
isSourceTarget :: String -> Bool
isSourceTarget str =
  looksLikeModuleName str || isSkFile str || isHsFile str

-- | Until ghc-8.6.0, 'configureHandleEncoding' did not exist.
configureHandleEncoding' :: IO ()
#if MIN_VERSION_ghc (8,6,0)
configureHandleEncoding' = configureHandleEncoding
#else
configureHandleEncoding' = return ()
#endif

foreign import ccall safe "initGCStatistics"
  initGCStatistics :: IO ()
