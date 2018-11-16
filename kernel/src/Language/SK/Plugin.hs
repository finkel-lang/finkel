-- | Module containing GHC frontend plugin for skc.
module Language.SK.Plugin
   ( -- * SK kernel compiler frontend plugin
     frontendPlugin

     -- * Helpers
   , makeSkFrontend
   , skPluginMain
   ) where

-- base
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Maybe (fromMaybe, isJust)
import Data.List (find, isPrefixOf, isSuffixOf)
import Data.Version (showVersion)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (exitFailure, exitWith)
import System.IO (hPutStr, stderr)
import System.Process (rawSystem)

-- ghc
import DriverPhases (Phase(..))
import GhcMonad (Ghc(..))
import GhcPlugins (FrontendPlugin(..), defaultFrontendPlugin)

-- ghc-paths
import qualified GHC.Paths as GhcPaths

-- Internal
import Language.SK.Emit
import Language.SK.Make
import Language.SK.Run
import Language.SK.SKC
import Paths_sk_kernel (version)


-- ---------------------------------------------------------------------
--
-- The frontend plugin
--
-- ---------------------------------------------------------------------

frontendPlugin :: FrontendPlugin
frontendPlugin =
  defaultFrontendPlugin {frontend = makeSkFrontend "skkc" []}

-- | Make a frontend plugin.
makeSkFrontend :: String
                   -- ^ Name of executable.
               -> [(String,Macro)]
                   -- ^ List of macros loaded to macro expander.
               -> [String]
                   -- ^ Plugin options
               -> [(FilePath, Maybe Phase)]
                   -- ^ Pairs of source file and phase.
               -> Ghc ()
makeSkFrontend name macros flags args = do
  let options = (parseOptions flags) {input = args}
      act o  = do debugSkc (concat [ ";;; flags: ", show flags, "\n"
                                   , ";;; args:  ", show args ])
                  chooseAction name (action options) o
      debug = skDebug options
      macros' = mergeMacros (envMacros defaultSkEnv)
                            (makeEnvMacros macros)
      sk_env = defaultSkEnv { envDebug = debug
                            , envDefaultMacros = macros'
                            , envMacros = macros' }
  handleSkException
    (\(SkException se) -> liftIO (do putStrLn se
                                     exitFailure))
    (fmap fst (toGhc (act options) sk_env))


-- ---------------------------------------------------------------------
--
-- Command line option
--
-- ---------------------------------------------------------------------

-- | Action to perform in modal manner.
--
-- Note that ghc make mode does not work when frontend plugin were
-- used. So when "--make" option were passed to sk compiler executable,
-- converts the "--make" flag to "--sk-make".
--
data SkAction
  = SkHsrc
  | SkMake
  | SkHelp
  | SkVersion
  deriving (Eq, Show)

-- | Options specified from command line arguments.
data SkcOptions = SkcOptions {
    -- | Whether to perform typecheck during Haskell source code
    -- generation.
    performTypecheck :: Bool,
    -- | SK source code files.
    input :: [(FilePath, Maybe Phase)],
    -- | Field to store "-o" option, for Dynflags.outputFile.
    skO :: Maybe String,
    -- | Field to store "-c" option.
    skC :: Bool,
    -- | The action to perform.
    action :: SkAction,
    -- | Field to store explicitly specified force recompilation.
    skForceRecomp :: Bool,
    -- | Flag value for debugging.
    skDebug :: Bool,
    -- | Dump Haskell codes during make.
    skDumpHs :: Bool,
    -- | Directory to save Haskell codes during make.
    skHsDir :: Maybe FilePath
  } deriving (Eq, Show)

initialSkcOptions :: SkcOptions
initialSkcOptions =
  SkcOptions { performTypecheck = True
             , input = []
             , skO = Nothing
             , skC = False
             , action = SkMake
             , skForceRecomp = False
             , skDebug = False
             , skDumpHs = False
             , skHsDir = Nothing }

parseOptions :: [String] -> SkcOptions
parseOptions args =
  case getOpt Permute (hiddenDescrs ++ visibleDescrs) args of
    (o,_,[]) -> foldl (flip id) initialSkcOptions o
    (_,_,es) -> error (show es)

option :: [String] -> ArgDescr a -> String -> OptDescr a
option = Option []

visibleDescrs :: [OptDescr (SkcOptions -> SkcOptions)]
visibleDescrs =
  [ option ["sk-hsrc"]
           (NoArg (\o -> o {action=SkHsrc}))
           "Generate Haskell source code."
  , option ["sk-no-typecheck"]
           (NoArg (\o -> o {performTypecheck=False}))
           "Skip type check in Haskell code gen."
  , option ["sk-debug"]
           (NoArg (\o -> o {skDebug=True}))
           "Show debug messages."
  , option ["sk-help"]
           (NoArg (\o -> o {action=SkHelp}))
           "Show this help."
  , option ["sk-version"]
           (NoArg (\o -> o {action=SkVersion}))
           "Show sk kernel version."
  , option ["sk-dump-hs"]
           (NoArg (\o -> o {skDumpHs=True}))
           "Dump Haskell source code."
  , option ["sk-hsdir"]
           (ReqArg (\path o -> o {skHsDir=Just path}) "DIR")
           "Directory to save generated haskell source."
  ]

hiddenDescrs :: [OptDescr (SkcOptions -> SkcOptions)]
hiddenDescrs =
  [ option ["sk-make"]
           (NoArg (\o -> o {action=SkMake}))
           "Intercept '--make'."
  , option ["sk-o"]
           (ReqArg (\file o -> o {skO=Just file}) "OUTPUT")
           "Intercept '-o'."
  , option ["sk-c"]
           (NoArg (\o -> o {skC=True}))
           "Intercept '-c'"
  , option ["sk-force-recomp"]
           (NoArg (\o -> o {skForceRecomp=True}))
           "Intercept '-fforce-recomp'"
  ]


-- ---------------------------------------------------------------------
--
-- Actions
--
-- ---------------------------------------------------------------------

chooseAction :: String -> SkAction -> SkcOptions -> Skc ()
chooseAction name act o =
  case act of
    SkMake    -> doMake o
    SkHsrc    -> hsrc o
    SkHelp    -> help name
    SkVersion -> printVersion

doMake :: SkcOptions -> Skc ()
doMake o = do
  initSessionForMake
  modifySkEnv (\e -> e { envDumpHs = skDumpHs o
                       , envHsDir = skHsDir o })
  make (input o) (skC o) (skForceRecomp o) (skO o)

hsrc :: SkcOptions -> Skc ()
hsrc o = do
  file <- case input o of
    [(x, _)] -> return x
    []       -> failS "hsrc: No input file"
    _        -> failS "hsrc: Multiple input files not supported."
  (mdl, sp) <- compileWithSymbolConversion file
  when (performTypecheck o)
       (void (tcHsModule (Just file) Nothing False mdl))
  hssrc <- genHsSrc sp (Hsrc mdl)
  liftIO (case skO o of
             Nothing  -> putStrLn hssrc
             Just out -> writeFile out hssrc)

help :: String -> Skc ()
help name = liftIO (putStrLn (unlines (usage name)))

usage :: String -> [String]
usage name =
  [ "USAGE: " ++ name ++ " [OPTIONS] [FILES]"
  , ""
  , skcUsage "OPTIONS:\n"
  , "  Other options are passed to ghc."]

skcUsage :: String -> String
skcUsage header = usageInfo header visibleDescrs

printVersion :: Skc ()
printVersion = liftIO (putStrLn v)
  where
    v = "sk kernel version " ++ showVersion version


-- ---------------------------------------------------------------------
--
-- Main function builder
--
-- ---------------------------------------------------------------------

-- | Make the main action for SK compiler executable with given frontend
-- plugin name and package.
--
-- Prepend argument passed to GHC frontend plugin and wraps input
-- arguments with "-ffrontend-opt". This function intercepts some of the
-- conflicting arguments for 'ghc' command and frontend plugin, such as
-- "--show-info" option.
--
-- Internally, this function invokes "ghc" executable via
-- 'System.Process.rawSystem' with custom argument and the frontend
-- plugin found in the given argument.
--
skPluginMain :: String -- ^ Name of the module containing the definition
                       -- of 'frontendPlugin'.
             -> String -- ^ Package name.
             -> IO ()
skPluginMain frontendModuleName packageName = do
  argIns <- getArgs
  let argOuts = [ "--frontend", frontendModuleName
                , "-plugin-package", packageName ]
      (srcs, skopts, ghcopts) = groupOptions [] [] [] argIns
      ghcopts' = reverse ghcopts
      ghc = fromMaybe GhcPaths.ghc (findGhc skopts)
  debug <- if isJust (find (== "--sk-debug") skopts)
              then return True
              else getSkcDebug
  when debug
       (hPutStr stderr
                (unlines [ ";;; ghc: " ++ show ghc
                         , ";;; argIns: " ++ show argIns
                         , ";;; srcs: " ++ show srcs
                         , ";;; skopts: " ++ show skopts
                         , ";;; ghcopts': " ++ show ghcopts']))
  let skopts' | isJust (find (== "--sk-debug") skopts) = skopts
              | debug = "-ffrontend-opt":"--sk-debug":skopts
              | otherwise = skopts
      getO =
        let go [] = []
            go (_:"--sk-o":_:val:_) = [val]
            go (_:rest) = go rest
        in  go skopts
      rawGhcOpts =
        if null getO
          then ghcopts'
          else ghcopts' ++ ("-o":getO)
      -- Testing whether ghc was invoked for building shared
      -- library. This may happen when building cabal package, to
      -- suppress unwanted warning messages.
      buildingSharedLib =
        null srcs && elem "-shared" ghcopts' && elem "-dynamic" ghcopts'
  exitCode <-
    -- When any of the conflicting options with frontend plugin were
    -- set, OR building shared library, delegate to raw ghc without
    -- frontend plugin.
    if any (`elem` conflictingOptions) ghcopts' || buildingSharedLib
       then do
         when debug
              (do putStrLn "Running raw ghc"
                  putStrLn ("rawGhcOpts: " ++ show rawGhcOpts))
         rawSystem ghc rawGhcOpts
       else do
         let args = concat [argOuts,skopts',ghcopts',"-x":"hs":srcs]
         rawSystem ghc args
  exitWith exitCode

-- | Categorize command line options.
groupOptions :: [String] -> [String] -> [String] -> [String]
             -> ([String], [String], [String])
groupOptions = go where
  go sksrc skopt ghcopt args =
    case args of
       -- Irregular pattern for "--sk-pgmf" option. Output file comes
       -- before the "--gk-pgmf".
       x:"--sk-pgmf":xs ->
         go sksrc (fopt:"--sk-pgmf":fopt:x: skopt) ghcopt xs

       -- Separate options one by one with predicates. Some are SK
       -- source code, some are conflicting flag with '--frontend'
       -- option, ... etc.
       x:xs
         | isSkSrc x ->
             go (x:sksrc) skopt ghcopt xs
         | isMake x ->
             go sksrc (fopt:"--sk-make":skopt) ghcopt xs
         | isO x ->
             go sksrc (fopt: "--sk-o":fopt:head xs:skopt) ghcopt (tail xs)
         | isC x ->
             go sksrc (fopt:"--sk-c":skopt) ghcopt ("-no-link":xs)
         | isForceRecomp x ->
             go sksrc (fopt:"--sk-force-recomp":skopt) (x:ghcopt) xs
         | isSkOption x ->
             go sksrc (fopt:head xs:fopt:x:skopt) ghcopt (tail xs)
         | isSkFlag x ->
             go sksrc (fopt:x:skopt) ghcopt xs
         | otherwise -> go sksrc skopt (x:ghcopt) xs

       -- Done.
       [] -> (sksrc, skopt, ghcopt)
     where
      fopt = "-ffrontend-opt"

-- | When any of options listed here were found, invoke raw @ghc@
-- without using SK frontend plugin. Otherwise @ghc@ will complain with
-- error message. These options are listed in "ghc/Main.hs" as
-- `mode_flags'.
conflictingOptions :: [String]
conflictingOptions =
  [ "--info"
  , "--show-options"
  , "--supported-languages"
  , "--supported-extensions"
  , "--show-packages"
  , "--show-iface"
  , "--print-libdir"
  , "--abi-hash"
  ]

-- | Test to find the string sequence "--make". Option "--make" could
-- not be used as frontend plugin option.
isMake :: String -> Bool
isMake = (== "--make")

-- | Another option to intercept.
isO :: String -> Bool
isO = (== "-o")

-- | Intercept "-c".
isC :: String -> Bool
isC = (== "-c")

-- | Intercept "-fforce-recomp".
isForceRecomp :: String -> Bool
isForceRecomp = (== "-fforce-recomp")

-- | SK source code extension is hard coded as ".sk". Surely it would be
-- better to have alternative choice specified via command line
-- argument.
isSkSrc :: String -> Bool
isSkSrc = isSuffixOf ".sk"

-- | Argument passed to SK plugin with value.
isSkOption :: String -> Bool
isSkOption str = str `elem` ["--sk-out", "--sk-ghc"]

-- | Argument passed to SK plugin without value.
isSkFlag :: String -> Bool
isSkFlag = isPrefixOf "--sk-"

-- | Find argument passed to "--sk-ghc" option.
findGhc :: [String] -> Maybe String
findGhc xs =
  case xs of
    ghc:_   :"--sk-ghc":_ -> Just ghc
    _  :rest              -> findGhc rest
    []                    -> Nothing
