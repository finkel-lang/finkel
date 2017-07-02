-- | Module containing GHC frontend plugin for skc.
module SK.Core.Plugin
   ( frontendPlugin
   ) where

-- base
import Control.Monad (void, when)
import System.Console.GetOpt
import System.Exit (exitFailure)

-- ghc
import GhcPlugins (FrontendPlugin(..), defaultFrontendPlugin)

-- Internal
import SK.Core.Emit
import SK.Core.GHC
import SK.Core.Make
import SK.Core.Run
import SK.Core.SKC


---
--- The frontend plugin
---

frontendPlugin :: FrontendPlugin
frontendPlugin = defaultFrontendPlugin {frontend = skFrontend}

skFrontend :: [String] -> [(String, Maybe Phase)] -> Ghc ()
skFrontend flags args = do
  let options = (parseOptions flags) {input = args}
      act o  = do debugIO (do putStrLn ("flags: " ++ show flags)
                              putStrLn ("args:  " ++ show args))
                  chooseAction (action options) o
      debug = skDebug options
      sk_env = initialSkEnv {envDebug = debug}
  ret <- toGhc (act options) sk_env
  case ret of
    Left err -> liftIO (putStrLn err >> exitFailure)
    Right _  -> return ()

---
--- Command line option
---

-- | Action to perform in modal manner.
data SkAction
  = SkHsrc
  | SkMake
  | SkHelp
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
    -- | Flag value for debugging.
    skDebug :: Bool
  } deriving (Eq, Show)

initialSkcOptions :: SkcOptions
initialSkcOptions =
  SkcOptions { performTypecheck = True
             , input = []
             , skO = Nothing
             , skC = False
             , action = SkMake
             , skDebug = False }

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
  ]

---
--- Actions
---

chooseAction :: SkAction -> SkcOptions -> Skc ()
chooseAction act o =
  case act of
    SkMake -> make (input o) (skC o) (skO o)
    SkHsrc -> hsrc o
    SkHelp -> help

hsrc :: SkcOptions -> Skc ()
hsrc o = do
  file <- case input o of
    [(x, _)] -> return x
    []  -> failS "hsrc: No input file"
    _   -> failS "hsrc: Multiple input files not supported."
  (mdl, sp) <- compileSkModule file
  when (performTypecheck o)
       (void (tcHsModule (Just file) False mdl))
  hssrc <- genHsSrc sp mdl
  liftIO (case (skO o) of
             Nothing -> putStrLn hssrc
             Just out -> writeFile out hssrc)

help :: Skc ()
help = liftIO (putStrLn (unlines usage))

usage :: [String]
usage =
  [ "USAGE: skc [OPTIONS] [FILES]"
  , ""
  , skcUsage "OPTIONS:\n"
  , "  Other options are passed to ghc."]

skcUsage :: String -> String
skcUsage header = usageInfo header visibleDescrs
