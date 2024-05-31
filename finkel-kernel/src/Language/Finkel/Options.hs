-- | Codes for command line options.
module Language.Finkel.Options
  ( FnkPluginOptions(..)
  , defaultFnkPluginOptions
  , fnkPluginOptions
  , fpoPragma
  , fpoIgnore
  , printPluginUsage

  , FnkSrcOptions (..)
  , defaultFnkSrcOptions
  , fromFnkSrcOptions
  ) where

-- base
import System.Console.GetOpt (ArgDescr (..), OptDescr (..), usageInfo)
import System.Environment    (getProgName)

-- Internal
import Language.Finkel.Fnk


-- ------------------------------------------------------------------------
--
-- Options for plugin
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

printPluginUsage :: String -> IO ()
printPluginUsage mod_name = do
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

