-- | Codes for command line options.
module Language.Finkel.Options
  (
    -- * Plugin options
    FnkPluginOptions(..)
  , defaultFnkPluginOptions
  , fnkPluginOptions
  , fpoPragma
  , fpoIgnore
  , printPluginUsage

    -- * Fnk source options
  , FnkSrcOptions (..)
  , defaultFnkSrcOptions
  , fromFnkSrcOptions

    -- * FnkEnv options
  , fnkEnvOptions
  , fnkEnvOptionsWithLib
  , partitionFnkEnvOptions
  , fromFnkEnvOptions
  , fnkEnvOptionsUsage

  ) where

-- base
import Data.Char                 (toLower)
import Data.List                 (isPrefixOf, partition)
import System.Console.GetOpt     (ArgDescr (..), OptDescr (..), usageInfo)
import System.Environment        (getProgName)

-- Internal
import Language.Finkel.Exception
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
    is_removed_option (Option so _ _ _) = so == ['B']

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


-- ---------------------------------------------------------------------
--
-- FnkEnv options
--
-- ---------------------------------------------------------------------

-- | Separate Finkel debug options from others.
partitionFnkEnvOptions
   :: [String]
   -- ^ Flag inputs, perhaps given as command line arguments.
   -> ([String], [String])
   -- ^ Pair of @(finkel_flags, other_flags)@.
partitionFnkEnvOptions = partition test
  where
    -- The "-B" option is to update the ghc libdir in FnkEnv.
    test arg = "--fnk-" `isPrefixOf` arg || "-B" `isPrefixOf` arg

-- | Command line option handlers to update 'FnkDumpFlag' in 'FnkEnv'.
fnkEnvOptions :: [OptDescr (FnkEnv -> FnkEnv)]
fnkEnvOptions =
  [ opt ["fnk-verbose"]
        (ReqArg (\i o -> o {envVerbosity = parseVerbosity i}) "INT")
        "Set verbosity level to INT."
  , opt ["fnk-hsdir"]
        (ReqArg (\path o -> o {envHsOutDir = Just path}) "DIR")
        "Set Haskell code output directory to DIR."

  -- Dump and trace options
  , debug_opt Fnk_dump_dflags "Dump DynFlags settings."
  , debug_opt Fnk_dump_expand "Dump expanded code."
  , debug_opt Fnk_dump_hs "Dump Haskell source code."
  , debug_opt Fnk_trace_expand "Trace macro expansion."
  , debug_opt Fnk_trace_session "Trace session env."
  , debug_opt Fnk_trace_make "Trace make function."
  , debug_opt Fnk_trace_spf "Trace builtin special forms."
  ]
  where
    opt = Option []
    debug_opt flag = opt [to_str flag] (NoArg (foptSet flag))
    to_str = map replace . show
    replace '_' = '-'
    replace c   = toLower c
    parseVerbosity = readOrFinkelException "INT" "verbosity"

-- | Options for @FnkEnv@ with an option to set ghc @libdir@.
fnkEnvOptionsWithLib :: [OptDescr (FnkEnv -> FnkEnv)]
fnkEnvOptionsWithLib = lib_option : fnkEnvOptions
  where
    lib_option =
      Option ['B'] []
             (ReqArg (\path o -> o {envLibDir = Just path}) "DIR")
             "Set ghc library directory to DIR."

-- | Convert 'fnkEnvOptions' to list of 'OptDescr' taking a function modifying
-- 'FnkEnv'.
fromFnkEnvOptions :: ((FnkEnv -> FnkEnv) -> a) -> [OptDescr a]
fromFnkEnvOptions f = map (fmap f) fnkEnvOptionsWithLib

-- | Usage information for 'fnkEnvOptions', without @-B@ option.
fnkEnvOptionsUsage :: String -> String
fnkEnvOptionsUsage = flip usageInfo fnkEnvOptions
