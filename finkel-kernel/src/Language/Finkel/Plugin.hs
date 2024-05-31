{-# LANGUAGE CPP #-}
-- | Plugin version of the finkel compiler.
module Language.Finkel.Plugin
  ( -- * Finkel plugin
    plugin
  , pluginWith
  )
where

#include "ghc_modules.h"

import GHC_Plugins                  (Plugin (..), defaultPlugin, flagRecompile)

import Language.Finkel.Fnk          (FnkEnv)
import Language.Finkel.SpecialForms (defaultFnkEnv)

#if MIN_VERSION_ghc(9,6,0)
import Language.Finkel.Hooks        (finkelHooks)
#else
import Language.Finkel.ParsedResult (fnkParsedResultAction)
#endif

-- | Finkel compiler plugin.
plugin :: Plugin
plugin = pluginWith "Language.Finkel.Plugin" defaultFnkEnv

-- | Finkel compiler plugin with given 'FnkEnv'.
pluginWith
  :: String -- ^ Plugin module name.
  -> FnkEnv -- ^ The environment used by the plugin.
  -> Plugin
pluginWith mod_name fnk_env =
#if MIN_VERSION_ghc(9,6,0)
  defaultPlugin
    { driverPlugin = finkelHooks mod_name fnk_env
    , pluginRecompile = flagRecompile
    }
#else
  defaultPlugin
    { parsedResultAction = fnkParsedResultAction mod_name fnk_env
    , pluginRecompile = flagRecompile
    }
#endif
