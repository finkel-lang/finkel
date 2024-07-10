{-# LANGUAGE CPP #-}
-- | Plugin version of the finkel compiler.
module Language.Finkel.Plugin
  ( -- * Finkel plugin
    plugin
  , pluginWith
  , setFinkelPluginWithArgs
  )
where

#include "ghc_modules.h"

-- base
#if MIN_VERSION_ghc(9,6,0)
import Control.Monad.IO.Class       (MonadIO (..))
#endif

#if !MIN_VERSION_ghc(9,2,0)
import Data.Functor                 (void)
#endif

-- ghc
import GHC_Driver_Env               (HscEnv (..))
import GHC_Driver_Monad             (GhcMonad (..))
import GHC_Plugins                  (Plugin (..), PluginWithArgs (..),
                                     StaticPlugin (..), defaultPlugin,
                                     flagRecompile)

#if MIN_VERSION_ghc(9,4,0)
import GHC.Driver.Plugins           (Plugins (..))
#endif

#if !MIN_VERSION_ghc(9,2,0)
import GHC                          (setSessionDynFlags)
import GHC_Driver_Session           (DynFlags (..))
#endif

-- Internal
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

-- | Initialize finkel plugin with given arguments.
setFinkelPluginWithArgs :: GhcMonad m => Plugin -> [String] -> m ()
setFinkelPluginWithArgs plgn args = do
  hsc_env <- getSession

#if MIN_VERSION_ghc(9,6,0)
  -- In ghc >= 9.6, updating current session with driverPlugin, because
  -- `GHC.Loader.initializePlugins' does not check the addition of static
  -- plusing, according to the comment in the function.
  hsc_env' <- liftIO $ driverPlugin plgn args hsc_env
  let sp = StaticPlugin (PluginWithArgs plgn args)
      old_plugins = hsc_plugins hsc_env'
      old_static_plugins = staticPlugins old_plugins
      new_static_plugins = sp : old_static_plugins
      new_plugins = old_plugins {staticPlugins = new_static_plugins}
  setSession (hsc_env' {hsc_plugins = new_plugins})
#elif MIN_VERSION_ghc(9,4,0)
  let sp = StaticPlugin (PluginWithArgs plgn args)
      old_plugins = hsc_plugins hsc_env
      old_static_plugins = staticPlugins old_plugins
      new_static_plugins = sp : old_static_plugins
      new_plugins = old_plugins {staticPlugins = new_static_plugins}
  setSession (hsc_env {hsc_plugins = new_plugins})
#elif MIN_VERSION_ghc(9,2,0)
  -- In ghc < 9.6, ading static plugin. From ghc 9.2, plugins are stored in
  -- HscEnv. Before 9.2, plugins are stored in DynFlags.
  let sp = StaticPlugin (PluginWithArgs plgn args)
      old_static_plugins = hsc_static_plugins hsc_env
      new_static_plugins = sp : old_static_plugins
  setSession (hsc_env {hsc_static_plugins = new_static_plugins})
#else
  let sp = StaticPlugin (PluginWithArgs plgn args)
      old_dflags = hsc_dflags hsc_env
      old_staticPlugins = staticPlugins old_dflags
      new_staticPlugins = sp : old_staticPlugins
      new_dflags = old_dflags {staticPlugins = new_staticPlugins}
  void (setSessionDynFlags new_dflags)
#endif
