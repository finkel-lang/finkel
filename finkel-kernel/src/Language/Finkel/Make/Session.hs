{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Module to manage HscEnv for macro expansion.
module Language.Finkel.Make.Session
  ( withExpanderSettings
  , setExpanding
  , isExpanding
  , bcoDynFlags
  , isInterpreted
  , discardInteractiveContext
  , clearGlobalSession
  ) where

#include "ghc_modules.h"

-- base
import           Control.Concurrent         (MVar, modifyMVar, newMVar)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Data.Foldable              (for_)
import           Data.IORef                 (atomicModifyIORef', newIORef)
import           Data.List                  (intercalate)
import           Data.Maybe                 (isJust)
import           System.IO.Unsafe           (unsafePerformIO)

#if MIN_VERSION_ghc(9,0,0)
import qualified Data.Set                   as Set
#endif

-- exception
import           Control.Monad.Catch        (bracket)

-- ghc
import           GHC_Driver_Env_Types       (HscEnv (..))
import           GHC_Driver_Main            (newHscEnv)
import           GHC_Driver_Monad           (Ghc (..), GhcMonad (..),
                                             Session (..), getSession,
                                             setSession)
import           GHC_Driver_Session         (DynFlags (..), GeneralFlag (..),
                                             GhcLink (..), HasDynFlags (..),
                                             WarningFlag (..), setGeneralFlag',
                                             updOptLevel, wopt_unset)
import           GHC_Utils_Outputable       (Outputable (..), fsep, nest, text,
                                             (<+>))

#if MIN_VERSION_ghc(9,8,0)
import           GHC.Driver.DynFlags        (ParMakeCount (..))
#endif

#if MIN_VERSION_ghc(9,6,0)
import           GHC.Driver.Backend         (backendWritesFiles)
import           GHC.Driver.Session         (topDir, unSetGeneralFlag')
#endif

#if MIN_VERSION_ghc(9,6,0)
import           GHC.Driver.Backend         (backendCanReuseLoadedCode,
                                             interpreterBackend)
#elif MIN_VERSION_ghc(9,2,0)
import           GHC.Driver.Backend         (Backend (..))
#else
import           GHC_Driver_Session         (HscTarget (..))
#endif

#if MIN_VERSION_ghc(9,4,0)
import           GHC.Driver.Env             (discardIC)
#else
import           GHC_Runtime_Context        (InteractiveContext (..),
                                             emptyInteractiveContext)
import           GHC_Types_Name             (nameIsFromExternalPackage)
#endif

#if MIN_VERSION_ghc(9,2,0) && !MIN_VERSION_ghc(9,4,0)
import           GHC.Driver.Env             (hsc_home_unit)
#endif

#if MIN_VERSION_ghc(9,0,0) && !MIN_VERSION_ghc(9,2,0)
import           GHC_Driver_Session         (homeUnit)
#endif

#if MIN_VERSION_ghc(9,0,0)
import           GHC                        (setSessionDynFlags)
import           GHC_Platform_Ways          (Way (..), hostFullWays)
#else
import           GHC_Driver_Session         (Way (..), interpWays, thisPackage)
#endif

#if MIN_VERSION_ghc(9,0,0)
import           GHC.Runtime.Loader         (initializePlugins)
#else
import           DynamicLoading             (initializePlugins)
#endif

-- Internal
import           Language.Finkel.Fnk
import           Language.Finkel.Make.Cache


-- ---------------------------------------------------------------------
--
-- Session management
--
-- ---------------------------------------------------------------------

-- | Perform given action with 'HscEnv' updated for macroexpansion with
-- interactive evaluation, then reset to the preserved original 'HscEnv'.
withExpanderSettings :: Fnk a -> Fnk a
withExpanderSettings act = do
  fnk_env <- getFnkEnv
  dflags <- getDynFlags

  debugWhen' dflags fnk_env Fnk_trace_session
    ["withExpanderSettings: envInvokedMode:" <+> ppr (envInvokedMode fnk_env)]

  case envInvokedMode fnk_env of
    ExecMode      -> withExpanderSettingsE act
    GhcPluginMode -> withExpanderSettingsG act

-- | Like 'withExpanderSettings', but takes a flag to discard interactive
-- context in the session used for the expansion.
withExpanderSettingsE :: Fnk a -> Fnk a
withExpanderSettingsE act =
  do dflags <- getDynFlags
     -- Switching to the dedicated 'HscEnv' for macro expansion when compiling
     -- object code. If not, assuming current session is using the bytecode
     -- interpreter, using the given action as-is.
     if isInterpreted dflags
        then act
        else bracket prepare restore (const act)
  where
    prepare = do
      fnk_env <- getFnkEnv
      hsc_env_old <- getSession

      -- Reusing previous 'HscEnv' for macro expansion if exist, or making a new
      -- 'HscEnv'. When reusing, discarding the previous 'InteractiveContext',
      -- to avoid file local compile time functions to affect other modules.
      case envSessionForExpand fnk_env of
        Just he -> setSession $! discardInteractiveContext he
        Nothing -> do
          he1 <- newHscEnvForExpand fnk_env hsc_env_old
          setSession he1
          postSetSession
          he2 <- getSession
          modifyFnkEnv (\e -> e {envSessionForExpand = Just he2})

      return hsc_env_old

    restore hsc_env_old = do
      hsc_env_new <- getSession
      modifyFnkEnv (\e -> e {envSessionForExpand = Just hsc_env_new})
      setSession hsc_env_old

#if MIN_VERSION_ghc(9,0,0)
    -- To set the "hsc_interp" field in the new session.
    postSetSession = getDynFlags >>= setSessionDynFlags
#else
    postSetSession = return ()
#endif

-- | Make new 'HscEnv' from given 'DynFlags'.
--
-- Adjusting the 'DynFlags' used by the macro expansion session, to support
-- evaluating expressions in dynamic and non-dynamic builds of the Finkel
-- compiler executable.
newHscEnvForExpand :: MonadIO m => FnkEnv -> HscEnv -> m HscEnv
newHscEnvForExpand fnk_env orig_hsc_env = do
  let tr = debugWhen' dflags0 fnk_env Fnk_trace_session
      dflags0 = hsc_dflags orig_hsc_env
      -- XXX: Constantly updating the backend to interpreter, the original
      -- backend information is gone. If the 'bcoDynFlags' was not applied,
      -- compilation of finkel-core package failed in ghc <= 8.10.
      dflags1 = bcoDynFlags dflags0
      dflags2 = if interpHasNoWayDyn
                   then removeWayDyn dflags1
                   else dflags1
  tr [ "newHscEnvForExpand.hsc_targets"
     , nest 2 (fsep (map ppr (hsc_targets orig_hsc_env)))]

#if MIN_VERSION_ghc(9,6,0)
  -- In ghc 9.6, arguments of newHscEnv takes top directory of ghc library path.
  new_hsc_env_0 <- liftIO $! newHscEnv (topDir dflags2) dflags2
#else
  new_hsc_env_0 <- liftIO $! newHscEnv dflags2
#endif
#if MIN_VERSION_ghc(9,4,0)
  -- From ghc 9.4, plugins (loaded and static) are stored in HscEnv instead of
  -- DynFlags. Updating the hsc_plugins and hsc_hooks fields from old hsc_env
  -- value.
  let new_hsc_env_1 = new_hsc_env_0 { hsc_plugins = hsc_plugins orig_hsc_env
                                    , hsc_hooks = hsc_hooks orig_hsc_env }
#elif MIN_VERSION_ghc(9,2,0)
  -- From ghc 9.2, hsc_env has separate fields for loaded plugins and static
  -- plugins.
  let new_hsc_env_1 =
        new_hsc_env_0 { hsc_plugins = hsc_plugins orig_hsc_env
                      , hsc_static_plugins = hsc_static_plugins orig_hsc_env }
#else
  -- No need to update hsc_env, plugins are stored in DynFlags.
  let new_hsc_env_1 = new_hsc_env_0
#endif

  pure new_hsc_env_1

-- | Run given 'Fnk' action with macro expansion settings for 'GhcPluginMode'.
withExpanderSettingsG :: Fnk a -> Fnk a
withExpanderSettingsG act = do
  dflags <- getDynFlags
  fnk_env <- getFnkEnv
  let tr = debugWhen' dflags fnk_env Fnk_trace_session
  if isExpanding dflags
    then do
      -- Clearing the current target, but not using 'withGlobalSession'. The
      -- 'withGlobalSesion' function locks the top level MVar, using it will
      -- cause a dead lock.
      tr ["withExpanderSettingsG: clearing hsc_targets for nested call"]
      tr ["withExpanderSettingsG: keys" <+> text (showExpanding dflags)]
      withEmptyTargets act
    else do
      tr ["withExpanderSettingsG: withGlobalSession"]
      withGlobalSession act

-- Run given action with empty 'hsc_targets', restores the original target after
-- running.
withEmptyTargets :: Fnk a -> Fnk a
withEmptyTargets act0 = bracket prepare restore act1
  where
    prepare = do
      hsc_env <-  getSession
      let orig_targets = hsc_targets hsc_env
      setSession (hsc_env {hsc_targets = []})
      updateHomeModCache
      pure orig_targets

    restore orig_targets = do
      storeHomeModCache
      hsc_env <- getSession
      setSession (hsc_env {hsc_targets = orig_targets})

    act1 _ = act0

-- Note: [Global HscEnv for plugin]
-- --------------------------------
--
-- When compiling with ghc plugin, FnkEnv is unwrapped with "toGhc" and "unGhc"
-- to perform the inner IO action. This way of invokation could not share the
-- FnkEnv when compiling multiple module, so reading from and writing to a
-- global MVar to pass around the "Session" to avoid redundant module compilation
-- when using home package modules during macro expansion.

-- | Wrapper to perform given action with global 'Session', to share the
-- underlying 'HscEnv' when compiling as ghc plugin.
withGlobalSession :: Fnk a -> Fnk a
withGlobalSession act0 = do
  fer <- Fnk pure
  fenv0 <- getFnkEnv
  orig_hsc_env <- getSession

  let tr = debugWhen' (hsc_dflags orig_hsc_env) fenv0 Fnk_trace_session
      prepare do_init = do
        hsc_env <- if do_init
          then initializeGlobalSession
          else getSession
        dumpHscEnv fenv0 "withGlobalSession (prepare)" hsc_env
        pure hsc_env

      restore hsc_env_orig = do
        hsc_env <- getSession
        dumpHscEnv fenv0 "withGlobalSession (restore):" hsc_env
        setSession hsc_env_orig

      act1 do_init = bracket (prepare do_init) restore $ \mex0 -> do
        let mex1 = discardInteractiveContext mex0
        setSession mex1
        modifyFnkEnv (\e -> e {envSessionForExpand = Just mex1})
        retval <- act0
        mex2 <- getSession
        modifyFnkEnv (\e -> e {envSessionForExpand = Just mex2})
        fnk_env <- getFnkEnv
        pure (retval, fnk_env)

  (retval, fnk_env) <- liftIO $ do
    modifyMVar globalSessionVar $ \mb_s0 -> do
      (do_init, s1@(Session r1)) <- case mb_s0 of
        Just s0 -> do
          tr ["withGlobalSession: global session already initialized"]
          pure (False, s0)
        Nothing -> do
          tr ["withGlobalSession: invoking newHscEnvForExpand"]
          new_hsc_env <- newHscEnvForExpand fenv0 orig_hsc_env
          r0 <- newIORef new_hsc_env
          pure (True, Session r0)
      (retval, fnk_env) <- unGhc (toGhc (act1 do_init) fer) s1
      for_ (envSessionForExpand fnk_env) $ \he ->
        atomicModifyIORef' r1 (const (he, ()))
      pure (Just s1, (retval, fnk_env))

  putFnkEnv fnk_env
  clearHomeModCache
  pure retval

-- | Clear the contents of global 'MVar' containing 'HscEnv' for macro
-- expansion.
clearGlobalSession :: IO ()
clearGlobalSession = modifyMVar globalSessionVar $ const $ pure (Nothing, ())
{-# INLINABLE clearGlobalSession #-}

initializeGlobalSession :: GhcMonad m => m HscEnv
initializeGlobalSession = do
#if MIN_VERSION_ghc(9,0,0)
  -- To set the "hsc_interp" field in the new session.
  _ <- getDynFlags >>= setSessionDynFlags
#endif
  getSession >>= initializePlugin'
{-# INLINABLE initializeGlobalSession #-}

-- Version compatible variant of 'initializePlugins'.
initializePlugin' :: MonadIO m => HscEnv -> m HscEnv
#if MIN_VERSION_ghc(9,2,0)
initializePlugin' = liftIO . initializePlugins
#else
initializePlugin' hsc_env = do
  plugin_dflags <- liftIO $ initializePlugins hsc_env (hsc_dflags hsc_env)
  return (updateDynFlags plugin_dflags hsc_env)
#endif
{-# INLINABLE initializePlugin' #-}

-- | Unsafe global 'MVar' to share the 'HscEnv' when compiling as plugin.
globalSessionVar :: MVar (Maybe Session)
globalSessionVar = unsafePerformIO (newMVar Nothing)
{-# NOINLINE globalSessionVar #-}

-- XXX: Workaround for passing state to recursively called "load'" function
-- defined in GHC driver. Modifying the "rawSettings" field in the DynFlags with
-- dummy String value, so that recursive call to the "load'" function can tell
-- whether current module is compiled for macro expansion or not.
--
-- The "parMakeCount" field update is a wokaround for concurrent build. Current
-- approach does not work with "-j" ghc option, which could cause race
-- conditions when multiple mudoles were requiring same home package module,
-- since the HscEnv is shared between all home package modules.

-- | Modify given 'DynFlags' as in macro expansion state.
setExpanding :: DynFlags -> DynFlags
setExpanding dflags0 =
  let raw_settings = rawSettings dflags0
#if MIN_VERSION_ghc(9,8,0)
      dflags1 = dflags0 {parMakeCount = Just (ParMakeThisMany 1)}
#else
      dflags1 = dflags0 {parMakeCount = Just 1}
#endif
      dflags2 = dflags1 {rawSettings = expandingKey : raw_settings}
  in  dflags2
{-# INLINABLE setExpanding #-}

-- | 'True' if given 'DynFlags' is in macro expansion state.
isExpanding :: DynFlags -> Bool
isExpanding = isJust . lookup (fst expandingKey) . rawSettings
{-# INLINABLE isExpanding #-}

-- | Internally used key value pair to mark macro expansion state.
expandingKey :: (String, String)
expandingKey = ("FNK_MEX", "1")
{-# INLINABLE expandingKey #-}

-- | Show expanding key.
showExpanding :: DynFlags -> String
showExpanding dflags =
  let keys = [ k <> "=" <> v
             | kv@(k, v) <- rawSettings dflags, kv == expandingKey]
  in  intercalate " " keys
{-# INLINABLE showExpanding #-}

removeWayDyn :: DynFlags -> DynFlags
#if MIN_VERSION_ghc(9,2,0)
removeWayDyn df = df {targetWays_ = removeDynFromWays (targetWays_ df)}
#else
removeWayDyn df = df {ways = removeDynFromWays (ways df)}
#endif
{-# INLINABLE removeWayDyn #-}

#if MIN_VERSION_ghc(9,0,0)
removeDynFromWays :: Set.Set Way -> Set.Set Way
removeDynFromWays = Set.filter (/= WayDyn)
#else
removeDynFromWays :: [Way] -> [Way]
removeDynFromWays = filter (/= WayDyn)
#endif
{-# INLINABLE removeDynFromWays #-}

-- | From `discardIC'.
discardInteractiveContext :: HscEnv -> HscEnv
#if MIN_VERSION_ghc(9,4,0)
discardInteractiveContext = discardIC
#else
discardInteractiveContext hsc_env =
  let dflags = hsc_dflags hsc_env
      empty_ic = emptyInteractiveContext dflags
      new_ic_monad = keep_external_name ic_monad
      old_ic = hsc_IC hsc_env
      keep_external_name ic_name =
        if nameIsFromExternalPackage this_pkg old_name
           then old_name
           else ic_name empty_ic
        where
         old_name = ic_name old_ic
#  if MIN_VERSION_ghc(9,2,0)
      this_pkg = hsc_home_unit hsc_env
#  elif MIN_VERSION_ghc(9,0,0)
      this_pkg = homeUnit dflags
#  else
      this_pkg = thisPackage dflags
#  endif
  in  hsc_env {hsc_IC = empty_ic {ic_monad = new_ic_monad}}
#endif
{-# INLINABLE discardInteractiveContext #-}

-- | Setup 'DynFlags' for interactive evaluation.
bcoDynFlags :: DynFlags -> DynFlags
-- XXX: See: 'GhcMake.enableCodeGenForUnboxedTupleOrSums'.
bcoDynFlags dflags0 =
  let dflags1 = dflags0 { ghcLink = LinkInMemory
#if MIN_VERSION_ghc(9,6,0)
                        , backend = interpreterBackend
#elif MIN_VERSION_ghc(9,2,0)
                        , backend = Interpreter
#else
                        , hscTarget = HscInterpreted
#endif
                        }
#if MIN_VERSION_ghc(9,6,0)
      -- See 'GHC.Driver.Main.hscMaybeWriteIface'. The function will panic if
      -- writing simple interface file with dyanmic-too option enabled. The
      -- simple interface is written from
      -- "GHC.Driver.Pipeline.Execute.runHscBackendPhase" if backend does not
      -- write files.
      --
      -- XXX: Not sure whether possible to have -dynamic-too with
      -- non-file-writing backend, confirm it.
      dflags2 | not (backendWritesFiles (backend dflags0))
              = unSetGeneralFlag' Opt_BuildDynamicToo dflags1
              | otherwise = dflags1
#else
      dflags2 = dflags1
#endif
#if MIN_VERSION_ghc(9,6,0)
      -- In ghc 9.6, seems like `Opt_ByteCode' is not in use any more.
      dflags3 = setGeneralFlag' Opt_UseBytecodeRatherThanObjects dflags2
#elif MIN_VERSION_ghc(9,2,0)
      dflags3 = setGeneralFlag' Opt_ByteCode dflags2
#elif MIN_VERSION_ghc(8,10,3)
      dflags3 = setGeneralFlag' Opt_ByteCodeIfUnboxed dflags2
#elif MIN_VERSION_ghc(8,10,1)
      dflags3 = setGeneralFlag' Opt_ByteCode dflags2
#else
      dflags3 = dflags2
#endif
      dflags4 = setGeneralFlag' Opt_IgnoreOptimChanges $
                setGeneralFlag' Opt_IgnoreHpcChanges $
                updOptLevel 0 dflags3
      -- XXX: Warning message for missing home package module is shown with
      -- -Wall option, suppressing for now ...
      dflags5 = wopt_unset dflags4 Opt_WarnMissingHomeModules
  in  dflags5
{-# INLINABLE bcoDynFlags #-}

interpHasNoWayDyn :: Bool
#if MIN_VERSION_ghc(9,0,0)
interpHasNoWayDyn = WayDyn `notElem` hostFullWays
#else
interpHasNoWayDyn = WayDyn `notElem` interpWays
#endif
{-# INLINABLE interpHasNoWayDyn #-}

-- | 'True' when the 'DynFlags' is using interpreter.
isInterpreted :: DynFlags -> Bool
#if MIN_VERSION_ghc(9,6,0)
-- As of ghc 9.6.2, interpreter backend is the only backend which can reuse
-- loaded code.
isInterpreted = backendCanReuseLoadedCode . backend
#elif MIN_VERSION_ghc(9,2,0)
isInterpreted dflags = backend dflags == Interpreter
#else
isInterpreted dflags = hscTarget dflags == HscInterpreted
#endif
{-# INLINABLE isInterpreted #-}
