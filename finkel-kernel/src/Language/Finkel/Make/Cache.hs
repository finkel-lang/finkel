{-# LANGUAGE CPP #-}
-- | Module for managing home module cache.
--
-- When compiling a module containing nested :require form, the required module
-- is expanded twice, once when pre-processing and again when compiling byte
-- code. This module contains functions for caching the home module to avoid
-- redundant recompilation.

module Language.Finkel.Make.Cache
  ( storeHomeModCache
  , updateHomeModCache
  , clearHomeModCache
  ) where

#if MIN_VERSION_ghc(9,4,0)

#include "ghc_modules.h"

-- base
import Control.Monad.IO.Class (MonadIO (..))
import Data.IORef             (IORef, atomicModifyIORef', newIORef, readIORef)
import System.IO.Unsafe       (unsafePerformIO)

-- ghc
import GHC_Driver_Make        (ModIfaceCache (..), newIfaceCache)

-- Internal
import Language.Finkel.Fnk    (Fnk (..), FnkEnv (..), getFnkEnv, modifyFnkEnv)


-- ------------------------------------------------------------------------
-- Exported
-- ------------------------------------------------------------------------

-- XXX: Unfortunately, home module caching is not working when compiled without
-- @-dynamic@ or @-dynamic-too@ option.
--
-- Seems like, when compiling byte code target, the module is always force
-- compiled.

-- | Store current 'HomeModCache' to private global reference.
storeHomeModCache :: Fnk ()
storeHomeModCache = do
  mb_mic <- fmap envInterpModIfaceCache getFnkEnv
  liftIO $ do
    new_ifc <- case mb_mic of
      -- XXX: Copy the contents of ModIfaceCache?
      Just mic -> pure $ HomeModCache {ifc_mic = mic}
      Nothing  -> newHomeModCache
    atomicModifyIORef' unsafeHomeModCacheRef $ \_ifc -> (new_ifc, ())
{-# INLINABLE storeHomeModCache #-}

-- | Update 'HomeModCache' in current 'FnkEnv'.
updateHomeModCache :: Fnk ()
updateHomeModCache = do
  HomeModCache {ifc_mic = mic} <- liftIO $ readIORef unsafeHomeModCacheRef
  modifyFnkEnv (\fnk_env -> fnk_env {envInterpModIfaceCache = Just mic})
{-# INLINABLE updateHomeModCache #-}

-- | Clear 'HomeModCache' in privarte global reference.
clearHomeModCache :: MonadIO m => m ()
clearHomeModCache = liftIO $ do
  mic <- newIfaceCache
  atomicModifyIORef' unsafeHomeModCacheRef $ \ifc -> (ifc {ifc_mic = mic}, ())
{-# INLINABLE clearHomeModCache #-}


-- ------------------------------------------------------------------------
-- Internal
-- ------------------------------------------------------------------------

-- | Data type to store home module cache passed from pre-process phase.
newtype HomeModCache = HomeModCache { ifc_mic :: ModIfaceCache }

newHomeModCache :: IO HomeModCache
newHomeModCache = fmap HomeModCache newIfaceCache
{-# INLINABLE newHomeModCache #-}

-- | Unsafe global IORef to share home module information from pre-process phase
-- to hsc phase.
unsafeHomeModCacheRef :: IORef HomeModCache
unsafeHomeModCacheRef = unsafePerformIO $ do
  hmc <- newHomeModCache
  newIORef hmc
{-# NOINLINE unsafeHomeModCacheRef #-}

#else /* ghc < 9.4 */

-- ModIfaceCache does not exist in ghc < 9.4. Do nothing with dummy functions.

storeHomeModCache :: Monad m => m ()
storeHomeModCache = pure ()

updateHomeModCache :: Monad m => m ()
updateHomeModCache = pure ()

clearHomeModCache :: Monad m => m ()
clearHomeModCache = pure ()

#endif /* ghc < 9.4 */
