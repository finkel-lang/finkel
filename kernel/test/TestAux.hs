{-# LANGUAGE CPP #-}
-- | Miscellaneous auxiliary codes for tests.
module TestAux
  ( ifUsingStack
  , initSessionForTest
  , whenUsingStack
  , resetPackageEnv
  ) where

-- base
import Control.Monad.IO.Class (MonadIO(..))
import System.Environment (lookupEnv)

-- ghc
import DynFlags (DynFlags(..), HasDynFlags(..))

-- sk-kernel
import Language.SK.SKC (Skc, setDynFlags)
import Language.SK.Make (initSessionForMake)

-- | Perform the first action when invoked from @stack@, other wise
-- perform the second.
ifUsingStack :: MonadIO m => m a -> m a -> m a
ifUsingStack stackAct otherAct = do
  mb_ghc_pkg_path <- liftIO (lookupEnv "GHC_PACKAGE_PATH")
  case mb_ghc_pkg_path of
    Just _  -> stackAct
    Nothing -> otherAct

-- | Variant of 'ifStack' doing @return ()@ as non-stack action.
whenUsingStack :: MonadIO m => m () -> m ()
whenUsingStack act = ifUsingStack act (return ())

-- | Reset package env in DynFlags by setting with \"-\".
--
-- When running tests with stack, explicitly specify "-" as package env
-- to avoid using ".ghc.environment.xxx" files for preserving package
-- environment. This need to be done before 'initSessionForMake'.
resetPackageEnv :: Skc ()
#if MIN_VERSION_ghc (8,4,4)
resetPackageEnv = do
  -- Use of "-" to reset package env is NOT supported until 8.4.4.
  dflags <- getDynFlags
  setDynFlags dflags {packageEnv = Just "-"}
#else
resetPackageEnv = return ()
#endif

-- | Initialize session with 'initSessionForMake', then reset package
-- env when invoked from stack.
initSessionForTest :: Skc ()
initSessionForTest = do
  whenUsingStack resetPackageEnv
  initSessionForMake
