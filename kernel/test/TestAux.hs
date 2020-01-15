{-# LANGUAGE CPP #-}
-- | Miscellaneous auxiliary codes for tests.
module TestAux
  ( ifUsingStack
  , initSessionForTest
  , whenUsingStack
  , removeArtifacts
  , resetPackageEnv
  ) where

-- base
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import System.Environment (lookupEnv)

-- directory
import System.Directory (getDirectoryContents, removeFile)

-- filepath
import System.FilePath ((</>), takeExtension)

-- ghc
import DynFlags (DynFlags(..), HasDynFlags(..))
#if !MIN_VERSION_ghc (8,4,4)
import DynFlags (parseDynamicFlagsCmdLine)
import SrcLoc (noLoc)
#endif

-- fnk-kernel
import Language.Finkel.Fnk (Fnk, setDynFlags)
import Language.Finkel.Make (initSessionForMake)

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
resetPackageEnv :: Fnk ()
#if MIN_VERSION_ghc (8,4,4)
resetPackageEnv = do
  -- Use of "-" to reset package env is NOT supported until 8.4.4.
  dflags <- getDynFlags
  setDynFlags dflags {packageEnv = Just "-"}
#else
resetPackageEnv = do
  -- Seems like package environment does not work well with stack
  -- version 2.1. Manually adding packages used in test codes.
  dflags0 <- getDynFlags
  let dflags1 = dflags0 { packageEnv = Nothing }
      flagstrs = map noLoc ["-package", "finkel-kernel"
                           ,"-package", "ghc-prim"
                           ,"-package", "array"
                           ,"-package", "containers"]
  (dflags2, _, _) <- parseDynamicFlagsCmdLine dflags1 flagstrs
  setDynFlags dflags2
#endif

-- | Initialize session with 'initSessionForMake', then reset package
-- env when invoked from stack.
initSessionForTest :: Fnk ()
initSessionForTest = do
  whenUsingStack resetPackageEnv
  initSessionForMake

removeArtifacts :: FilePath -> IO ()
removeArtifacts dir = do
  contents <- getDirectoryContents dir
  mapM_ removeObjAndHi contents
  where
    removeObjAndHi file =
      when (takeExtension file `elem` [".o", ".hi", ".p_o", ".p_hi"])
           (removeFile (dir </> file))
