{-# LANGUAGE CPP #-}
-- | Miscellaneous auxiliary codes for tests.
module TestAux
  ( ifUsingStack
  , initSessionForTest
  , whenUsingStack
  , removeArtifacts
  , runDefaultMain
  ) where

-- base
import           Control.Exception      (catch, throw)
import           Control.Monad          (when)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Version           (showVersion)
import           System.Environment     (lookupEnv)
import           System.Environment     (withArgs)
import           System.Exit            (ExitCode (..))


-- directory
import           System.Directory       (getDirectoryContents, getHomeDirectory,
                                         removeFile)

-- filepath
import           System.FilePath        (joinPath, takeExtension, (</>))

-- ghc
import           Config                 (cProjectVersion)
import           DynFlags               (DynFlags (..), HasDynFlags (..),
                                         parseDynamicFlagsCmdLine)
import           SrcLoc                 (noLoc)

-- fnk-kernel
import           Language.Finkel.Fnk    (Fnk, setDynFlags)
import           Language.Finkel.Main   (defaultMain)
import           Language.Finkel.Make   (initSessionForMake)
import qualified Paths_finkel_kernel


-- -----------------------------------------------------------------------
--
-- Configured values from setup script
--
-- -----------------------------------------------------------------------

#include "finkel_kernel_config.h"

distpref :: FilePath
#ifdef FINKEL_KERNEL_CONFIG_DISTPREF
distpref = FINKEL_KERNEL_CONFIG_DISTPREF
#else
distpref = error "FINKEL_KERNEL_CONFIG_DISTPREF not defined"
#endif


-- -----------------------------------------------------------------------
--
-- Auxiliary functions
--
-- -----------------------------------------------------------------------

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
resetPackageEnvForStack :: Fnk ()
resetPackageEnvForStack = do
  dflags0 <- getDynFlags
  args <- map noLoc <$> liftIO getPackageArgsForStack
  (dflags1, _, _) <- parseDynamicFlagsCmdLine dflags0 args
  setDynFlags (clearPackageEnv dflags1)

getPackageArgsForStack :: IO [String]
getPackageArgsForStack =
#if MIN_VERSION_ghc(8,4,4)
  return []
#else
  return [ "-package", "finkel-kernel"
         , "-package", "ghc-prim"
         , "-package", "array"
         , "-package", "containers"]
#endif

getPackageArgsForCabal :: IO [String]
getPackageArgsForCabal =
  do home <- getHomeDirectory
     let storedb = joinPath [home, ".cabal", "store", ghc_ver, "package.db"]
         ghc_ver = "ghc-" ++ cProjectVersion

         -- To support running the test without building the package, using the
         -- package db found in "package.conf.inplace" directory for inplace
         -- package db.
         --
         -- There is a "dist-newstyle/packagedb" directory for holding package
         -- data of project local packages, but the package db file will be
         -- written only after running the "cabal v2-build" command once, which
         -- means that running "cabal v2-test" will fail if "v2-build" were not
         -- invoked in advance.
         inplacedb = joinPath [distpref, "package.conf.inplace"]
         fkv = showVersion Paths_finkel_kernel.version
         inplacepkg = "finkel-kernel-" ++ fkv ++ "-inplace"

         args = [ "-clear-package-db"
                , "-global-package-db"
                , "-package-db", storedb
                , "-package-db", inplacedb
                , "-package-id", inplacepkg ]
     return args

-- | Get package related command line arguments for 'DynFlags'.
getPackageArgs :: IO [String]
getPackageArgs = ifUsingStack getPackageArgsForStack  getPackageArgsForCabal

-- | Reset package environment to support running the test with
-- cabal-install.
resetPackageEnvForCabal :: Fnk ()
resetPackageEnvForCabal = do
  dflags0 <- getDynFlags
  args <- liftIO getPackageArgsForCabal
  let largs = map noLoc args
      dflags1 = clearPackageEnv dflags0
  (dflags2, _, _) <- parseDynamicFlagsCmdLine dflags1 largs
  setDynFlags dflags2

-- | Clear 'packageEnv' field in 'DynFlags'.
clearPackageEnv :: DynFlags -> DynFlags
-- Use of "-" to reset package env is NOT supported until 8.4.4.
#if MIN_VERSION_ghc(8,4,4)
clearPackageEnv dflags = dflags {packageEnv = Just "-"}
#else
clearPackageEnv dflags = dflags {packageEnv = Nothing}
#endif

-- | Initialize session with 'initSessionForMake', then reset package
-- env when invoked from stack.
initSessionForTest :: Fnk ()
initSessionForTest = do
  ifUsingStack resetPackageEnvForStack resetPackageEnvForCabal
  initSessionForMake

-- | Remove compiled artifacts, such as @.o@ and @.hi@ files.
removeArtifacts :: FilePath -> IO ()
removeArtifacts dir = do
  contents <- getDirectoryContents dir
  mapM_ removeObjAndHi contents
  where
    removeObjAndHi file =
      when (takeExtension file `elem` [".o", ".hi", ".p_o", ".p_hi"])
           (removeFile (dir </> file))

-- | Wrapper function to run 'Language.Finkel.Main.defaultMain'.
runDefaultMain
  :: [String]
  -- ^ Command line arguments.
  -> IO ()
runDefaultMain args =
  -- "defaultMain" uses "System.Process.rawSystem" to delegate non-finkel
  -- related works to ghc, which throws "ExitSuccess" when successfully done.
  do pkgargs <- getPackageArgs
     catch (withArgs (pkgargs ++ args) defaultMain)
           (\e -> case e of
                    ExitSuccess -> return ()
                    _           -> throw e)
