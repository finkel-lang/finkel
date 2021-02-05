{-# LANGUAGE CPP #-}
-- | Miscellaneous auxiliary codes for tests.
module TestAux
  ( initSessionForTest
  , removeArtifacts
  , runDefaultMain
  , fnkTestEnv
  ) where

-- base
import           Control.Exception      (catch, fromException, throw)
import           Control.Monad          (when)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Version           (showVersion)
import           System.Environment     (withArgs)
import           System.Exit            (ExitCode (..))


-- directory
import           System.Directory       (doesFileExist, getDirectoryContents,
                                         removeFile)

-- filepath
import           System.FilePath        (joinPath, takeExtension, (<.>), (</>))

-- ghc
import           DynFlags               (DynFlags (..), HasDynFlags (..),
                                         parseDynamicFlagsCmdLine)
import           SrcLoc                 (noLoc)

-- fnk-kernel
import           Language.Finkel        (defaultFnkEnv)
import           Language.Finkel.Fnk    (Fnk, FnkEnv (..), setDynFlags)
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

getPackageArgs :: IO [String]
getPackageArgs =
  -- To support running the test without building the package, using the package
  -- db found in "package.conf.inplace" directory for inplace package db.
  --
  -- There is a "dist-newstyle/packagedb" directory for holding package data of
  -- project local packages, but the package db file will be written only after
  -- running the "cabal v2-build" command once, which means that running "cabal
  -- v2-test" will fail if "v2-build" were not invoked in advance.
  --
  -- The "inpkacepkgconf" is to support cabal-install v2-build, which uses
  -- "PKGNAME-X.Y.Z-inplace" format for inplace package.
  do let inplacedb = joinPath [distpref, "package.conf.inplace"]
         fkv = showVersion Paths_finkel_kernel.version
         inplacepkg = "finkel-kernel-" ++ fkv ++ "-inplace"
         inplacepkgconf = inplacedb </> inplacepkg <.> "conf"

     has_inplacepkgconf <- doesFileExist inplacepkgconf

     let args = [ "-clear-package-db"
                , "-global-package-db"

                -- For overloaded label test which imports `GHC.Types' module.
                , "-package", "ghc-prim"
                ] ++
                if has_inplacepkgconf
                   then [ "-package-db", inplacedb
                        , "-package-id", inplacepkg ]
                   else [ "-package-db", inplacedb ]

     return args

-- | Reset package environment to support running the test with cabal-install.
resetPackageEnvForCabal :: Fnk ()
resetPackageEnvForCabal = do
  dflags0 <- getDynFlags
  args <- liftIO getPackageArgs
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

-- | Reset package env and initialize session with 'initSessionForMake'.
initSessionForTest :: Fnk ()
initSessionForTest = do
  resetPackageEnvForCabal
  initSessionForMake

-- | Remove compiled artifacts, such as @.o@ and @.hi@ files.
removeArtifacts :: FilePath -> IO ()
removeArtifacts dir = do
  contents <- getDirectoryContents dir
  mapM_ removeObjAndHi contents
  where
    removeObjAndHi file =
      when (takeExtension file `elem` [ ".o", ".hi"
                                      , ".p_o", ".p_hi"
                                      , ".dyn_o", ".dyn_hi" ])
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
           (\e -> case fromException e of
               Just ExitSuccess -> return ()
               _                -> print e >> throw e)

-- | The 'FnkEnv' used for test. Has 'envLibDir' field from CPP header file.
fnkTestEnv :: FnkEnv
fnkTestEnv = defaultFnkEnv {envLibDir = Just FINKEL_KERNEL_LIBDIR}
