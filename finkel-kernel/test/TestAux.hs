{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE CPP              #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE TypeApplications #-}
-- | Miscellaneous auxiliary codes for tests.
module TestAux
  ( FnkSpec
  , FnkTestResource(..)
  , getFnkTestResource
  , initSessionForTest
  , evalWith
  , mkIIDecl
  , parseAndSetDynFlags
  , removeArtifacts
  , fnkTestEnv
  , getTestFiles
  , beforeAllWith
  , quietly
  ) where

#include "ghc_modules.h"

-- base
import           Control.Exception       (catch, fromException, throw, throwIO)
import           Control.Monad           (when)
import           Control.Monad.IO.Class  (MonadIO (..))
import           Data.List               (isSubsequenceOf, sort)
import           Data.Maybe              (fromMaybe)
import           Data.Version            (showVersion)
import           System.Environment      (getExecutablePath, lookupEnv,
                                          withArgs)
import           System.Exit             (ExitCode (..))
import           System.IO               (stderr, stdout)

#if !MIN_VERSION_hspec(2,7,6)
import           Control.Concurrent      (MVar, modifyMVar, newMVar)
import           Control.Exception       (SomeException, try)
#endif

-- directory
import           System.Directory        (doesFileExist, getDirectoryContents,
                                          removeFile)

-- filepath
import           System.FilePath         (joinPath, takeExtension, (<.>), (</>))

-- ghc
import           GHC                     (setContext)
import           GHC_Data_FastString     (fsLit)
import           GHC_Data_StringBuffer   (StringBuffer)
import           GHC_Driver_Session      (DynFlags (..), HasDynFlags (..),
                                          parseDynamicFlagsCmdLine)
import           GHC_Runtime_Context     (InteractiveImport (..))
import           GHC_Hs_ImpExp           (simpleImportDecl)
import           GHC_Runtime_Eval        (getContext)
import           GHC_Types_Basic         (SuccessFlag)
import           GHC_Types_SrcLoc        (noLoc)
import           GHC_Unit_Module         (mkModuleNameFS)

-- hspec
import           Test.Hspec              (SpecWith)

#if MIN_VERSION_hspec(2,7,6)
import           Test.Hspec              (beforeAllWith)
#else
import           Test.Hspec              (beforeWith, runIO)
#endif

-- process
import           System.Process          (readProcess)

-- silently
import           System.IO.Silently      (hSilence)

-- fnk-kernel
import           Language.Finkel         (defaultFnkEnv)
import           Language.Finkel.Builder (Builder)
import           Language.Finkel.Expand  (expands, withExpanderSettings)
import           Language.Finkel.Fnk     (Fnk, FnkEnv (..), setDynFlags)
import           Language.Finkel.Lexer   (evalSP)
import           Language.Finkel.Main    (defaultMain)
import           Language.Finkel.Make    (buildHsSyn, initSessionForMake, make)
import           Language.Finkel.Reader  (sexprs)
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

-- | Type synonym for hspec test taking 'FnkTestResource'.
type FnkSpec = SpecWith FnkTestResource

-- | Test resource for finkel-kernel package tests.
data FnkTestResource =
  FnkTestResource
    { ftr_main :: [String] -> IO ()
    -- ^ Function to run 'defaultMain'.
    , ftr_init :: Fnk ()
    -- ^ Initialization action inside 'Fnk'.
    , ftr_load :: [FilePath] -> Fnk SuccessFlag
    -- ^ Function to load a module.
    -- , ftr_eval :: forall a b. String -> Builder a -> (a -> Fnk b)
    --            -> StringBuffer -> Fnk b
    -- -- ^ Function to evaluate an expression string, returns a string
    -- -- representation of evaluated result.
    }

getFnkTestResource :: IO FnkTestResource
getFnkTestResource = do
  pkg_args <- getPackageArgs
  return (FnkTestResource { ftr_main = makeMain pkg_args
                          , ftr_init = makeInit pkg_args
                          , ftr_load = makeLoad
                          })

makeMain :: [String] -> [String] -> IO ()
makeMain pkg_args other_args =
  catch (withArgs (pkg_args ++ other_args) (quietly defaultMain))
        (\e -> case fromException e of
            Just ExitSuccess -> return ()
            _                -> throw e)

-- XXX: Ignoring all messages, including messages for reporting error.
-- Might be better to implement an option to redirect log outputs.
quietly :: IO a -> IO a
quietly = hSilence [stderr, stdout]

makeInit :: [String] -> Fnk ()
makeInit pkg_args = resetPackageEnv pkg_args >> initSessionForMake

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
     snapshotdb <- getSnapshotDb

     let inplacedbs =
           if has_inplacepkgconf
              then [ "-package-db", inplacedb
                   , "-package-id", inplacepkg ]
              else [ "-package-db", inplacedb ]
         args = [ "-clear-package-db"
                , "-global-package-db"

                -- For overloaded label test which imports `GHC.Types' module.
                , "-package", "ghc-prim"
                ] ++ inplacedbs ++ snapshotdb

     return args

-- Note: [Snapthos package database for stack]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- In ghc 9.0.1, the "exceptions" package has been added to the bundled packages
-- shipped with ghc, to make "Ghc" monad as an instance of the type classes
-- defined in "Control.Monad.Catch". Finkel followed this change and made "Fnk"
-- as an instance of the type classes defined in "Control.Monad.Catch", and back
-- ported the change. When running the stack with ghc version prior to 9.0.1,
-- the "exceptions" package is installed in non-bundled package database. Thus,
-- getting the snapshot package database with "stack path
-- --snapshot-pkg-db".

getSnapshotDb :: IO [String]
getSnapshotDb = do
   me <- getExecutablePath
   let is_stack = ".stack" `isSubsequenceOf` me
   if is_stack
      then do
         mb_resolver <- lookupEnv "RESOLVER"
         let resolver = fromMaybe "lts-16" mb_resolver
         ret <- readProcess "stack" ["--resolver=" ++ resolver
                                    ,"path", "--snapshot-pkg-db"]
                                    ""
         let snapshot_db = filter (not . null) (lines ret)
         return ("-package-db" : snapshot_db)
      else return []

-- | Reset package environment to support running the test with cabal-install.
resetPackageEnv :: [String] -> Fnk ()
resetPackageEnv pkg_args = do
  dflags0 <- getDynFlags
  let largs = map noLoc pkg_args
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

parseAndSetDynFlags :: [String] -> Fnk ()
parseAndSetDynFlags args = do
  dflags0 <- getDynFlags
  (dflags1,_,_) <- parseDynamicFlagsCmdLine dflags0 (map noLoc args)
  setDynFlags dflags1

makeLoad :: [FilePath] -> Fnk SuccessFlag
makeLoad files = make (map (\p -> (noLoc p, Nothing)) files) False Nothing

evalWith ::  String -> Builder a -> (a -> Fnk b) -> StringBuffer -> Fnk b
evalWith !label !parser !act !input = do
  case evalSP sexprs (Just label) input of
    Right form0 -> do
      !form1 <- withExpanderSettings (prepare >> expands form0)
      !hthing <- buildHsSyn parser form1
      act hthing
    Left err -> liftIO (throwIO err)
  where
    -- Adding 'Prelude' and 'Language.Finkel' to interactive context, since the
    -- codes in the file does not contain ':require' forms.
    prepare = do
      ctxt <- getContext
      setContext (mkIIDecl "Prelude" : mkIIDecl "Language.Finkel" : ctxt)
    -- mkII = IIDecl . simpleImportDecl . mkModuleNameFS . fsLit

mkIIDecl :: String -> InteractiveImport
mkIIDecl = IIDecl . simpleImportDecl . mkModuleNameFS . fsLit

-- | Reset package env and initialize session with 'initSessionForMake'.
initSessionForTest :: Fnk ()
initSessionForTest = liftIO getPackageArgs >>= makeInit

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

-- | The 'FnkEnv' used for test. Has 'envLibDir' field from CPP header file.
fnkTestEnv :: FnkEnv
fnkTestEnv = defaultFnkEnv {envLibDir = Just FINKEL_KERNEL_LIBDIR}

-- | Get files under test data directory.
getTestFiles :: String -- ^ Name of the sub directory under test data directory.
             -> IO [FilePath]
getTestFiles name =
  let dir = "test" </> "data" </> name
      f x acc = if takeExtension x == ".fnk"
                  then (dir </> x) : acc
                  else acc
      files = getDirectoryContents dir
  in  sort . foldr f [] <$> files

#if !MIN_VERSION_hspec(2,7,6)
-- "Test.Hspec.Core.Hooks.beforeAllWith" did not exist.
beforeAllWith :: (b -> IO a) -> SpecWith a -> SpecWith b
beforeAllWith action spec = do
  mvar <- runIO (newMVar Nothing)
  beforeWith (memoize mvar . action) spec

memoize :: MVar (Maybe a) -> IO a -> IO a
memoize mvar action = do
  et_result <- modifyMVar mvar $ \mb_val -> do
    case mb_val of
      Nothing -> do
        et_val <- try @ SomeException action
        case et_val of
          Left err  -> return (Nothing, Left err)
          Right val -> return (Just val, Right val)
      Just val -> return (Just val, Right val)
  either throwIO return et_result
#endif
