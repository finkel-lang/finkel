{-# LANGUAGE CPP #-}
-- | Tests for 'make'.
module MakeTest
  ( makeTests
  ) where

-- base
import Control.Monad                (when)
import Control.Monad.IO.Class       (MonadIO (..))
import Data.List                    (isPrefixOf, tails)
import System.FilePath              (takeBaseName, (</>))
import System.Info                  (os)

-- directory
import System.Directory             (getDirectoryContents)

-- filepath
import System.FilePath              (takeExtension)

-- ghc
import Config                       (cProjectVersionInt)
import DynFlags                     (DynFlags, HasDynFlags (..), Way (..),
                                     interpWays)
import FastString                   (fsLit)
import Module                       (componentIdToInstalledUnitId)
import Outputable                   (Outputable (..), showPpr, showSDoc)
import Packages                     (InstalledPackageInfo (..), PackageConfig,
                                     PackageName (..), lookupInstalledPackage,
                                     lookupPackageName, pprPackageConfig)

#if !MIN_VERSION_ghc(8,10,0)
import GhcMonad                     (GhcMonad (..))
import Linker                       (unload)
#endif

-- hspec
import Test.Hspec

-- finkel-kernel
import Language.Finkel.Fnk
import Language.Finkel.Lexer
import Language.Finkel.SpecialForms
import Language.Finkel.TargetSource

-- Internal
import TestAux

makeTests :: Spec
makeTests = beforeAll_ (removeArtifacts odir) $ do
  targetSourceTests

  -- Build bytecode
  buildBytecode "main1.fnk"
  buildBytecode "main2.fnk"
  buildBytecode "main3.fnk"
  buildByteCodeWith ["--fnk-dump-hs", "--fnk-debug", "-v2"] "main4.fnk"
  buildBytecode "main5.fnk"
  buildBytecode "main9.fnk"

  -- Build object codes
  buildC (odir </> "cbits1.c")
  buildObj ["-fforce-recomp", "-ddump-parsed", "-ddump-parsed-ast"
           ,"-dsource-stats"]
           ["main5.fnk"]
  buildObj [] ["cbits1.c", "cbits2.c", "cbits3.c", "main6.fnk"]
  buildObj [] (map (odir </>) ["cbits1.o","cbits2.o","cbits3.o"] ++
               ["main6.fnk"])
  buildObj [] ["main6.fnk", "cbits1.c", "cbits2.c", "cbits3.c"]
  buildObj [] ["M4.A"]
  buildObj ["--fnk-dump-hs", "--fnk-hsdir=" ++ (odir </> "gen")]
           ["M5", "M4" </> "A.fnk", "M4" </> "B.fnk", "M4", "main7.fnk"]
  buildObj ["-O2"] ["main8.fnk"]

  -- XXX: Disabled under Windows ...
  let buildObj' flags inputs =
        before_ (doUnload >> removeArtifacts odir) (buildObj flags inputs)

  -- Compile object codes with and without optimization option
  buildObj' [] ["P1", "P2"]
  buildObj' ["-O1"] ["P1", "P2"]

  -- Compile object code with and without optimization, module reorderd
  buildObj' [] ["P2", "P1"]
  buildObj' ["-O1"] ["P2", "P1"]

#if !defined(mingw32_HOST_OS)
  -- Compile object codes with dynamic-too and optimization option
  buildObj' ["-O0", "-dynamic-too"] ["P1", "P2"]
  buildObj' ["-O1", "-dynamic-too"] ["P1", "P2"]

  -- Compile object codes with dynamic-too and optimization option, reorderd
  buildObj' ["-O0", "-dynamic-too"] ["P2", "P1"]
  buildObj' ["-O1", "-dynamic-too"] ["P2", "P1"]
#endif

  -- Compile object codes with profiling option
  has_profiling_obj <- runIO hasProfilingObj
  when has_profiling_obj $ do
    buildObj' ["-O","-prof", "-osuf", "p_o", "-hisuf", "p_hi"] ["P1", "P2"]
    buildObj' ["-O","-prof", "-osuf", "p_o", "-hisuf", "p_hi"] ["P1", "P2"]

  -- Errors
  buildFilesNG [] ["E01"]

-- Action to unload package libraries
--
-- Until ghc 8.10, persistent linker state is stored in a global variable.  The
-- loaded package libraries are shared in "HscEnv.hsc_dynLinker" in every Fnk
-- run, which may cause link time error with dynamic object on some platforms.
-- To avoid such link time error, invoking "Linker.unload" before running the
-- test containing macro expansion.

-- Unload old objects.
doUnload :: IO ()
#if MIN_VERSION_ghc(8,10,0)
-- Persistent linker state is isolated, does nothing.
doUnload = return ()
#else
-- Persistent linker state is a global IORef.
doUnload = runFnk (getSession >>= liftIO . flip unload []) defaultFnkEnv
#endif

-- Action to decide whether profiling objects for the "finkel-kernel" package is
-- available at runtime.
hasProfilingObj :: IO Bool
hasProfilingObj = runFnk (hasProfilingObj' pkg_name) defaultFnkEnv
  where
    pkg_name = PackageName (fsLit "finkel-kernel")

hasProfilingObj' :: PackageName -> Fnk Bool
hasProfilingObj' pkg_name  =
  do initSessionForTest
     dflags <- getDynFlags
     case lookupPackageConfig dflags pkg_name of
       Nothing  -> return False
       Just cfg -> liftIO (do putStrLn (showSDoc dflags (pprPackageConfig cfg))
                              lookupProfObjInDirectories (libraryDirs cfg))

lookupPackageConfig :: DynFlags -> PackageName -> Maybe PackageConfig
lookupPackageConfig dflags pkg_name = do
  cmpid <- lookupPackageName dflags pkg_name
  lookupInstalledPackage dflags (componentIdToInstalledUnitId cmpid)

lookupProfObjInDirectories :: [FilePath] -> IO Bool
lookupProfObjInDirectories =
  let go (dir:dirs) = do
         files <- getDirectoryContents dir
         if any (\file -> ".p_o" == takeExtension file) files
            then return True
            else go dirs
      go [] = return False
  in  go

fnksrc1, hssrc1, othersrc1 :: TargetSource
fnksrc1 = FnkSource "path1" "Foo" [] (initialSPState (fsLit "dummy") 1 1)
hssrc1 = HsSource "path2"
othersrc1 = OtherSource "path3"

subseq :: Eq a => [a] -> [a] -> Bool
subseq xs ys = any (isPrefixOf xs) (tails ys)

targetSourceTests :: Spec
targetSourceTests =
  describe "TargetSource" $ do
    showTargetTest
    pprTargetTest
    asModuleNameTest

showTargetTest :: Spec
showTargetTest = do
  describe "show TargetSource" $
    it "should contain filepath" $ do
      show fnksrc1 `shouldSatisfy` subseq "path1"
      show hssrc1 `shouldSatisfy` subseq "path2"
      show othersrc1 `shouldSatisfy` subseq "path3"

asModuleNameTest :: Spec
asModuleNameTest =
  describe "asModuleName" $
    it "should replace path separators" $
      asModuleName ("Foo" </> "Bar" </> "Buzz.fnk") `shouldBe` "Foo.Bar.Buzz"

runOutputable :: (MonadIO m, Outputable a) => a -> m String
runOutputable obj =
  liftIO $ runFnk (flip showPpr obj <$> getDynFlags) defaultFnkEnv

pprTargetTest :: Spec
pprTargetTest =
  describe "ppr TargetSource" $
    it "should contain filepath" $ do
      let t target path = do str <- runOutputable target
                             str `shouldSatisfy` subseq path
      t fnksrc1 "path1"
      t hssrc1 "path2"
      t othersrc1 "path3"

buildBytecode :: FilePath -> Spec
buildBytecode = buildByteCodeWith []

buildByteCodeWith :: [String] -> FilePath -> Spec
buildByteCodeWith extra file =
  buildFiles (["-no-link", "-fbyte-code"] ++ extra) [file]

buildC :: FilePath -> Spec
buildC file = buildFiles ["-no-link"] [file]

buildObj :: [String] -> [FilePath] -> Spec
buildObj = buildFiles

buildFiles :: [String] -> [FilePath] -> Spec
buildFiles pre inputs =
  describe (labelWithOptionsAndFiles pre inputs) $
    it "should compile successfully" (buildWork pre inputs)

buildFilesNG :: [String] -> [FilePath] -> Spec
buildFilesNG pre inputs =
  describe (labelWithOptionsAndFiles pre inputs) $
    it "should throw an exception"
       (buildWork pre inputs `shouldThrow` anyException)

labelWithOptionsAndFiles :: [String] -> [FilePath] -> String
labelWithOptionsAndFiles pre inputs  =
  "file " ++ show inputs ++
  if null pre
     then ""
     else " with " ++ unwords pre

buildWork :: [String] -> [FilePath] -> Expectation
buildWork pre inputs
  | cProjectVersionInt == "810"
  , os == "mingw32"
  , any (\path -> takeBaseName path `elem` skipped) inputs
  = pendingWith "Not yet supported"
  | otherwise
  = do_work
  where
    skipped = ["main4"]
    do_work
       | WayProf `elem` interpWays = do_prof_work
       | otherwise = do_work_with []
    -- Use dflags setttings for profile when running test executable with "+RTS
    -- -p" option.
    do_prof_work =
      do_work_with [ "-prof", "-fprof-auto", "-fprof-cafs"
                   , "-hisuf", "p_hi", "-osuf", "p_o" ]
    do_work_with extra =
      runDefaultMain (extra ++ common_args ++ pre)
    common_args =
      ["-i.", "-i" ++ odir, "-v0"] ++ inputs

odir :: FilePath
odir = "test" </> "data" </> "make"
