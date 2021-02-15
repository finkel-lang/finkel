{-# LANGUAGE CPP       #-}
{-# LANGUAGE MagicHash #-}
-- | Tests for 'make'.
module MakeTest
  ( makeTests
  , makeFnkTests
  ) where

#include "ghc_modules.h"

-- base
import Control.Exception      (SomeException (..))
import Control.Monad          (unless, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.List              (isPrefixOf, tails)
import Data.Maybe             (isJust)
import GHC.Exts               (unsafeCoerce#)
import System.Environment     (lookupEnv)
import System.Info            (os)

-- directory
import System.Directory       (copyFile, createDirectoryIfMissing,
                               doesFileExist, getTemporaryDirectory,
                               removeDirectoryRecursive)
#if !MIN_VERSION_ghc(9,0,0)
import System.Directory       (getDirectoryContents)
#endif

-- filepath
import System.FilePath        ((<.>), (</>))
#if !MIN_VERSION_ghc(9,0,0)
import System.FilePath        (takeExtension)
#endif

-- ghc
import GHC                    (setTargets)
import GHC_Data_FastString    (fsLit)
import GHC_Driver_Monad       (GhcMonad (..))
import GHC_Driver_Session     (HasDynFlags (..), parseDynamicFlagsCmdLine)
#if !MIN_VERSION_ghc(9,0,0)
import GHC_Driver_Session     (DynFlags (..))
#endif
import GHC_Driver_Types       (Target (..), TargetId (..))
import GHC_Runtime_Linker     (unload)
import GHC_Types_SrcLoc       (noLoc)
import GHC_Unit_Module        (mkModuleName)

import GHC_Unit_State         (PackageName (..))
#if !MIN_VERSION_ghc(9,0,0)
import GHC_Unit_State         (lookupPackageName)
#endif

import GHC_Utils_Outputable   (Outputable (..), showPpr)
#if !MIN_VERSION_ghc(9,0,0)
import GHC_Utils_Outputable   (showSDoc)
#endif

#if MIN_VERSION_ghc(9,0,0)
import GHC.Driver.Ways        (hostIsDynamic, hostIsProfiled)
#else
import DynFlags               (Way (..), dynamicGhc, interpWays)
import Module                 (componentIdToInstalledUnitId)
import Packages               (InstalledPackageInfo (..), PackageConfig,
                               lookupInstalledPackage, pprPackageConfig)
#endif

-- process
import System.Process         (readProcess)

-- hspec
import Test.Hspec

-- finkel-kernel
import Language.Finkel.Eval
import Language.Finkel.Fnk
import Language.Finkel.Form
import Language.Finkel.Make
import Language.Finkel.Syntax

-- Internal
import TestAux

makeTests :: Spec
makeTests = beforeAll getFnkTestResource makeFnkTests

makeFnkTests :: FnkSpec
makeFnkTests = beforeAll_ (removeArtifacts odir) $ do
  targetSourceTests

  -- Build bytecode
  buildByteCode "main1.fnk"
  buildByteCode "main2.fnk"
  buildByteCode "main3.fnk"
  buildByteCodeWith ["--fnk-verbose=2", "-v2"] "main4.fnk"
  buildByteCode "main5.fnk"
  buildByteCodeWith [ "--fnk-dump-dflags"
                    , "--fnk-dump-expand"
                    , "--fnk-dump-hs"
                    , "--fnk-trace-expand"
                    , "--fnk-trace-make"
                    , "--fnk-trace-spf" ]
                    "main9.fnk"

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

  let buildObj' flags inputs =
        before_  prepare_obj (buildObj flags inputs)
      buildObjAndExist' flags inputs =
        beforeAll_ prepare_obj
                   (let outputs = map (<.> "o") inputs
                    in  buildObjAndExist flags inputs outputs)
      prepare_obj = doUnload >> removeArtifacts odir

  -- Compile object codes with and without optimization option
  buildObjAndExist' [] ["P1", "P2"]
  buildObjAndExist' ["-O1"] ["P1", "P2"]

  -- Recompile P1 and P2 without deleting previous results
  --
  -- Seems like, the use of global persistent linker state in ghc < 8.10 have
  -- problem when recompiling the same modules after "-O1" option, unloading the
  -- object files.
  before_ doUnload (buildObj [] ["P1","P2"])

  -- Compile object code with and without optimization, module reorderd
  buildObjAndExist' [] ["P2", "P1"]
  buildObjAndExist' ["-O1"] ["P2", "P1"]

  -- Compile object codes, P3 requires but does not import P1.
  buildObjAndExist' [] ["P1", "P3"]
  buildObjAndExist' ["-O1"] ["P1", "P3"]

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

  -- Reload tests
  let reload_simple t =
        buildReload t
                    "foo"
                    [("R01.fnk.1", "R01.fnk"), (t, t)]
                    [("R01.fnk.2", "R01.fnk")]
                    "foo: before"
                    "foo: after"

  -- Reloading without modifications.
  buildReload "R02.fnk"
              "foo"
              [("R01.fnk.1","R01.fnk"), ("R02.fnk","R02.fnk")]
              []
              "foo: before"
              "foo: before"

  reload_simple "R02.fnk"

#if MIN_VERSION_ghc(8,10,0)
  -- Reloading test for modules containing `:require' of home package modules
  -- not working well with ghc < 8.10.

  -- XXX: Disabled at the moment.
  -- reload_simple "R03.fnk"
#endif

  -- Recompile tests
  let recompile_simple t extras =
        buildRecompile t
                       ([ ("R01.fnk.1", "R01.fnk") , dot_fnk t] ++
                        map dot_fnk extras)
                       [("R01.fnk.2", "R01.fnk")]
                       "foo: before\n"
                       "foo: after\n"
      dot_fnk x = let y = x <.> "fnk" in (y,y)

  recompile_simple "R04" []
  recompile_simple "R05" ["R05a"]
  recompile_simple "R06" ["R06a"]
  recompile_simple "R07" ["R07a", "R07b"]
  recompile_simple "R08" ["R08a", "R08b"]
  recompile_simple "R09" ["R09a", "R09b"]
  recompile_simple "R10" ["R10a", "R10b"]
  recompile_simple "R11" ["R11a", "R11b"]

  -- Errors
  buildFilesNG [] ["E01"]
  buildFilesNG [] ["E02"]


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
doUnload = runFnk (getSession >>= liftIO . flip unload []) fnkTestEnv
#endif

-- Action to decide whether profiling objects for the "finkel-kernel" package
-- are available at runtime.
hasProfilingObj :: IO Bool
hasProfilingObj = runFnk (hasProfilingObj1 pkg_name) fnkTestEnv
  where
    pkg_name = PackageName (fsLit "finkel-kernel")

hasProfilingObj1 :: PackageName -> Fnk Bool
#if MIN_VERSION_ghc(9,0,0)
hasProfilingObj1 _ = return False
#else
hasProfilingObj1 pkg_name = do
  initSessionForTest
  dflags <- getDynFlags
  case lookupPackageConfig dflags pkg_name of
    Nothing -> return False
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
#endif

fnksrc1, hssrc1, othersrc1 :: TargetSource
fnksrc1 = FnkSource "path1" (mkModuleName "Foo")
hssrc1 = HsSource "path2" (mkModuleName "Bar")
othersrc1 = OtherSource "path3"

subseq :: Eq a => [a] -> [a] -> Bool
subseq xs ys = any (isPrefixOf xs) (tails ys)

targetSourceTests :: FnkSpec
targetSourceTests =
  describe "TargetSource" $ do
    showTargetTest
    pprTargetTest
    asModuleNameTest

showTargetTest :: FnkSpec
showTargetTest = do
  describe "show TargetSource" $
    it "should contain filepath" $ \_ -> do
      show fnksrc1 `shouldSatisfy` subseq "path1"
      show hssrc1 `shouldSatisfy` subseq "path2"
      show othersrc1 `shouldSatisfy` subseq "path3"

asModuleNameTest :: FnkSpec
asModuleNameTest =
  describe "asModuleName" $
    it "should replace path separators" $ \_ ->
      asModuleName ("Foo" </> "Bar" </> "Buzz.fnk") `shouldBe` "Foo.Bar.Buzz"

runOutputable :: (MonadIO m, Outputable a) => a -> m String
runOutputable obj =
  liftIO $ runFnk (flip showPpr obj <$> getDynFlags) fnkTestEnv

pprTargetTest :: FnkSpec
pprTargetTest =
  describe "ppr TargetSource" $
    it "should contain filepath" $ \_ -> do
      let t target path = do str <- runOutputable target
                             str `shouldSatisfy` subseq path
      t fnksrc1 "path1"
      t hssrc1 "path2"
      t othersrc1 "path3"

buildByteCode :: FilePath -> FnkSpec
buildByteCode = buildByteCodeWith []

buildByteCodeWith :: [String] -> FilePath -> FnkSpec
buildByteCodeWith extra file =
  buildFiles (["-no-link", "-fbyte-code"] ++ extra) [file]

buildC :: FilePath -> FnkSpec
buildC file = buildFiles ["-no-link"] [file]

buildObj :: [String] -> [FilePath] -> FnkSpec
buildObj = buildFiles

buildObjAndExist :: [String] -> [FilePath] -> [String] -> FnkSpec
buildObjAndExist args inputs outputs =
  describe (labelWithOptionsAndFiles args inputs) $
    it "should write to output" $ \ftr ->
       pendingInputsForWindowsOr inputs $
         mapM_ (\f ->
                  do buildWork ftr args inputs
                     doesFileExist (odir </> f) `shouldReturn` True)
               outputs

buildFiles :: [String] -> [FilePath] -> FnkSpec
buildFiles pre inputs =
  describe (labelWithOptionsAndFiles pre inputs) $
    it "should compile successfully" $ \ftr ->
      pendingInputsForWindowsOr inputs $
          buildWork ftr pre inputs

pendingInputsForWindowsOr :: [String] -> Expectation -> Expectation
pendingInputsForWindowsOr inputs act =
  if os == "mingw32" && any (`elem` pendingInputsUnderWindows) inputs
    then pendingWith "pending under Windows"
    else act

-- Compilation of modules containing macro expansion is not working well under
-- Windows, pending for now.
pendingInputsUnderWindows :: [String]
pendingInputsUnderWindows = ["main4.fnk", "main8.fnk", "main9.fnk", "P1"]

buildFilesNG :: [String] -> [FilePath] -> FnkSpec
buildFilesNG pre inputs =
  describe (labelWithOptionsAndFiles pre inputs) $
    it "should throw an exception" $ \ftr ->
       (buildWork ftr pre inputs `shouldThrow` anyException)

labelWithOptionsAndFiles :: [String] -> [FilePath] -> String
labelWithOptionsAndFiles pre inputs  =
  "file " ++ show inputs ++
  if null pre
     then ""
     else " with " ++ unwords pre

buildWork :: FnkTestResource -> [String] -> [FilePath] -> Expectation
buildWork ftr pre inputs = do_work
  where
    do_work
       | isProfWay = do_prof_work
       | otherwise = do_work_with []
    -- Use dflags setttings for profile when running test executable with "+RTS
    -- -p" option.
    do_prof_work =
      do_work_with [ "-prof", "-fprof-auto", "-fprof-cafs"
                   , "-hisuf", "p_hi", "-osuf", "p_o" ]
    do_work_with extra =
      ftr_main ftr (extra ++ common_args ++ pre)
    common_args =
      ["-i.", "-i" ++ odir, "-v0"] ++ inputs
#if MIN_VERSION_ghc(9,0,0)
    isProfWay = hostIsProfiled
#else
    isProfWay = WayProf `elem` interpWays
#endif

odir :: FilePath
odir = "test" </> "data" </> "make"


-- ------------------------------------------------------------------------
--
-- For reload and recompile tests
--
-- ------------------------------------------------------------------------

buildReload
  :: String -- ^ Target module name.
  -> String -- ^ Function to return a 'String' value.
  -> [(String, String)] -- ^ List of (input file, output file), before.
  -> [(String, String)] -- ^ List of (input file, output file), after.
  -> String -- ^ Expected value of before.
  -> String -- ^ Expected value for after.
  -> FnkSpec
buildReload the_file fname files1 files2 before_str after_str =
  beforeAllWith (\ftr -> do
                    dir <- mk_tmp_dir ("reload" ++ the_file)
                    return (dir, ftr))
                (afterAll (rmdir . fst) work)
  where
    work = do
      describe (unwords ["Reload test for", the_file , "with", fname]) $ do
        it "should get expected values (bytecode)" $ do_work False
        it "should get expected values (objcode)" $ do_work True

    do_work use_obj (tmpdir, ftr) = do
       if use_obj && not dynamicGhc
          -- XXX: Reloading with non-dynamic object code not yet working. It
          -- does work when the test executable was compiled with "-dynamic"
          -- option.
          then pendingWith "non-dynamic object code not yet supported"
          else do
            is_travis <- lookupEnv "TRAVIS"
            if isJust is_travis && os == "darwin"
               then pendingWith "not supported under Travis OSX"
               else do_work' use_obj tmpdir ftr

    do_work' use_obj tmpdir ftr = do
       (ret1, ret2) <- runFnk (fnk_work use_obj tmpdir ftr) reloadFnkEnv
       (ret1, ret2) `shouldBe` (before_str, after_str)

    reloadFnkEnv = fnkTestEnv {envVerbosity = 3}

    fnk_work use_obj tmpdir ftr = do
      setup_reload_env use_obj tmpdir ftr
      copy_files tmpdir files1
      str1 <- make_and_eval
      reset_env
      copy_files tmpdir files2
      str2 <- make_and_eval
      return (str1, str2)

    setup_reload_env :: Bool -> FilePath -> FnkTestResource -> Fnk ()
    setup_reload_env use_obj tmpdir ftr = do
      let args0 = ("-i" ++ tmpdir) : if use_obj
                                       then ["-fobject-code"]
                                       else []
          args1 = ["-v0"]
          parseAndSet args = do
            dflags0 <- getDynFlags
            (dflags1,_,_) <- parseDynamicFlagsCmdLine dflags0 (map noLoc args)
            setDynFlags dflags1
          tfile = TargetFile the_file Nothing
      parseAndSet args0
      ftr_init ftr
      prepareInterpreter
      parseAndSet args1
      setTargets [Target tfile use_obj Nothing]

    make_and_eval :: Fnk String
    make_and_eval = do
      _ <- make [(noLoc the_file, Nothing)] False Nothing
      setContextModules [asModuleName the_file]
      hexpr <- buildHsSyn parseExpr [qSymbol fname fname 0 0 0 0]
      unsafeCoerce# <$> evalExpr hexpr

    reset_env = getSession >>= liftIO . flip unload []

#if MIN_VERSION_ghc(9,0,0)
    dynamicGhc = hostIsDynamic
#endif

-- | Make a test for recompilation.
buildRecompile
  :: String -- ^ Module containing the @main@ function.
  -> [(String, String)] -- ^ List of @(SRC_FILE, DEST_FILE)@ for first run.
  -> [(String, String)] -- ^ List of @(SRC_FILE, DEST_FILE)@ for second run.
  -> String -- ^ Expected output from the first run.
  -> String -- ^ Expected output from the second run.
  -> FnkSpec
buildRecompile main_mod files1 files2 before_str after_str =
  beforeAllWith (\ftr -> do
                    dir <- mk_tmp_dir ("recompile" ++ main_mod)
                    return (dir, ftr))
                (afterAll (rmdir . fst) work)
    where
      work =
        describe ("Recompile " ++ main_mod) $
          it "should return expected result" $ \(tmpdir, ftr) -> do
          is_travis <- lookupEnv "TRAVIS"
          if isJust is_travis && os == "darwin"
            then pendingWith "not supported under Travis OSX"
            else if os == "mingw32"
              -- XXX: Recompile tests not working well under Windows, pending
              -- for now.
              then pendingWith "recompile tests pending under Windows"
              else do_work (tmpdir, ftr)

      do_work (tmpdir, ftr) = do
        -- Running with files1 twice to see compilation avoidance.
        compile_and_run tmpdir ftr False files1 before_str
        compile_and_run tmpdir ftr True files1 before_str
        compile_and_run tmpdir ftr False files2 after_str

      compile_and_run tmpdir ftr skip_copy files expected_str = do
        let a_dot_out = tmpdir </> "a.out"
        unless skip_copy $ copy_files tmpdir files
        buildWork ftr []
                  [ "-i" ++ tmpdir
                  , "-outputdir", tmpdir
                  , "-main-is", main_mod
                  , "-o", a_dot_out
                  , "--fnk-trace-make"
                  , main_mod ]
        output1 <- readProcess a_dot_out [] ""
        output1 `shouldBe` expected_str

copy_files :: MonadIO m => FilePath -> [(FilePath, FilePath)] -> m ()
copy_files dir fs = liftIO (mapM_ copy fs)
  where
    copy (i,o) = copyFile (odir </> i) (dir </> o)

-- | Create temporary directory with given name.
mk_tmp_dir :: String -> IO FilePath
mk_tmp_dir name = do
  tmp <- getTemporaryDirectory
  let my_tmpdir = tmp </> name
  catch (removeDirectoryRecursive my_tmpdir)
        (\(SomeException _e) -> return ())
  createDirectoryIfMissing True my_tmpdir
  return my_tmpdir

-- | Remove given directory.
rmdir :: FilePath -> IO ()
rmdir = removeDirectoryRecursive
