{-# LANGUAGE CPP       #-}
{-# LANGUAGE MagicHash #-}
-- | Tests for 'make'.
module MakeTest
  ( makeTests
  , makeFnkTests
  ) where

#include "ghc_modules.h"

-- base
import Control.Exception       (SomeException (..))
import Control.Monad           (unless, when)
import Control.Monad.IO.Class  (MonadIO (..))
import Data.List               (isPrefixOf, tails)
import Data.Maybe              (isJust)
import GHC.Exts                (unsafeCoerce#)
import System.Environment      (getExecutablePath, lookupEnv)
import System.Info             (os)

-- directory
import System.Directory        (copyFile, createDirectoryIfMissing,
                                doesFileExist, getTemporaryDirectory,
                                removeDirectoryRecursive)
#if !MIN_VERSION_ghc(9,0,0)
import System.Directory        (getDirectoryContents)
#endif

-- filepath
import System.FilePath         ((<.>), (</>))
#if !MIN_VERSION_ghc(9,0,0)
import System.FilePath         (takeExtension)
#endif

-- ghc
import GHC_Data_FastString     (fsLit)
import GHC_Driver_Monad        (GhcMonad (..))
import GHC_Driver_Ppr          (showPpr)
import GHC_Driver_Session      (HasDynFlags (..))
import GHC_Types_SrcLoc        (noLoc)
import GHC_Unit_Module         (mkModuleName)
import GHC_Unit_State          (PackageName (..))
import GHC_Utils_Outputable    (Outputable (..))

#if MIN_VERSION_ghc(9,4,0)
import GHC.Driver.Env          (hscInterp)
import GHC.Linker.Loader       (unload)
#elif MIN_VERSION_ghc(9,2,0)
import GHC.Linker.Loader       (unload)
import GHC.Runtime.Interpreter (hscInterp)
#else
import GHC_Runtime_Linker      (unload)
#endif

#if !MIN_VERSION_ghc(9,0,0)
import GHC_Driver_Session      (DynFlags (..))
import GHC_Unit_State          (lookupPackageName)
import GHC_Utils_Outputable    (showSDoc)
#endif

#if MIN_VERSION_ghc(9,0,0)
import GHC_Platform_Ways       (hostIsDynamic, hostIsProfiled)
#else
import DynFlags                (Way (..), dynamicGhc, interpWays)
import Module                  (componentIdToInstalledUnitId)
import Packages                (InstalledPackageInfo (..), PackageConfig,
                                lookupInstalledPackage, pprPackageConfig)
#endif

-- process
import System.Process          (readProcess)

-- hspec
import Test.Hspec

-- finkel-kernel
import Language.Finkel.Eval
import Language.Finkel.Fnk
import Language.Finkel.Form
import Language.Finkel.Make    (TargetSource (..), asModuleName, buildHsSyn,
                                setContextModules, simpleMake,
                                withExpanderSettings)
import Language.Finkel.Plugin  (plugin, setFinkelPluginWithArgs)
import Language.Finkel.Syntax

-- Internal
import TestAux

makeTests :: Spec
makeTests = beforeAll getFnkTestResource makeFnkTests

makeFnkTests :: FnkSpec
makeFnkTests = beforeAll_ (removeArtifacts odir) $ do
  targetSourceTests
  let in_odir file = odir </> file

  -- Build bytecode
  buildByteCode (in_odir "main1.hs")
  buildByteCode (in_odir "main2.hs")
  buildByteCode (in_odir "main3.hs")
  buildByteCodeWith ["--fnk-verbose=2", "-v2"] (in_odir "main4.hs")
  buildByteCode (in_odir "main5.hs")
  buildByteCodeWith [ "--fnk-dump-dflags"
                    , "--fnk-dump-expand"
                    , "--fnk-dump-hs"
                    , "--fnk-trace-expand"
                    , "--fnk-trace-make"
                    , "--fnk-trace-spf" ]
                    (in_odir "main9.hs")

  -- Build object codes
  buildC (odir </> "cbits1.c")
  buildObj ["-fforce-recomp", "-ddump-parsed", "-ddump-parsed-ast"
           ,"-dsource-stats"]
           [in_odir "main5.hs"]
  buildObj [] (map in_odir ["cbits1.c", "cbits2.c", "cbits3.c", "main6.hs"])
  buildObj [] (map (odir </>) ["cbits1.o","cbits2.o","cbits3.o"] ++
               [in_odir "main6.hs"])
  buildObj [] (map in_odir ["main6.hs", "cbits1.c", "cbits2.c", "cbits3.c"])
  buildObj [] ["M4.A"]
  buildObj ["--fnk-dump-hs", "--fnk-hsdir=" ++ (in_odir "gen")]
           ["M5", "M4.A", "M4.B", "M4", in_odir "main7.hs"]
  buildObj ["-O2"] [in_odir "main8.hs"]

  let buildObj' flags inputs =
        before_  prepare_obj (buildObj flags inputs)
      buildObjAndExist' flags inputs =
        beforeAll_ prepare_obj
                   (let outputs = map (<.> "o") inputs
                    in  buildObjAndExist flags inputs outputs)
      prepare_obj = do
        mapM_ removeArtifacts [odir, in_odir "M4", in_odir "M6"]

  -- Compile object codes with and without optimization option
  buildObjAndExist' [] ["P1", "P2"]
  buildObjAndExist' ["-O1"] ["P1", "P2"]

  -- Recompile P1 and P2 without deleting previous results
  buildObj [] ["P1","P2"]

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
  let reload_simple t after_files after_output =
        buildReload t
                    "foo"
                    [("R01.hs.1", "R01.hs"), (t, t)]
                    after_files
                    "foo: before"
                    after_output

  -- Reloading without modifications.
  reload_simple "R02.hs" [] "foo: before"

  -- Reloading with modifications.
  reload_simple "R02.hs" [("R01.hs.2", "R01.hs")] "foo: after"

  -- Reloading test for modules containing `:require' of home package modules
  -- not working well with ghc >= 8.10.

  -- XXX: Disabled at the moment.
  -- reload_simple "R03.fnk"

  -- Recompile tests
  let recompile_simple t extras =
        buildRecompile t
                       ([("R01.hs.1", "R01.hs"), dot_hs t] ++ map dot_hs extras)
                       [("R01.hs.2", "R01.hs")]
                       "foo: before\n"
                       "foo: after\n"
      dot_hs x = let y = x <.> "hs" in (y,y)

  recompile_simple "R04" []
  recompile_simple "R05" ["R05a"]
  recompile_simple "R06" ["R06a"]

  -- XXX: R07 and R08 contains nested require of home modules. When compiling
  -- with plugin, recompilation is not working with modification of R01.hs.
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
--
-- Persistent linker state is isolated from ghc 8.10, does nothing.

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
       buildWork ftr pre inputs `shouldThrow` anyException

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
      ["-i.", "-i" ++ odir, "-v1"] ++ inputs
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
                    dir <- mk_tmp_dir ("reload_" ++ the_file)
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
       let act = runFnk (fnk_work use_obj tmpdir ftr) reloadFnkEnv
       (ret1, ret2) <- quietly act
       (ret1, ret2) `shouldBe` (before_str, after_str)

    reloadFnkEnv = fnkTestEnv {envVerbosity = 3}

    fnk_work use_obj tmpdir ftr = do
      setup_reload_env use_obj tmpdir ftr
      copy_files tmpdir files1
      str1 <- make_and_eval tmpdir
      reset_env
      copy_files tmpdir files2
      str2 <- make_and_eval tmpdir
      return (str1, str2)

    setup_reload_env :: Bool -> FilePath -> FnkTestResource -> Fnk ()
    setup_reload_env use_obj tmpdir ftr = do
      me <- liftIO getExecutablePath
      let args0 = ("-i" ++ tmpdir) : ["-fobject-code" | use_obj]
          args1 = ["-v0", "-F", "-pgmF", me, "-optF", "--no-warn-interp"]
      parseAndSetDynFlags (args0 <> args1)
      ftr_init ftr
      prepareInterpreter
      setFinkelPluginWithArgs plugin []

    make_and_eval :: FilePath -> Fnk String
    make_and_eval tmpdir = do
      _ <- simpleMake [(noLoc (tmpdir </> the_file), Nothing)] False Nothing
      setContextModules [asModuleName the_file]
      hexpr <- buildHsSyn parseExpr [qSymbol fname fname 0 0 0 0]
      unsafeCoerce# <$> evalExpr hexpr

    reset_env = do
      hsc_env <- getSession
#if MIN_VERSION_ghc(9,2,0)
      liftIO (unload (hscInterp hsc_env) hsc_env [])
#else
      liftIO (unload hsc_env [])
#endif

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
                    dir <- mk_tmp_dir ("recompile_" ++ main_mod)
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

        -- When compiling with plugin, need to unload home unit modules from the
        -- global HscEnv used during macro expansion. Otherwise the required R01
        -- module won't recompiled.
        clearGlobalSession

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

clearGlobalSession :: IO ()
clearGlobalSession = runFnk clear (fnkTestEnv {envVerbosity = 1
                                              ,envInvokedMode = GhcPluginMode})
  where
    clear = withExpanderSettings $ do
      -- See also 'clearHPTs' in "ghc/GHCi/UI.hs".
      _ <- simpleMake [] False Nothing
      pure ()

copy_files :: MonadIO m => FilePath -> [(FilePath, FilePath)] -> m ()
copy_files dir fs = liftIO (mapM_ copy fs)
  where
    copy (i,o) =
      let src = odir </> i
          dst = dir </> o
      in  copyFile src dst

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
