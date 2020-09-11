{-# LANGUAGE CPP       #-}
{-# LANGUAGE MagicHash #-}
-- | Tests for 'make'.
module MakeTest
  ( makeTests
  ) where

-- base
import Control.Exception            (SomeException (..), catch)
import Control.Monad                (unless, when)
import Control.Monad.IO.Class       (MonadIO (..))
import Data.List                    (isPrefixOf, tails)
import GHC.Exts                     (unsafeCoerce#)
import System.FilePath              ((<.>), (</>))

-- directory
import System.Directory             (copyFile, createDirectoryIfMissing,
                                     doesFileExist, getDirectoryContents,
                                     getTemporaryDirectory,
                                     removeDirectoryRecursive)

-- filepath
import System.FilePath              (takeExtension)

-- ghc
import DynFlags                     (DynFlags (..), HasDynFlags (..), Way (..),
                                     dynamicGhc, interpWays,
                                     parseDynamicFlagsCmdLine)
import FastString                   (fsLit)
import GHC                          (setTargets)
import GhcMonad                     (GhcMonad (..))
import HscTypes                     (Target (..), TargetId (..))
import Linker                       (unload)
import Module                       (componentIdToInstalledUnitId)
import Outputable                   (Outputable (..), showPpr, showSDoc)
import Packages                     (InstalledPackageInfo (..), PackageConfig,
                                     PackageName (..), lookupInstalledPackage,
                                     lookupPackageName, pprPackageConfig)
import SrcLoc                       (noLoc)

-- hspec
import Test.Hspec

-- finkel-kernel
import Language.Finkel.Eval
import Language.Finkel.Fnk
import Language.Finkel.Form
import Language.Finkel.Lexer
import Language.Finkel.Make
import Language.Finkel.SpecialForms
import Language.Finkel.Syntax
import Language.Finkel.TargetSource

-- Internal
import TestAux

makeTests :: Spec
makeTests = beforeAll_ (removeArtifacts odir) $ do
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

  -- Errors
  buildFilesNG [] ["E01"]

  -- Reload tests
  buildReload "R2.fnk"
              "foo"
              [("R1.fnk.1", "R1.fnk"), ("R2.fnk", "R2.fnk")]
              [("R1.fnk.2", "R1.fnk")]
              "foo: before"
              "foo: after"

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

-- Action to decide whether profiling objects for the "finkel-kernel" package
-- are available at runtime.
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

buildByteCode :: FilePath -> Spec
buildByteCode = buildByteCodeWith []

buildByteCodeWith :: [String] -> FilePath -> Spec
buildByteCodeWith extra file =
  buildFiles (["-no-link", "-fbyte-code"] ++ extra) [file]

buildC :: FilePath -> Spec
buildC file = buildFiles ["-no-link"] [file]

buildObj :: [String] -> [FilePath] -> Spec
buildObj = buildFiles

buildObjAndExist :: [String] -> [FilePath] -> [String] -> Spec
buildObjAndExist args inputs outputs =
  describe (labelWithOptionsAndFiles args inputs) $
    do it "should compile successfully" (buildWork args inputs)
       mapM_ (\f ->
                do let ofile = odir </> f
                   it ("should write " ++ ofile) $
                     doesFileExist ofile `shouldReturn` True)
             outputs

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
buildWork pre inputs = do_work
  where
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


-- ------------------------------------------------------------------------
--
-- For reload test
--
-- ------------------------------------------------------------------------

buildReload
  :: String -- ^ Target module name
  -> String -- ^ Function to return 'String'
  -> [(String, String)] -- ^ List of (input file, output file), before.
  -> [(String, String)] -- ^ List of (input file, output file), after.
  -> String -- ^ Expected value of before.
  -> String -- ^ Expected value for after.
  -> Spec
buildReload the_file fname files1 files2 before_str after_str =
  beforeAll (mk_tmp_dir "reload") (afterAll rmdir work)
  where
    work = do
      describe (unwords ["Reload test for", the_file , "with", fname]) $ do
        it "should get expected values (bytecode)" $ do_work False
        it "should get expected values (objcode)" $ do_work True

    do_work use_obj tmpdir = do
       if use_obj && not dynamicGhc
          -- XXX: Reloading with non-dynamic object code not yet working. It
          -- does work when the test executable was compiled with "-dynamic"
          -- option.
          then pendingWith "non-dynamic object code not yet supported"
          else do_work' use_obj tmpdir

    do_work' use_obj tmpdir = do
       (ret1, ret2) <- runFnk (fnk_work use_obj tmpdir) defaultFnkEnv
       (ret1, ret2) `shouldBe` (before_str, after_str)

    fnk_work use_obj tmpdir = do
      setup_reload_env use_obj tmpdir
      copy_files tmpdir files1
      str1 <- make_and_eval False
      reset_env
      copy_files tmpdir files2
      str2 <- make_and_eval True
      return (str1, str2)

    setup_reload_env :: Bool -> FilePath -> Fnk ()
    setup_reload_env use_obj tmpdir = do
      let args = "-v0" : ("-i" ++ tmpdir) : if use_obj
                                              then ["-fobject-code"]
                                              else []
          tfile = TargetFile the_file Nothing
      initSessionForTest
      prepareInterpreter
      dflags0 <- getDynFlags
      (dflags1, _, _) <- parseDynamicFlagsCmdLine dflags0 (map noLoc args)
      setDynFlags dflags1
      setTargets [Target tfile use_obj Nothing]

    make_and_eval :: Bool -> Fnk String
    make_and_eval is_reload = do
      make [(noLoc the_file, Nothing)] False False Nothing
      unless is_reload $ setContextModules [asModuleName the_file]
      hexpr <- buildHsSyn parseExpr [qSymbol fname]
      unsafeCoerce# <$> evalExpr hexpr

    copy_files tmpdir = liftIO . mapM_ (copy tmpdir)
    copy tmp (i, o) = copyFile (odir </> i) (tmp </> o)

    reset_env = do
      hsc_env <- getSession
      liftIO (unload hsc_env [])

    rmdir my_tmpdir =
      removeDirectoryRecursive my_tmpdir

    mk_tmp_dir name =
      do tmp <- getTemporaryDirectory
         let my_tmpdir = tmp </> name
         catch (removeDirectoryRecursive my_tmpdir)
               (\(SomeException _e) -> return ())
         createDirectoryIfMissing True my_tmpdir
         return my_tmpdir
