-- | Tests for 'make'.
module MakeTest
  ( makeTests
  ) where

-- base
import Control.Monad.IO.Class       (MonadIO (..))
import Data.List                    (isPrefixOf, tails)
import System.FilePath              (takeBaseName, (</>))
import System.Info                  (os)

-- ghc
import Config                       (cProjectVersionInt)
import DynFlags                     (HasDynFlags (..), Way (..), interpWays)
import FastString                   (fsLit)
import Outputable                   (Outputable (..), showPpr)

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

  -- Build object codes
  buildC (odir </> "cbits1.c")
  buildObj ["-fforce-recomp", "-ddump-parsed", "-ddump-parsed-ast"
           ,"-dsource-stats"]
           ["main5.fnk"]
  buildObj [] ["cbits1.c", "cbits2.c", "cbits3.c", "main6.fnk"]
  buildObj [] (map (odir </>) ["cbits1.o","cbits2.o","cbits3.o"] ++
               ["main6.fnk"])
  buildObj [] ["main6.fnk", "cbits1.c", "cbits2.c", "cbits3.c"]
  buildObj ["--fnk-dump-hs", "--fnk-hsdir=" ++ (odir </> "gen")]
           ["M5", "M4" </> "A.fnk", "M4" </> "B.fnk", "M4", "main7.fnk"]

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
buildFiles pre paths =
  describe ("file " ++ show paths) $
    it "should compile successfully" work
  where
    work
      | cProjectVersionInt == "810"
      , os == "mingw32"
      , any (\path -> takeBaseName path `elem` skipped) paths
      = pendingWith "Not yet supported"
      | otherwise
      = do_work
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
      ["-i.", "-i" ++ odir, "-v0"] ++ paths

odir :: FilePath
odir = "test" </> "data" </> "make"
