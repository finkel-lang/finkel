-- | Tests for 'make'.
module MakeTest
  ( makeTests
  ) where

-- base
import Data.List                    (isPrefixOf, tails)
import System.FilePath              (takeBaseName, (</>))
import System.Info                  (os)

-- ghc
import Config                       (cProjectVersionInt)
import DynFlags                     (Way (..), interpWays)
import FastString                   (fsLit)

-- hspec
import Test.Hspec

-- finkel-kernel
import Language.Finkel.Lexer
import Language.Finkel.TargetSource

-- Internal
import TestAux

makeTests :: Spec
makeTests = beforeAll_ (removeArtifacts odir) $ do
  showTargetTest
  buildFnk "main1.fnk"
  buildFnk "main2.fnk"
  buildFnk "main3.fnk"
  buildFnk "main4.fnk"
  buildFnk "main5.fnk"
  buildC (odir </> "cbits1.c")

showTargetTest :: Spec
showTargetTest = do
  let fnksrc = FnkSource "path1" "Foo" [] sp
      hssrc = HsSource "path2"
      otsrc = OtherSource "path3"
      sp = initialSPState (fsLit "showTargetTest") 1 1
  describe "show TargetSource" $
    it "should contain filepath" $ do
      let subseq xs ys = any (isPrefixOf xs) (tails ys)
      show fnksrc `shouldSatisfy` subseq "path1"
      show hssrc `shouldSatisfy` subseq "path2"
      show otsrc `shouldSatisfy` subseq "path3"

buildFnk :: FilePath -> Spec
buildFnk = buildFile []

buildC :: FilePath -> Spec
buildC = buildFile ["-no-link"]

buildFile :: [String] -> FilePath -> Spec
buildFile pre path =
  describe ("file " ++ path) $
    it "should compile successfully" work
  where
    work
      | cProjectVersionInt == "810"
      , os == "mingw32"
      , takeBaseName path `elem` skipped
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
      ["-no-link", "-fbyte-code", "-i.", "-i" ++ odir, "-v0", path]

odir :: FilePath
odir = "test" </> "data" </> "make"
