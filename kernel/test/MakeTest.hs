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
  showTargetTest
  pprTargetTest
  buildFnk "main1.fnk"
  buildFnk "main2.fnk"
  buildFnk "main3.fnk"
  buildFnk "main4.fnk"
  buildFnk "main5.fnk"
  buildC (odir </> "cbits1.c")

fnksrc1, hssrc1, othersrc1 :: TargetSource
fnksrc1 = FnkSource "path1" "Foo" [] (initialSPState (fsLit "dummy") 1 1)
hssrc1 = HsSource "path2"
othersrc1 = OtherSource "path3"

subseq :: Eq a => [a] -> [a] -> Bool
subseq xs ys = any (isPrefixOf xs) (tails ys)

showTargetTest :: Spec
showTargetTest = do
  describe "show TargetSource" $
    it "should contain filepath" $ do
      show fnksrc1 `shouldSatisfy` subseq "path1"
      show hssrc1 `shouldSatisfy` subseq "path2"
      show othersrc1 `shouldSatisfy` subseq "path3"

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
