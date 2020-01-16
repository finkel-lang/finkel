-- | Tests for 'make'.
module MakeTest
  ( makeTests
  ) where

-- base
import Control.Monad                (void)
import Data.List                    (isPrefixOf, tails)
import System.FilePath              ((</>))

-- ghc
import DynFlags                     (DynFlags (..), GhcLink (..), Way (..),
                                     interpWays, parseDynamicFlagsCmdLine)
import FastString                   (fsLit)
import GHC                          (setSessionDynFlags)
import GhcMonad                     (getSessionDynFlags)
import SrcLoc                       (noLoc)

-- hspec
import Test.Hspec

-- finkel-kernel
import Language.Finkel.Fnk
import Language.Finkel.Lexer
import Language.Finkel.Make
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
  buildC "cbits1.c"

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
  describe "eq TargetSource" $
    it "should return True iff comparing with itself" $ do
      fnksrc `shouldBe` fnksrc
      fnksrc `shouldNotBe` hssrc
      fnksrc `shouldNotBe` otsrc

buildFnk :: FilePath -> Spec
buildFnk = buildFile initSessionForTest

buildC :: FilePath -> Spec
buildC = buildFile
           (do initSessionForTest
               dflags <- getSessionDynFlags
               void (setSessionDynFlags (dflags {ghcLink=NoLink})))

buildFile :: Fnk () -> FilePath -> Spec
buildFile pre path =
  describe ("file " ++ path) $
    it "should compile successfully" $ do
      ret <- runFnk
               (do pre
                   -- Use dflags setttings for profile when running test
                   -- executable with "+RTS -p" option.
                   if WayProf `elem` interpWays
                      then make_profile targets Nothing
                      else make_simple targets Nothing)
                    (defaultFnkEnv { envSilent = True })
      ret `shouldBe` ()
  where
    targets = [(noLoc path, Nothing)]
    make' flags sources out = do
      dflags0 <- getSessionDynFlags
      let dflags1 = dflags0 {importPaths = [".", odir]}
          flags' = map noLoc flags
      (dflags2,_,_) <- parseDynamicFlagsCmdLine dflags1 flags'
      _ <- setSessionDynFlags dflags2
      make sources False False out
    make_simple = make' []
    make_profile = make' ["-prof", "-fprof-auto", "-fprof-cafs"
                         , "-hisuf", "p_hi", "-osuf", "p_o"]

odir :: FilePath
odir = "test" </> "data" </> "make"
