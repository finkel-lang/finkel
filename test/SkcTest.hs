module SkcTest where

import GHC.Paths (libdir)
import Test.Hspec
import Test.QuickCheck

import Language.SK.Expand
import Language.SK.Form
import Language.SK.GHC
import Language.SK.Homoiconic
import Language.SK.Run
import Language.SK.SKC

import qualified Data.Set as Set

skcTests :: Spec
skcTests = do
  exceptionTest
  fromGhcTest
  gensymTest

exceptionTest :: Spec
exceptionTest = do
  describe "Eq and Show instance of SkException" $
    it "should return True when comparing with self" $
      property (\str -> let e1 = SkException str
                            e2 = SkException str
                        in  e1 == e2 && show e1 == show e2)

  describe "running Skc action containing `failS'" $
    it "should throw SkException" $ do
      let p :: SkException -> Bool
          p (SkException m) = m == "foo"
          act = failS "foo"
      runSkcWithoutHandler act initialSkEnv `shouldThrow` p

  describe "running Skc action containing SourceError" $
    it "should throw SourceError" $ do
      let act = toGhc (skSrcError nil "foo") initialSkEnv
          p :: SourceError -> Bool
          p _ = True
      runGhc (Just libdir) act `shouldThrow` p

  describe "applying macroNames to specialForms" $
    it "should not return name of special forms" $ do
      let ns = macroNames specialForms
      ns `shouldBe` []

fromGhcTest :: Spec
fromGhcTest =
  describe "converting Ghc to Skc" $
    it "should return the returned value in Ghc" $ do
      let v :: Int
          v = 42
      x <- runSkc (fromGhc (return v)) initialSkEnv
      x `shouldBe` Right v

gensymTest :: Spec
gensymTest =
  describe "generating two gensyms" $
    it "should not be equal" $ do
      let gen _ = do
            g1 <- gensym
            g2 <- gensym
            return $ toCode [g1, g2]
          f = macroFunction (Macro gen)
      ret <- f nil
      case ret of
        Right (LForm (L _ (HsList [g1, g2]))) -> g1 `shouldNotBe` g2
        _ -> expectationFailure "macro expansion failed"
