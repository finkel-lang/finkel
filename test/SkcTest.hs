module SkcTest where

import Control.Exception
import Data.List (isPrefixOf, tails)
import GHC.Paths (libdir)
import Test.Hspec
import Test.QuickCheck

import Language.SK.Expand
import Language.SK.Form
import Language.SK.GHC
import Language.SK.Homoiconic
import Language.SK.Run
import Language.SK.SKC
import Language.SK.Syntax

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Set as Set

skcTests :: Spec
skcTests = do
  exceptionTest
  fromGhcTest
  gensymTest
  expandTest

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

  describe "builderError and failB" $
    it "should throw SkException" $ do
      let form = "(:: foo (->)) (= foo 100)"
          sel :: SkException -> Bool
          sel (SkException msg) = subseq "syntax" msg
          run a = runSkcWithoutHandler a initialSkEnv
          build = do (form', _) <- parseSexprs Nothing (BL.pack form)
                     buildHsSyn parseDecls form'
      run build `shouldThrow` sel

subseq :: Eq a => [a] -> [a] -> Bool
subseq xs ys = any (isPrefixOf xs) (tails ys)

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

expandTest :: Spec
expandTest = do
  let expand1_fn = macroFunction (Macro expand1)
  describe "expand-1 of nil" $
    it "should return nil" $ do
      ret <- expand1_fn nil
      case ret of
        Right ret' -> ret' `shouldBe` nil
        _ -> expectationFailure "macro expansion failed"
  describe "expand-1 of (quote 42.0)" $
    it "should return non-empty form" $ do
      let form = toCode (List [ toCode $ aSymbol "quote"
                              , toCode $ aFractional 42.0])
      ret <- expand1_fn form
      case ret of
        Right ret' -> length ret' `shouldSatisfy` (>= 1)
        _ -> expectationFailure "macro expansion failed"
