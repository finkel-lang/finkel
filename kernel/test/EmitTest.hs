module EmitTest where

-- ghc
import FastString (fsLit)
import RdrName (mkVarUnqual)
import SrcLoc (GenLocated(..), noSrcSpan)

-- hspec
import Test.Hspec

-- sk-kernel
import Language.SK.Emit
import Language.SK.Lexer
import Language.SK.Make
import Language.SK.Run

emitTests :: Spec
emitTests = do
  let fooVar = mkVarUnqual (fsLit "foo")
  describe "emit RdrName" $ do
    it "should show \"foo\"" $ do
      foo <- emitSimple fooVar
      foo `shouldBe` "foo"
  describe "emit located thing" $ do
    it "should show located contents" $ do
      x <- emitSimple (L noSrcSpan fooVar)
      x `shouldBe` "foo"

emitSimple :: HsSrc a => a -> IO String
emitSimple h = runSkc (genHsSrc sp h) defaultSkEnv
  where
    sp = initialSPState (fsLit "<EmitTest>") 0 0
