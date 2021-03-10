{-# LANGUAGE CPP #-}

module EmitTest where

#include "ghc_modules.h"

-- ghc
import GHC_Data_FastString   (fsLit)
import GHC_Types_Name_Reader (mkVarUnqual)
import GHC_Types_SrcLoc      (GenLocated (..), noSrcSpan)

-- hspec
import Test.Hspec

-- finkel-kernel
import Language.Finkel.Emit
import Language.Finkel.Fnk
import Language.Finkel.Lexer

-- Internal
import TestAux

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
emitSimple h = runFnk (genHsSrc sp h) fnkTestEnv
  where
    sp = initialSPState (fsLit "<EmitTest>") 0 0
