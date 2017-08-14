module EvalTest (evalTests) where

import Unsafe.Coerce
import qualified Data.ByteString.Lazy as BL

import Test.Hspec

import Language.SK.Eval (evalExpr)
import Language.SK.Lexer (evalSP)
import Language.SK.Macro (expands, withExpanderSettings)
import Language.SK.Reader (sexprs)
import Language.SK.Run (buildHsSyn, initialSkEnv, runSkc)
import Language.SK.SKC (failS)
import Language.SK.Syntax (parseExpr)

evalTests :: [FilePath] -> Spec
evalTests = mapM_ mkTest

mkTest :: FilePath -> Spec
mkTest file =
  describe file $
    it "should evaluate to True" $ do
      contents <- BL.readFile file
      ret <- runSkc (evalContents contents) initialSkEnv
      ret `shouldBe` Right True
  where
    evalContents bs =
      case evalSP sexprs (Just "<mkTest>") bs of
        Right form0 -> do
          form1 <- withExpanderSettings (expands form0)
          hexpr <- buildHsSyn parseExpr form1
          hval <- evalExpr hexpr
          return (unsafeCoerce hval)
        Left err -> failS err
