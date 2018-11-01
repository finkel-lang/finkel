module EvalTest (evalTests) where

import Unsafe.Coerce
import qualified Data.ByteString.Lazy as BL

import Test.Hspec

import Language.SK.Eval (evalExpr)
import Language.SK.Lexer (evalSP)
import Language.SK.Expand (expands, withExpanderSettings)
import Language.SK.Make (initSessionForMake, defaultSkEnv)
import Language.SK.Reader (sexprs)
import Language.SK.Run (buildHsSyn, runSkc)
import Language.SK.SKC (SkEnv(..), failS, setContextModules)
import Language.SK.Syntax (parseExpr)

evalTests :: [FilePath] -> Spec
evalTests = mapM_ mkTest

mkTest :: FilePath -> Spec
mkTest file =
  describe file $
    it "should evaluate to True" $ do
      contents <- BL.readFile file
      let modules = ["Prelude", "Language.SK"]
      ret <- runSkc (evalContents contents)
                    (defaultSkEnv {envContextModules = modules})
      ret `shouldBe` Right True
  where
    evalContents bs = do
      initSessionForMake
      case evalSP sexprs (Just "<mkTest>") bs of
        Right form0 -> do
          form1 <- withExpanderSettings (expands form0)
          hexpr <- buildHsSyn parseExpr form1
          hval <- evalExpr hexpr
          return (unsafeCoerce hval)
        Left err -> failS err
