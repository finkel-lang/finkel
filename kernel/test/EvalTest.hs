module EvalTest (evalTests) where

import Unsafe.Coerce
import qualified Data.ByteString.Lazy.Char8 as BL

import DynFlags (HasDynFlags(..))
import GHC (getPrintUnqual)
import Outputable (showSDocForUser)
import PprTyThing (pprTypeForUser)

import Test.Hspec

import Language.SK.Builder (Builder)
import Language.SK.Eval (evalExpr, evalExprType, evalTypeKind)
import Language.SK.Form (Code)
import Language.SK.Lexer (evalSP)
import Language.SK.Expand (expands, withExpanderSettings)
import Language.SK.Make (initSessionForMake, defaultSkEnv)
import Language.SK.Reader (sexprs)
import Language.SK.Run (buildHsSyn, runSkc)
import Language.SK.SKC (Skc, SkEnv(..), failS, setContextModules)
import Language.SK.Syntax (parseExpr, parseType)

evalTests :: [FilePath] -> Spec
evalTests exprFiles = do
  mapM_ exprTest exprFiles
  exprTypeTest
  typeKindTest

exprTest :: FilePath -> Spec
exprTest file =
  describe file $
    it "should evaluate to True" $ do
      contents <- BL.readFile file
      let modules = ["Prelude", "Language.SK"]
      ret <- evalContents contents
      ret `shouldBe` Right True
  where
    evalContents bs =
      runSkc (doEval "<exprTypeTest>" parseExpr act bs) evalSkEnv
    act expr = do
      hval <- evalExpr expr
      return (unsafeCoerce hval)

exprTypeTest :: Spec
exprTypeTest = do
  describe "type of True" $
    it "should be Bool" $ do
      ret <- runEvalType "True"
      ret `shouldBe` Right "Bool"
  where
    runEvalType str =
      runSkc (doEval "<exprTypeTest>" parseExpr act (BL.pack str))
             evalSkEnv
    act expr  = do
      ty <- evalExprType expr
      dflags <- getDynFlags
      unqual <- getPrintUnqual
      return (showSDocForUser dflags unqual (pprTypeForUser ty))

typeKindTest :: Spec
typeKindTest = do
  describe "kind of Maybe" $
    it "should be * -> *" $ do
      ret <- runTypeKind "Maybe"
      ret `shouldBe` Right "* -> *"
  where
    runTypeKind str =
      runSkc (doEval "<typeKindTest>" parseType act (BL.pack str))
             evalSkEnv
    act expr = do
      (_, kind) <- evalTypeKind expr
      dflags <- getDynFlags
      unqual <- getPrintUnqual
      return (showSDocForUser dflags unqual (pprTypeForUser kind))

doEval ::
  String -> Builder a -> (a -> Skc b) -> BL.ByteString -> Skc b
doEval label parser act input = do
  initSessionForMake
  case evalSP sexprs (Just label) input of
    Right form0 -> do
      form1 <- withExpanderSettings (expands form0)
      hthing <- buildHsSyn parser form1
      act hthing
    Left err -> failS err

evalSkEnv :: SkEnv
evalSkEnv = defaultSkEnv {envContextModules = modules}
  where
    modules = ["Prelude", "Language.SK"]
