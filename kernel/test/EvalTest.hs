module EvalTest (evalTests) where

-- base
import Unsafe.Coerce

-- ghc
import DynFlags                (HasDynFlags (..))
import GHC                     (getPrintUnqual)
import Outputable              (showSDocForUser)
import PprTyThing              (pprTypeForUser)
import StringBuffer            (StringBuffer, hGetStringBuffer,
                                stringToStringBuffer)

-- hspec
import Test.Hspec

-- finkel-kernel
import Language.Finkel.Builder (Builder)
import Language.Finkel.Eval    (evalExpr, evalExprType, evalTypeKind)
import Language.Finkel.Expand  (expands, withExpanderSettings)
import Language.Finkel.Fnk     (Fnk, FnkEnv (..), failS, runFnk)
import Language.Finkel.Lexer   (evalSP)
import Language.Finkel.Make    (buildHsSyn, defaultFnkEnv)
import Language.Finkel.Reader  (sexprs)
import Language.Finkel.Syntax  (parseExpr, parseType)

-- Test internal
import TestAux

evalTests :: [FilePath] -> Spec
evalTests exprFiles = do
  mapM_ exprTest exprFiles
  exprTypeTest
  typeKindTest

exprTest :: FilePath -> Spec
exprTest file =
  describe file $
    it "should evaluate to True" $ do
      contents <- hGetStringBuffer file
      ret <- evalContents contents
      ret `shouldBe` True
  where
    evalContents buf =
      runFnk (doEval "<exprTypeTest>" parseExpr act buf) evalFnkEnv
    act expr = do
      hval <- evalExpr expr
      return (unsafeCoerce hval)

exprTypeTest :: Spec
exprTypeTest =
  describe "type of True" $
    it "should be Bool" $ do
      ret <- runEvalType "True"
      ret `shouldBe` "Bool"
  where
    runEvalType str =
      let str' = stringToStringBuffer str
      in runFnk (doEval "<exprTypeTest>" parseExpr act str')
                evalFnkEnv
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
      ret `shouldBe` "* -> *"
  where
    runTypeKind str =
      let str' = stringToStringBuffer str
      in  runFnk (doEval "<typeKindTest>" parseType act str')
                 evalFnkEnv
    act expr = do
      (_, kind) <- evalTypeKind expr
      dflags <- getDynFlags
      unqual <- getPrintUnqual
      return (showSDocForUser dflags unqual (pprTypeForUser kind))

doEval ::
  String -> Builder a -> (a -> Fnk b) -> StringBuffer -> Fnk b
doEval label parser act input = do
  initSessionForTest
  case evalSP sexprs (Just label) input of
    Right form0 -> do
      form1 <- withExpanderSettings (expands form0)
      hthing <- buildHsSyn parser form1
      act hthing
    Left err -> failS err

evalFnkEnv :: FnkEnv
evalFnkEnv = defaultFnkEnv {envContextModules = modules}
  where
    modules = ["Prelude", "Language.Finkel"]
