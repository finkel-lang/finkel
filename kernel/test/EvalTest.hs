{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash    #-}
module EvalTest (evalTests) where

-- base
import Control.Exception            (throwIO)
import Control.Monad.IO.Class       (MonadIO (..))
import GHC.Exts                     (unsafeCoerce#)
import System.Info                  (os)

-- filepath
import System.FilePath              (takeBaseName)

-- ghc
import Config                       (cProjectVersionInt)
import DynFlags                     (HasDynFlags (..))
import GHC                          (getPrintUnqual)
import GhcMonad                     (printException)
import HscTypes                     (handleSourceError)
import Outputable                   (showSDocForUser)
import PprTyThing                   (pprTypeForUser)
import StringBuffer                 (StringBuffer, hGetStringBuffer,
                                     stringToStringBuffer)


-- hspec
import Test.Hspec

-- finkel-kernel
import Language.Finkel.Builder      (Builder)
import Language.Finkel.Eval         (evalExpr, evalExprType, evalTypeKind)
import Language.Finkel.Expand       (expands, withExpanderSettings)
import Language.Finkel.Fnk          (Fnk, FnkEnv (..), failS, runFnk)
import Language.Finkel.Lexer        (evalSP)
import Language.Finkel.Make         (buildHsSyn)
import Language.Finkel.Reader       (sexprs)
import Language.Finkel.SpecialForms (defaultFnkEnv)
import Language.Finkel.Syntax       (parseExpr, parseType)

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
    it "should evaluate to True" work
  where
    work
      | cProjectVersionInt == "810"
      , os == "mingw32"
      , takeBaseName file `elem` skipped
      = pendingWith "Not yet supported"
      | otherwise
      = do contents <- hGetStringBuffer file
           ret <- runEvalExpr contents
           ret `shouldBe` True
    skipped = [ "0002-shadowing-macro"
              , "0004-unquote-unquote-splice" ]
    runEvalExpr !buf =
      runFnk (handleSourceError
                (\se -> printException se >> liftIO (throwIO se))
                (doEval "<exprTypeTest>" parseExpr act buf))
             evalFnkEnv
    act !expr = unsafeCoerce# $! evalExpr expr

exprTypeTest :: Spec
exprTypeTest =
  describe "type of True" $
    it "should be Bool" $ do
      ret <- runEvalType "True"
      ret `shouldBe` "Bool"
  where
    runEvalType str =
      let buf = stringToStringBuffer str
      in  runFnk (doEval "<exprTypeTest>" parseExpr act buf) evalFnkEnv
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
      let buf = stringToStringBuffer str
      in  runFnk (doEval "<typeKindTest>" parseType act buf) evalFnkEnv
    act expr = do
      (_, kind) <- evalTypeKind expr
      dflags <- getDynFlags
      unqual <- getPrintUnqual
      return (showSDocForUser dflags unqual (pprTypeForUser kind))

doEval :: String -> Builder a -> (a -> Fnk b) -> StringBuffer -> Fnk b
doEval !label !parser !act !input = do
  initSessionForTest
  case evalSP sexprs (Just label) input of
    Right form0 -> do
      !form1 <- withExpanderSettings (expands form0)
      !hthing <- buildHsSyn parser form1
      act hthing
    Left err -> failS err

evalFnkEnv :: FnkEnv
evalFnkEnv = defaultFnkEnv {envContextModules = modules}
  where
    modules = ["Prelude", "Language.Finkel"]
