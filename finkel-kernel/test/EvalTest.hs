{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
{-# LANGUAGE MagicHash    #-}
module EvalTest (evalFnkTests) where

#include "ghc_modules.h"

-- base
import Control.Exception       (throwIO)
import Control.Monad.IO.Class  (MonadIO (..))
import GHC.Exts                (unsafeCoerce#)
import System.Info             (os)

-- filepath
import System.FilePath         (takeBaseName)

-- ghc
import GHC                     (getPrintUnqual, setContext)
import GHC_Core_Ppr_TyThing    (pprTypeForUser)
import GHC_Data_FastString     (fsLit)
import GHC_Data_StringBuffer   (StringBuffer, hGetStringBuffer,
                                stringToStringBuffer)
import GHC_Driver_Monad        (printException)
import GHC_Driver_Session      (HasDynFlags (..))
import GHC_Driver_Types        (InteractiveImport (..), handleSourceError)
import GHC_Hs_ImpExp           (simpleImportDecl)
import GHC_Runtime_Eval        (getContext)
import GHC_Settings_Config     (cProjectVersionInt)
import GHC_Unit_Module         (mkModuleNameFS)
import GHC_Utils_Outputable    (showSDocForUser)

-- hspec
import Test.Hspec

-- finkel-kernel
import Language.Finkel.Builder (Builder)
import Language.Finkel.Eval    (evalExpr, evalExprType, evalTypeKind)
import Language.Finkel.Expand  (expands, withExpanderSettings)
import Language.Finkel.Fnk     (Fnk, FnkEnv (..), failS, runFnk)
import Language.Finkel.Lexer   (evalSP)
import Language.Finkel.Make    (buildHsSyn)
import Language.Finkel.Reader  (sexprs)
import Language.Finkel.Syntax  (parseExpr, parseType)

-- Test internal
import TestAux


evalFnkTests :: FnkSpec
evalFnkTests = do
  files <- runIO (getTestFiles "eval")
  mapM_ exprTest files
  exprTypeTest
  typeKindTest

exprTest :: FilePath -> FnkSpec
exprTest file =
  describe file $
    it "should evaluate to True" work
  where
    work ftr
      | cProjectVersionInt == "810"
      , os == "mingw32"
      , takeBaseName file `elem` skipped
      = pendingWith "Not yet supported"
      | otherwise
      = do contents <- hGetStringBuffer file
           ret <- runEvalExpr ftr contents
           ret `shouldBe` True
    skipped = [ "0002-shadowing-macro"
              , "0004-unquote-unquote-splice" ]
    runEvalExpr ftr !buf =
      runFnk (handleSourceError
                (\se -> printException se >> liftIO (throwIO se))
                (doEval ftr "<exprTest>" parseExpr act buf))
             evalFnkEnv
    act !expr = unsafeCoerce# $! evalExpr expr

exprTypeTest :: FnkSpec
exprTypeTest =
  describe "type of True" $
    it "should be Bool" $ \ftr -> do
      ret <- runEvalType ftr "True"
      ret `shouldBe` "Bool"
  where
    runEvalType ftr str =
      let buf = stringToStringBuffer str
      in  runFnk (doEval ftr "<exprTypeTest>" parseExpr act buf) evalFnkEnv
    act expr  = do
      ty <- evalExprType expr
      dflags <- getDynFlags
      unqual <- getPrintUnqual
      return (showSDocForUser dflags unqual (pprTypeForUser ty))

typeKindTest :: FnkSpec
typeKindTest = do
  describe "kind of Maybe" $
    it "should be * -> *" $ \ftr -> do
      ret <- runTypeKind ftr "Maybe"
      ret `shouldBe` "* -> *"
  where
    runTypeKind ftr str =
      let buf = stringToStringBuffer str
      in  runFnk (doEval ftr "<typeKindTest>" parseType act buf) evalFnkEnv
    act expr = do
      (_, kind) <- evalTypeKind expr
      dflags <- getDynFlags
      unqual <- getPrintUnqual
      return (showSDocForUser dflags unqual (pprTypeForUser kind))

doEval :: FnkTestResource
       -> String -> Builder a -> (a -> Fnk b) -> StringBuffer -> Fnk b
doEval ftr !label !parser !act !input = do
  ftr_init ftr
  case evalSP sexprs (Just label) input of
    Right form0 -> do
      !form1 <- withExpanderSettings (prepare >> expands form0)
      !hthing <- buildHsSyn parser form1
      act hthing
    Left err -> failS err
  where
    -- Adding 'Prelude' and 'Language.Finkel' to interactive context, since the
    -- codes in the file does not contain ':require' forms.
    prepare = do
      ctxt <- getContext
      setContext (mkII "Prelude" : mkII "Language.Finkel" : ctxt)
    mkII = IIDecl . simpleImportDecl . mkModuleNameFS . fsLit

evalFnkEnv :: FnkEnv
evalFnkEnv = fnkTestEnv {envContextModules = modules}
  where
    modules = ["Prelude", "Language.Finkel"]
