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
import GHC                     (getPrintUnqual)
import GHC_Data_StringBuffer   (StringBuffer, hGetStringBuffer,
                                stringToStringBuffer)
import GHC_Driver_Monad        (printException)
import GHC_Driver_Ppr          (showSDocForUser)
import GHC_Driver_Session      (HasDynFlags (..))
import GHC_Settings_Config     (cProjectVersionInt)

import GHC_Types_SourceError   (handleSourceError)
import GHC_Types_TyThing_Ppr   (pprTypeForUser)

import GHC_Utils_Outputable    (SDoc)

#if MIN_VERSION_ghc(9,2,0)
import GHC.Driver.Env          (hsc_units)
import GHC.Driver.Monad        (getSession)
#endif

-- hspec
import Test.Hspec

-- finkel-kernel
import Language.Finkel.Builder (Builder)
import Language.Finkel.Eval    (evalExpr, evalExprType, evalTypeKind)
import Language.Finkel.Fnk     (Fnk, FnkEnv (..), runFnk)
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
    act = fmap unsafeCoerce# . evalExpr

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
      pprDocForUser (pprTypeForUser ty)

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
      pprDocForUser (pprTypeForUser kind)

doEval :: FnkTestResource
       -> String -> Builder a -> (a -> Fnk b) -> StringBuffer -> Fnk b
doEval !ftr !label !parser !act !input = do
  ftr_init ftr
  evalWith label parser act input

evalFnkEnv :: FnkEnv
evalFnkEnv = fnkTestEnv {envContextModules = modules}
  where
    modules = ["Prelude", "Language.Finkel"]

pprDocForUser :: SDoc -> Fnk String
pprDocForUser sdoc = do
  dflags <- getDynFlags
  unqual <- getPrintUnqual
#if MIN_VERSION_ghc(9,2,0)
  unit_state <- hsc_units <$> getSession
  pure (showSDocForUser dflags unit_state unqual sdoc)
#else
  pure (showSDocForUser dflags unqual sdoc)
#endif
