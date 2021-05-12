{-# LANGUAGE CPP #-}

module FnkTest where

#include "ghc_modules.h"

-- base
import           Control.Exception            (throwIO)
import qualified Control.Monad.Fail           as MonadFail
import           Control.Monad.IO.Class
import           Data.Maybe                   (fromMaybe, isNothing)

-- ghc
import           GHC_Data_FastString          (fsLit)
import           GHC_Data_StringBuffer        (stringToStringBuffer)
import           GHC_Driver_Types             (ms_mod_name)
import           GHC_Types_SrcLoc             (GenLocated (..))
import           GHC_Unit_Module              (moduleNameString)

-- exceptions
import           Control.Monad.Catch          (bracket)

-- hspec
import           Test.Hspec
import           Test.QuickCheck

-- Internal
import           Language.Finkel.Builder
import           Language.Finkel.Exception
import           Language.Finkel.Expand
import           Language.Finkel.Fnk
import           Language.Finkel.Form
import           Language.Finkel.Homoiconic
import           Language.Finkel.Make
import           Language.Finkel.Reader
import           Language.Finkel.SpecialForms
import           Language.Finkel.Syntax

import           TestAux

fnkTests :: Spec
fnkTests = do
  exceptionTest
  fromGhcTest
  gensymTest
  expandTest
  envTest

exceptionTest :: Spec
exceptionTest = do

  let e_foo :: FinkelException
      e_foo = FinkelException "foo"

      test_e_foo :: FinkelException -> Bool
      test_e_foo e = case e of
        FinkelException msg -> msg == "foo"
        _                   -> False

      fnkSrcErrorSelector :: FinkelException -> Bool
      fnkSrcErrorSelector (FinkelSrcError {}) = True
      fnkSrcErrorSelector _                   = False

      run :: Fnk a -> IO a
      run = flip runFnk fnkTestEnv

  describe "Eq and Show instance of FinkelException" $ do
    it "should return True when comparing with itself" $
      property (\str -> let e1 = FinkelException str
                            e2 = FinkelException str
                        in  e1 == e2 && show e1 == show e2)
    it "should return False when message is different" $
      FinkelException "foo" /= FinkelException "bar" `shouldBe` True

  describe "Eq and Show instance of SyntaxError" $ do
    it "should return True when cmparing with itself" $
      property (\str -> let e1 = SyntaxError nil str
                            e2 = SyntaxError nil str
                        in  e1 == e2 && show e1 == show e2)
    it "should return False when message is different" $
      let e1 = SyntaxError nil "foo"
          e2 = SyntaxError nil "bar"
      in  e1 `shouldNotBe` e2

  describe "Code and message in SyntaxError" $ do
    let se = SyntaxError nil "message"
    it "should have given code" $
      syntaxErrCode se `shouldBe` nil
    it "should have given message" $
      syntaxErrMsg se `shouldBe` "message"

  describe "Applicative instance of Fnk" $
    it "should return 42" $ do
      let act = (*) <$> pure 6 <*> pure 7
      run act `shouldReturn` (42 :: Int)

  describe "ExceptionMonad instance of Fnk" $ do
    it "should return 42 with action in bracket" $ do
      let act = bracket (return 21) return (\x -> return (x * 2))
      run act `shouldReturn` (42 :: Int)

    it "should catch exception from throwM" $ do
      let act = throwM e `catch` handler
          e = FinkelException ""
          handler :: FinkelException -> Fnk Int
          handler _ = return 42
      run act `shouldReturn` 42

    it "masks an exception" $ do
      let act = mask (\restore -> restore (throwM e_foo))
      run act `shouldThrow` test_e_foo

    it "masks an exception (uninterruptible)" $ do
      let act = uninterruptibleMask (\restore -> restore (throwM e_foo))
      run act `shouldThrow` test_e_foo

  describe "Handling FinkelException" $
    it "should return 42" $ do
      let act = handleFinkelException
                  (\_ -> return (42 :: Int))
                  (liftIO (throwIO (FinkelException "")))
      run act `shouldReturn` 42

  describe "running Fnk action containing `failFnk'" $
    it "should throw FinkelException" $ do
      let act = failFnk "foo"
      run act `shouldThrow` test_e_foo

  describe "running Fnk action containing `fail'" $
    it "should throw FinkelException" $ do
      let act = MonadFail.fail "foo"
      run act `shouldThrow` test_e_foo

  describe "running Fnk action containing FinkelSrcError" $
    it "should throw SourceError" $ do
      let act = finkelSrcError nil "foo"
      run act `shouldThrow` fnkSrcErrorSelector

  describe "applying macroNames to specialForms" $
    it "should not return name of special forms" $ do
      let ns = macroNames specialForms
      ns `shouldBe` []

  describe "running buildHsSyn action" $
    it "should throw FinkelSrcError" $ do
      let form = "(:: foo ->) (= foo 100)"
          form' = stringToStringBuffer form
          build = do (form'', _) <- parseSexprs Nothing form'
                     buildHsSyn parseDecls form''
      run build `shouldThrow` fnkSrcErrorSelector

fromGhcTest :: Spec
fromGhcTest =
  describe "converting Ghc to Fnk" $
    it "should return the returned value in Ghc" $ do
      let v :: Int
          v = 42
      x <- runFnk (fromGhc (return v)) fnkTestEnv
      x `shouldBe` v

gensymTest :: Spec
gensymTest =
  describe "generating two gensyms" $
    it "should not be equal" $ do
      let gen _ = do
            g1 <- gensym
            g2 <- gensym
            return $ toCode [g1, g2]
          f = macroFunction (Macro gen)
          env = cleanFnkEnv
      ret <- runFnk (f nil) env
      case ret of
        LForm (L _ (HsList [g1, g2])) -> g1 `shouldNotBe` g2
        _ -> expectationFailure "macro expansion failed"

expandTest :: Spec
expandTest = do
  let expand1_fn code =
        runFnk (macroFunction (Macro expand1) code) env
      env = cleanFnkEnv
  describe "expand-1 of nil" $
    it "should return nil" $ do
      ret <- expand1_fn nil
      ret `shouldBe` nil
  describe "expand-1 of (:quote 42.0)" $
    it "should return non-empty form" $ do
      let form = toCode (List [toCode $ aSymbol ":quote"
                              ,toCode $ aFractional (42.0 :: Double)])
      ret <- expand1_fn form
      length ret `shouldSatisfy` (>= 1)
  describe "expand-1 of non-macro" $
    it "should return the original form" $ do
      let form = toCode (List [toCode $ aSymbol "show"
                              ,toCode $ aFractional (42 :: Double)])
      ret <- expand1_fn form
      ret `shouldBe` form
  describe "expanding with macroFunction" $
    it "should return empty form" $ do
      let mb_qt = lookupMacro (fsLit ":quasiquote") env
          qt = fromMaybe (error "macro not found") mb_qt
          s x = LForm (genSrc (Atom (ASymbol (fsLit x))))
          li xs = LForm (genSrc (List xs))
          form0 = li [s ":quasiquote", s "a"]
          form1 = li [s ":quote", s "a"]
      ret <- runFnk (macroFunction qt form0) env
      ret `shouldBe` form1

envTest :: Spec
envTest = do
  describe "deleting macro from specialForms" $
    it "should delete macro with matching name" $ do
      let m0 = specialForms
          m1 = deleteMacro (fsLit ":with-macro") m0
          e1 = emptyFnkEnv {envMacros = m1}
          mb_let_macro = lookupMacro (fsLit ":with-macro") e1
      isNothing mb_let_macro `shouldBe` True

  describe "deleting macro from emptyMacros" $
    it "should delete nothing" $ do
      let m0 = emptyEnvMacros
          m1 = deleteMacro (fsLit ":no-such-macro") m0
          n0 = macroNames m0
          n1 = macroNames m1
      n0 `shouldBe` n1

  describe "merging macros with itself" $
    it "should not change" $ do
      let m0 = specialForms
          m1 = mergeMacros specialForms specialForms
          n0 = macroNames m0
          n1 = macroNames m1
      n0 `shouldBe` n1

  describe "showing special forms" $
    it "should be <special-forms>" $ do
      let e1 = emptyFnkEnv {envMacros = specialForms}
          mb_let_macro = lookupMacro (fsLit ":eval-when-compile") e1
          let_macro = fromMaybe (error "not found") mb_let_macro
      show let_macro `shouldBe` "<special-form>"

  describe "empty finkel env" $ do
   it "should have empty envMacros" $
     macroNames (envMacros emptyFnkEnv) `shouldBe` []
   it "should have empty envDefaultMacros" $
     macroNames (envDefaultMacros emptyFnkEnv) `shouldBe` []
   it "should set verbosity to 1" $
     envVerbosity emptyFnkEnv `shouldBe` 1
   it "should not have required module names" $
     map (moduleNameString . ms_mod_name) (envRequiredHomeModules emptyFnkEnv)
     `shouldBe` []

emptyForm :: Code
emptyForm =
  let bgn = LForm (genSrc (Atom (ASymbol (fsLit ":begin"))))
  in  LForm (genSrc (List [bgn]))

cleanFnkEnv :: FnkEnv
cleanFnkEnv = fnkTestEnv {envDefaultMacros = emptyEnvMacros}
