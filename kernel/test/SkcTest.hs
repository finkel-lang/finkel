module SkcTest where

-- base
import Control.Exception
import Control.Monad.IO.Class
import Data.Maybe (fromMaybe, isNothing)
import qualified Control.Monad.Fail as MonadFail

-- bytestring
import qualified Data.ByteString.Lazy.Char8 as BL

-- ghc-paths
import GHC.Paths (libdir)

-- ghc
import FastString (fsLit)
import Exception (gbracket)
import HscTypes (SourceError)
import SrcLoc (GenLocated(..), unLoc)

-- hspec
import Test.Hspec
import Test.QuickCheck

-- Internal
import Language.SK.Builder
import Language.SK.Expand
import Language.SK.Form
import Language.SK.Homoiconic
import Language.SK.Make
import Language.SK.Reader
import Language.SK.SKC
import Language.SK.Syntax

skcTests :: Spec
skcTests = do
  exceptionTest
  fromGhcTest
  gensymTest
  expandTest
  envTest

exceptionTest :: Spec
exceptionTest = do
  describe "Eq and Show instance of SkException" $ do
    it "should return True when comparing with itself" $
      property (\str -> let e1 = SkException str
                            e2 = SkException str
                        in  e1 == e2 && show e1 == show e2)
    it "should return False when message is different" $
      SkException "foo" /= SkException "bar" `shouldBe` True

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

  describe "Applicative instance of Skc" $
    it "should return 42" $ do
      let act = (*) <$> pure 6 <*> pure 7
      ret <- runSkc act defaultSkEnv
      ret `shouldBe` (42 :: Int)

  describe "ExceptionMonad instance of Skc" $
    it "should return 42" $ do
      let act = gbracket (return 21) return (\x -> return (x * 2))
      ret <- runSkc act defaultSkEnv
      ret `shouldBe` (42 :: Int)

  describe "Handling SkException" $
    it "should return 42" $ do
      let act = handleSkException (\_ -> return (42 :: Int))
                                  (liftIO (throwIO (SkException "")))
      ret <- runSkc act defaultSkEnv
      ret `shouldBe` 42

  describe "running Skc action containing `failS'" $
    it "should throw SkException" $ do
      let p :: SkException -> Bool
          p (SkException m) = m == "foo"
          act = failS "foo"
      runSkc act defaultSkEnv `shouldThrow` p

  describe "running Skc action containing `fail'" $
    it "should throw SkException" $ do
      let p :: SkException -> Bool
          p (SkException m) = m == "foo"
          act = MonadFail.fail "foo"
      runSkc act defaultSkEnv `shouldThrow` p

  describe "running Skc action containing SourceError" $
    it "should throw SourceError" $ do
      let act = skSrcError nil "foo"
          p :: SourceError -> Bool
          p = const True
      runSkc act defaultSkEnv `shouldThrow` p

  describe "applying macroNames to specialForms" $
    it "should not return name of special forms" $ do
      let ns = macroNames specialForms
      ns `shouldBe` []

  describe "running buildHsSyn action" $
    it "should throw SourceError" $ do
      let form = "(:: foo ->) (= foo 100)"
          sel :: SourceError -> Bool
          sel _ = True
          run a = runSkc a defaultSkEnv
          build = do (form', _) <- parseSexprs Nothing (BL.pack form)
                     buildHsSyn parseDecls form'
      run build `shouldThrow` sel

fromGhcTest :: Spec
fromGhcTest =
  describe "converting Ghc to Skc" $
    it "should return the returned value in Ghc" $ do
      let v :: Int
          v = 42
      x <- runSkc (fromGhc (return v)) defaultSkEnv
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
          env = emptySkEnv {envMacros = specialForms
                           ,envLibDir = Just libdir}
      ret <- runSkc (f nil) env
      case ret of
        LForm (L _ (HsList [g1, g2])) -> g1 `shouldNotBe` g2
        _ -> expectationFailure "macro expansion failed"

expandTest :: Spec
expandTest = do
  let expand1_fn code =
        runSkc (macroFunction (Macro expand1) code) env
      env = emptySkEnv {envMacros = specialForms
                       ,envLibDir = Just libdir}
  describe "expand-1 of nil" $
    it "should return nil" $ do
      ret <- expand1_fn nil
      ret `shouldBe` nil
  describe "expand-1 of (quote 42.0)" $
    it "should return non-empty form" $ do
      let form = toCode (List [toCode $ aSymbol "quote"
                              ,toCode $ aFractional (42.0 :: Double)])
      ret <- expand1_fn form
      length ret `shouldSatisfy` (>= 1)
  describe "expanding with macroFunction" $
    it "should return empty form" $ do
      let mb_qt = lookupMacro (fsLit "quote") env
          qt = fromMaybe (error "macro not found") mb_qt
          s x = LForm (genSrc (Atom (ASymbol (fsLit x))))
          li xs = LForm (genSrc (List xs))
          t x = LForm (genSrc (Atom (AString x)))
          form0 = li [s "quote", s "a"]
          form1 = li [s "qSymbol", t "a"]
      ret <- runSkc (macroFunction qt form0) env
      ret `shouldBe` form1

envTest :: Spec
envTest = do
  describe "deleting macro from specialForms" $
    it "should delete macro with matching name" $ do
      let m0 = specialForms
          m1 = deleteMacro (fsLit "let-macro") m0
          e1 = emptySkEnv {envMacros = m1}
          mb_let_macro = lookupMacro (fsLit "let-macro") e1
      isNothing mb_let_macro `shouldBe` True

  describe "deleting macro from emptyMacros" $
    it "should delete nothing" $ do
      let m0 = emptyEnvMacros
          m1 = deleteMacro (fsLit "let-macro") m0
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
      let e1 = emptySkEnv {envMacros = specialForms}
          mb_let_macro = lookupMacro (fsLit "eval_when_compile") e1
          let_macro = fromMaybe (error "not found") mb_let_macro
      show let_macro `shouldBe` "<special-form>"

  describe "empty sk env" $ do
   it "should have empty envMacros" $
     macroNames (envMacros emptySkEnv) `shouldBe` []
   it "should have empty envDefaultMacros" $
     macroNames (envDefaultMacros emptySkEnv) `shouldBe` []
   it "should have no language extensions" $
     let (ext, flagset) = envDefaultLangExts emptySkEnv
         empties = isNothing ext && flagSetToIntList flagset == []
     in  empties `shouldBe` True
   it "should have not set envSilent" $
     envSilent emptySkEnv `shouldBe` False
   it "should not have set envMake" $
     isNothing (envMake emptySkEnv) `shouldBe` True
   it "should not have required module names" $
     map unLoc (envRequiredModuleNames emptySkEnv) `shouldBe` []

emptyForm :: Code
emptyForm =
  let bgn = LForm (genSrc (Atom (ASymbol (fsLit "begin"))))
  in  LForm (genSrc (List [bgn]))
