module FnkTest where

-- base
import           Control.Exception
import qualified Control.Monad.Fail         as MonadFail
import           Control.Monad.IO.Class
import           Data.Maybe                 (fromMaybe, isNothing)

-- bytestring
import qualified Data.ByteString.Lazy.Char8 as BL

-- ghc-paths
import           GHC.Paths                  (libdir)

-- ghc
import           Exception                  (gbracket)
import           FastString                 (fsLit)
import           HscTypes                   (SourceError)
import           SrcLoc                     (GenLocated (..), unLoc)

-- hspec
import           Test.Hspec
import           Test.QuickCheck

-- Internal
import           Language.Finkel.Builder
import           Language.Finkel.Expand
import           Language.Finkel.Fnk
import           Language.Finkel.Form
import           Language.Finkel.Homoiconic
import           Language.Finkel.Make
import           Language.Finkel.Reader
import           Language.Finkel.Syntax

fnkTests :: Spec
fnkTests = do
  exceptionTest
  fromGhcTest
  gensymTest
  expandTest
  envTest

exceptionTest :: Spec
exceptionTest = do
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
      ret <- runFnk act defaultFnkEnv
      ret `shouldBe` (42 :: Int)

  describe "ExceptionMonad instance of Fnk" $
    it "should return 42" $ do
      let act = gbracket (return 21) return (\x -> return (x * 2))
      ret <- runFnk act defaultFnkEnv
      ret `shouldBe` (42 :: Int)

  describe "Handling FinkelException" $
    it "should return 42" $ do
      let act = handleFinkelException (\_ -> return (42 :: Int))
                                  (liftIO (throwIO (FinkelException "")))
      ret <- runFnk act defaultFnkEnv
      ret `shouldBe` 42

  describe "running Fnk action containing `failS'" $
    it "should throw FinkelException" $ do
      let p :: FinkelException -> Bool
          p (FinkelException m) = m == "foo"
          act = failS "foo"
      runFnk act defaultFnkEnv `shouldThrow` p

  describe "running Fnk action containing `fail'" $
    it "should throw FinkelException" $ do
      let p :: FinkelException -> Bool
          p (FinkelException m) = m == "foo"
          act = MonadFail.fail "foo"
      runFnk act defaultFnkEnv `shouldThrow` p

  describe "running Fnk action containing SourceError" $
    it "should throw SourceError" $ do
      let act = finkelSrcError nil "foo"
          p :: SourceError -> Bool
          p = const True
      runFnk act defaultFnkEnv `shouldThrow` p

  describe "applying macroNames to specialForms" $
    it "should not return name of special forms" $ do
      let ns = macroNames specialForms
      ns `shouldBe` []

  describe "running buildHsSyn action" $
    it "should throw SourceError" $ do
      let form = "(:: foo ->) (= foo 100)"
          sel :: SourceError -> Bool
          sel _ = True
          run a = runFnk a defaultFnkEnv
          build = do (form', _) <- parseSexprs Nothing (BL.pack form)
                     buildHsSyn parseDecls form'
      run build `shouldThrow` sel

fromGhcTest :: Spec
fromGhcTest =
  describe "converting Ghc to Fnk" $
    it "should return the returned value in Ghc" $ do
      let v :: Int
          v = 42
      x <- runFnk (fromGhc (return v)) defaultFnkEnv
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
          env = emptyFnkEnv {envMacros = specialForms
                            ,envLibDir = Just libdir}
      ret <- runFnk (f nil) env
      case ret of
        LForm (L _ (HsList [g1, g2])) -> g1 `shouldNotBe` g2
        _ -> expectationFailure "macro expansion failed"

expandTest :: Spec
expandTest = do
  let expand1_fn code =
        runFnk (macroFunction (Macro expand1) code) env
      env = emptyFnkEnv {envMacros = specialForms
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
      let mb_qt = lookupMacro (fsLit ":quote") env
          qt = fromMaybe (error "macro not found") mb_qt
          s x = LForm (genSrc (Atom (ASymbol (fsLit x))))
          li xs = LForm (genSrc (List xs))
          t x = LForm (genSrc (Atom (AString (fsLit x))))
          form0 = li [s ":quote", s "a"]
          form1 = li [s "qSymbol", t "a"]
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
   it "should have no language extensions" $
     let (ext, flagset) = envDefaultLangExts emptyFnkEnv
         empties = isNothing ext && flagSetToIntList flagset == []
     in  empties `shouldBe` True
   it "should have not set envSilent" $
     envSilent emptyFnkEnv `shouldBe` False
   it "should not have set envMake" $
     isNothing (envMake emptyFnkEnv) `shouldBe` True
   it "should not have required module names" $
     map unLoc (envRequiredModuleNames emptyFnkEnv) `shouldBe` []

emptyForm :: Code
emptyForm =
  let bgn = LForm (genSrc (Atom (ASymbol (fsLit ":begin"))))
  in  LForm (genSrc (List [bgn]))
