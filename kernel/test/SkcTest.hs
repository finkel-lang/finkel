module SkcTest where

-- base
import Control.Exception
import Data.List (isPrefixOf, tails)
import Data.Maybe (fromMaybe)

-- bytestring
import qualified Data.ByteString.Lazy.Char8 as BL

-- containers
import qualified Data.Set as Set

-- ghc-paths
import GHC.Paths (libdir)

-- ghc
import GHC (runGhc)
import Exception (gbracket)
import HscTypes (SourceError(..))

-- hspec
import Test.Hspec
import Test.QuickCheck

-- Internal
import Language.SK.Expand
import Language.SK.Form
import Language.SK.Homoiconic
import Language.SK.Make
import Language.SK.Run
import Language.SK.SKC
import Language.SK.Syntax

skcTests :: Spec
skcTests = do
  exceptionTest
  fromGhcTest
  gensymTest
  expandTest

exceptionTest :: Spec
exceptionTest = do
  describe "Eq and Show instance of SkException" $
    it "should return True when comparing with self" $
      property (\str -> let e1 = SkException str
                            e2 = SkException str
                        in  e1 == e2 && show e1 == show e2)

  describe "Applicative instance of Skc" $
    it "should return 42" $ do
      let act = (*) <$> pure 6 <*> pure 7
      ret <- runSkcWithoutHandler act defaultSkEnv
      ret `shouldBe` Right 42

  describe "ExceptionMonad instance of Skc" $
    it "should return 42" $ do
      let act = gbracket (return 21) return (\x -> return (x * 2))
      ret <- runSkcWithoutHandler act defaultSkEnv
      ret `shouldBe` Right (42 :: Int)

  describe "running Skc action containing `failS'" $
    it "should throw SkException" $ do
      let p :: SkException -> Bool
          p (SkException m) = m == "foo"
          act = failS "foo"
      runSkcWithoutHandler act defaultSkEnv `shouldThrow` p

  describe "running Skc action containing SourceError" $
    it "should throw SourceError" $ do
      let act = toGhc (skSrcError nil "foo") defaultSkEnv
          p :: SourceError -> Bool
          p _ = True
      runGhc (Just libdir) act `shouldThrow` p

  describe "applying macroNames to specialForms" $
    it "should not return name of special forms" $ do
      let ns = macroNames specialForms
      ns `shouldBe` []

  describe "builderError and failB" $
    it "should throw SkException" $ do
      let form = "(:: foo (->)) (= foo 100)"
          sel :: SkException -> Bool
          sel (SkException msg) = subseq "syntax" msg
          run a = runSkcWithoutHandler a defaultSkEnv
          build = do (form', _) <- parseSexprs Nothing (BL.pack form)
                     buildHsSyn parseDecls form'
      run build `shouldThrow` sel

subseq :: Eq a => [a] -> [a] -> Bool
subseq xs ys = any (isPrefixOf xs) (tails ys)

fromGhcTest :: Spec
fromGhcTest =
  describe "converting Ghc to Skc" $
    it "should return the returned value in Ghc" $ do
      let v :: Int
          v = 42
      x <- runSkc (fromGhc (return v)) defaultSkEnv
      x `shouldBe` Right v

gensymTest :: Spec
gensymTest =
  describe "generating two gensyms" $
    it "should not be equal" $ do
      let gen _ = do
            g1 <- gensym
            g2 <- gensym
            return $ toCode [g1, g2]
          f = macroFunction (Macro gen)
      ret <- f nil
      case ret of
        Right (LForm (L _ (HsList [g1, g2]))) -> g1 `shouldNotBe` g2
        _ -> expectationFailure "macro expansion failed"

expandTest :: Spec
expandTest = do
  let expand1_fn = macroFunction (Macro expand1)
  describe "expand-1 of nil" $
    it "should return nil" $ do
      ret <- expand1_fn nil
      case ret of
        Right ret' -> ret' `shouldBe` nil
        _ -> expectationFailure "macro expansion failed"
  describe "expand-1 of (quote 42.0)" $
    it "should return non-empty form" $ do
      let form = toCode (List [ toCode $ aSymbol "quote"
                              , toCode $ aFractional 42.0])
      ret <- expand1_fn form
      case ret of
        Right ret' -> length ret' `shouldSatisfy` (>= 1)
        _ -> expectationFailure "macro expansion failed"
  describe "expanding with macroFunction" $
    it "should return empty form" $ do
      let env = emptySkEnv {envMacros = specialForms}
          mb_qt = lookupMacro (fsLit "quote") env
          qt = fromMaybe (error "macro not found") mb_qt
          l = skSrcSpan
          s x = LForm (L l (Atom (ASymbol (fsLit x))))
          li xs = LForm (L l (List xs))
          t x = LForm (L l (Atom (AString x)))
          form0 = li [s "quote", s "a"]
          form1 = li [s "quoted", li [s "Atom", li [s "aSymbol", t "a"]]]
      ret <- macroFunction qt form0
      ret `shouldBe` Right form1

emptyForm :: Code
emptyForm =
  let bgn = LForm (L skSrcSpan (Atom (ASymbol (fsLit "begin"))))
  in  LForm (L skSrcSpan (List [bgn]))
