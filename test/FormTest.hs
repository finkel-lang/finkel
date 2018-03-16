{-# LANGUAGE DeriveDataTypeable #-}
-- | Tests for forms.
module FormTest where

import Control.DeepSeq
import Control.Exception
import Data.Complex
import Data.Data
import qualified Data.Fixed as Fixed
import Data.Functor.Compose
import Data.Functor.Const
import Data.Functor.Identity
import qualified Data.Functor.Product as Product
import qualified Data.Functor.Sum as Sum
import Data.Int
import Data.List (isPrefixOf, isSubsequenceOf)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid
import Data.Version
import Data.Ratio
import qualified Data.Semigroup as Semigroup
import Data.Word
import Numeric.Natural
import Test.Hspec
import Test.QuickCheck
import Text.Show.Functions ()

import Language.SK.Expand
import Language.SK.Form
import Language.SK.GHC
import Language.SK.Homoiconic
import Language.SK.Lexer
import Language.SK.Reader

import Data.ByteString.Builder (stringUtf8, toLazyByteString)

formTests :: Spec
formTests = do
  mapM_ readShow
    [ "foo", "\a", "12345", "6.789"
    , "(foo bar buzz)"
    , "(\\a \\\\SP \\\\ \"bcd\")"
    , "[\\\\BEL \\\\BS \\\\FF \\\\LF \\\\CR \\\\HT \\\\VT]"
    , "[()]"
    , "(1 -2 345 6.789 0.001)" ]

  readUnicodeStringProp
  readShowFormProp

  fracTest 1.23
  fracTest (-1.23)
  fracTest 0
  fracTest 1e-9

  showTest
  functorTest "(a \"foo\" \\x [True False])"
  foldableTest
  traversableTest

  nameTest "foo"
  nameTest "bar-buzz-quux"

  eqTest "(a \"bcd\" \\e [f g] (h i))"
  eqPropTest

  locationTest Nothing "foo"
  locationTest (Just "locationTest") "foo"

  lengthTest 3 "(a b c)"
  lengthTest 5 "(a (b (c)) d e)"
  lengthTest 1 "()"
  lengthTest 8 "[a (b (c d e) [f g]) h]"
  lengthTest 1 "foo"

  rnfTest
  listTest
  symbolNameTest
  homoiconicTests

  fromCodeTest Foo

  dataToCodeTest

  unquoteSpliceTest

readShow :: String -> Spec
readShow str =
  describe ("read and show `" ++ str ++ "'") $
    it "should match the input" $
      show (parseE str) `shouldBe` str

readUnicodeStringProp :: Spec
readUnicodeStringProp =
  describe "read and show unicode string" $
    it "should return itself" $
      property (\uni ->
                  let str = getUnicodeString uni
                  in  parseE (show str) == toCode (AString str))

readShowFormProp :: Spec
readShowFormProp =
  describe "read and show form property" $
    it "should match the input" $
      property (\form ->
                  form == form && parseE (show form) `eqForm` form)

eqForm :: Code -> Code -> Bool
eqForm a b =
  case (unCode a, unCode b) of
    -- Ignoring rounding error for fractional literals.
    (Atom (AFractional x), Atom (AFractional y))
      -> abs (fl_value x - fl_value y) <= toRational epsilon

    -- Recursively compare with `eqForm' for 'List' and 'HsList'.
    (List [], List []) -> True
    (List (x:xs), List (y:ys)) ->
      eqForm x y && eqForm (quoted (List xs)) (quoted (List ys))

    (HsList [], HsList []) -> True
    (HsList (x:xs), HsList (y:ys)) ->
      eqForm x y && eqForm (quoted (HsList xs)) (quoted (HsList ys))

    -- Treating empty 'List' and Atom symbol 'nil' as same value.
    (Atom (ASymbol sym), List []) | sym == fsLit "nil" -> True
    (List [], Atom (ASymbol sym)) | sym == fsLit "nil" -> True

    -- Using '==' for other Atom values.
    (Atom x, Atom y) -> x == y

    _ -> False
  where
    epsilon = 1e-7

rnfTest :: Spec
rnfTest = do
  describe "rnf of arbitrary form" $
    it "should return ()" $
       property (rnf :: Code -> ())
  describe "rnf of comment" $
    it "should return ()" $
      rnf (AComment "foo") `shouldBe` ()
  describe "rnf of TEnd" $
    it "should return ()" $
      rnf (TEnd :: Form Atom) `shouldBe` ()

listTest :: Spec
listTest =
  describe "list from arbitrary form applied to arbitrary function" $
    it "should be a list" $ do
      let f :: (Code -> Code) -> Code -> Bool
          f g form = isListL (toListL (g form))
      property f

symbolNameTest :: Spec
symbolNameTest =
  describe "symbolName" $ do
    let isSym (LForm (L _ (Atom (ASymbol _)))) = True
        isSym _                                = False
    it "should return name of symbol" $
      property (\form ->
                  not (isSym form) || length (symbolName form) >= 1)
    it "should throw error when applied to non-symbol" $ do
      let p :: ErrorCall -> Bool
          p _ = True
      print (symbolName nil) `shouldThrow` p

fracTest :: Double -> Spec
fracTest x =
  describe ("read and show a fractional number `" ++ show x ++ "'") $
    it "should match the input" $
       show (aFractional x) `shouldBe` show x

showTest :: Spec
showTest = do
  describe "showing comment" $
    it "should be empty string" $
      show (AComment "foo bar buzz") `shouldBe` ""
  describe "showing TEnd" $
    it "should be \"TEnd\"" $
      show (TEnd :: Form Atom) `shouldBe` "TEnd"

functorTest :: String -> Spec
functorTest str = do
  describe ("Functor instance of Code `" ++ str ++ "'") $
    it "should obey the Functor law" $
       let c = parseE str
       in  fmap id c `shouldBe` c
  describe "fmap to TEnd" $
    it "should be TEnd" $ do
      let te :: Form Atom
          te = TEnd
          f :: Atom -> Atom
          f _ = AUnit
      fmap f te `shouldBe` te

foldableTest :: Spec
foldableTest = do
  let fsum = foldr (\x acc -> case x of
                                AInteger n -> acc + n
                                _ -> acc)
                   0
  describe "taking sum of 1 to 10 with foldr" $ do
    let str1 = "(1 2 3 4 5 6 7 8 9 10)"
    it ("should be 55 for " ++ str1) $
      fsum (parseE str1) `shouldBe` 55
    it "should be 0 for TEnd" $ do
      let sp = UnhelpfulSpan (fsLit "<foldableTest>")
      fsum (LForm (L sp TEnd)) `shouldBe` 0
  describe "length of nil" $
    it "should be 0" $
      length nil `shouldBe` 0

traversableTest :: Spec
traversableTest = do
  let f atom = case atom of
                 ASymbol sym | '$' : _ <- unpackFS sym
                             -> return (aSymbol "_")
                 _ -> return atom
  describe "replacing symbol with mapM" $ do
    let str1 = "(a $b c [$d e] [f g $h] ($i $j))"
        str2 = "(a  _ c [ _ e] [f g  _] ( _  _))"
    it "should replace $.* with _" $ do
      let form1 = parseE str1
          form2 = parseE str2
      mapM f form1 `shouldBe` Just form2
  describe "traversing TEnd" $
    it "should be Just TEnd" $
      mapM f TEnd `shouldBe` Just TEnd

nameTest :: String -> Spec
nameTest str =
  describe ("name of symbol `" ++ str ++ "'") $
    it ("should be `" ++ str ++ "'") $
       symbolName (parseE str) `shouldBe` str

eqTest :: String -> Spec
eqTest str =
  describe "parsing same string twice" $
   it "should result in equal codes" $
     let c1 = parseE str
         c2 = parseE str
     in  c1 `shouldBe` c2

eqPropTest :: Spec
eqPropTest = do
  describe "Eq instance for LForm" $
    it "should ignore location information" $ do
      let g :: Code -> Bool
          g x@(LForm (L _ body)) = x == LForm (L sp body)
          sp = UnhelpfulSpan (fsLit "<eqPropTest>")
      property g
  describe "comparing comment with comment" $ do
    let c1 = AComment "foo"
        c2 = AComment "bar"
    it "should be True" $
      c1 `shouldBe` c1
    it "should be False" $
      c1 `shouldNotBe` c2
  describe "comparing TEnd with TEnd" $
    it "should be True" $
      (TEnd :: Form Atom) `shouldBe` (TEnd :: Form Atom)

locationTest :: Maybe FilePath -> String -> Spec
locationTest mb_path str =
  describe ("location of `" ++ str ++ "'") $
    it (case mb_path of
          Just path -> "should contain `" ++ path ++ "'"
          Nothing   -> "should be unhelpful") $ do
       let c = parseE' mb_path str
           l = showLoc c
       case mb_path of
         Just path -> (path `isPrefixOf` l) `shouldBe` True
         Nothing   -> ("anon" `isSubsequenceOf` l) `shouldBe` True

lengthTest :: Int -> String -> Spec
lengthTest n str =
  describe ("length of " ++ str) $
   it ("should be " ++ show n) $
     length (parseE str) `shouldBe` n

homoiconicTests :: Spec
homoiconicTests = do
  let t x = describe ("to/from code " ++ show x) $
              it "shoult match the input" $
                 case fromCode (toCode x) of
                   Just y -> y `shouldBe` x
  t (AInteger 42)
  t ()
  t 'x'
  t "string"
  t (42 :: Int)
  t (42 :: Int8)
  t (42 :: Int16)
  t (42 :: Int32)
  t (42 :: Int64)
  t (42 :: Integer)
  t (42 :: Word)
  t (42 :: Word8)
  t (42 :: Word16)
  t (42 :: Word32)
  t (42 :: Word64)
  t (0.123456789 :: Double)
  t (1.234 :: Float)
  t ([1,2,3] :: [Int])
  t (Fixed.MkFixed 2 :: Fixed.Pico)
  t (Identity 'a')
  t (1 :+ 2 :: Complex Int)
  t (Compose (Just (Just 'a')))
  t (Const True :: Const Bool Char)
  t (Product.Pair (Just 'a') (Just 'b'))
  t [Sum.InL (Just 'a'), Sum.InR (Right 'b'), Sum.InR (Left "foo")]
  t ('a' :| ['b', 'c', 'd'])
  t (All False)
  t (Alt (Just True))
  t (Any False)
  t (Dual 'x')
  t (First (Just 'a'))
  t (Last (Just 'a'))
  t (Product (42 :: Int))
  t (Sum (42 :: Int))
  t (Proxy :: Proxy ())
  t (Version [1,2,3] ["foo", "bar"])
  t (1 % 3 :: Rational)
  t (Semigroup.Arg 'x' False)
  t (Semigroup.First 'a')
  t (Semigroup.Last 'z')
  t (Semigroup.Max (42 :: Int))
  t (Semigroup.Min (42 :: Int))
  t (Semigroup.Option (Just "foo"))
  t (Semigroup.WrapMonoid True)
  t (42 :: Natural)
  t (Atom (AInteger 42))
  t (parseE "(foo bar buzz)")
  t [True, False]
  t [EQ, LT, GT]
  t (Just (42 :: Int))
  t [Right True, Left "foo"]
  t (Just 'x', [Right False, Left "foo"])
  t (Just 'x', [Right False, Left "foo"], EQ)
  t (Just 'x', [Right False, Left "foo"], EQ, (42::Int))
  t (Just 'x', [Right False, Left "foo"], EQ, (42::Int) ,False)
  t (Just 'x', [Right False, Left "foo"], EQ, (42::Int)
    ,False, Just [Right (Just EQ), Left (3.1 :: Double)])

data Foo = Foo deriving (Eq, Show)

instance Homoiconic Foo where
  toCode foo = toCode (aSymbol (show foo))

fromCodeTest :: Foo -> Spec
fromCodeTest foo =
  describe "default toCode implementation" $
    it "should return Nothing" $
      (fromCode nil :: Maybe Foo) `shouldBe` Nothing

data D1 = D1a | D1b | D1c
  deriving (Eq, Show, Data, Typeable)

instance Homoiconic D1

data D2 a = D2a a | D2b a a
  deriving (Eq, Show, Data, Typeable)

instance Data a => Homoiconic (D2 a)

data D3 a b = D3a Int a b
  deriving (Eq, Show, Data, Typeable)

instance (Data a, Data b) => Homoiconic (D3 a b)

data D4 a = D4a (a, a, a, a)
  deriving (Eq, Show, Data, Typeable)

instance Data a => Homoiconic (D4 a)

dataToCodeTest :: Spec
dataToCodeTest = do
  let s = toCode . aSymbol
  describe "D1 to Code" $ do
    let e1 = toCode (HsList (map s ["D1a", "D1b", "D1c"]))
    it ("should match `" ++ show e1 ++ "'") $
      toCode [D1a, D1b, D1c] `shouldBe` e1
  describe "D2 to Code" $ do
    let e2 = toCode
               (HsList
                  [ toCode (List [s "D2a", toCode 'x'])
                  , toCode (List [s "D2b", toCode 'y', toCode 'z'])])
    it ("should match `" ++ show e2 ++ "'") $
      toCode [D2a 'x', D2b 'y' 'z'] `shouldBe` e2
  describe "D2 with D1 to Code" $ do
    let e2b = toCode (List [s "D2b", s "D1a", s "D1b"])
    it ("should match `" ++ show e2b ++ "'") $
      toCode (D2b D1a D1b) `shouldBe` e2b
  describe "D2 with Double to Code" $ do
    let e2c = toCode (List [s "D2a", toCode (1.23 :: Double)])
    it ("should match `" ++ show e2c ++ "'") $
      toCode (D2a (1.23 :: Double)) `shouldBe` e2c
  describe "D3 to Code" $ do
    let e3 = toCode (List [ s "D3a"
                          , toCode (42 :: Int)
                          , toCode False
                          , toCode 'a' ])
    it ("should match `" ++ show e3 ++ "'") $
      toCode (D3a 42 False 'a') `shouldBe` e3
  describe "D4 to Code" $ do
    let e4 = toCode (List [ s "D4a"
                          , toCode (List [ s ","
                                         , toCode 'w'
                                         , toCode 'x'
                                         , toCode 'y'
                                         , toCode 'z'])])
    it ("should match `" ++ show e4 ++ "'") $
       toCode (D4a ('w', 'x', 'y', 'z')) `shouldBe` e4


unquoteSpliceTest :: Spec
unquoteSpliceTest =
  describe "unquote splicing List" $
    it "should return list contents" $
      property
        (\form ->
           if (isListL form || isHsListL form ||
               isStringL form || isUnitL form)
             then (0 <= length (unquoteSplice form)) === True
             else expectFailure (unquoteSplice form === []))

parseE :: String -> Code
parseE = parseE' Nothing

parseE' :: Maybe FilePath -> String -> Code
parseE' mb_path str =
  case runSP sexpr mb_path (toLazyByteString (stringUtf8 str)) of
    Right (expr, _) -> expr
    Left err        -> error err

isListL :: Code -> Bool
isListL (LForm (L _ (List _))) = True
isListL _                      = False

isHsListL :: Code -> Bool
isHsListL (LForm (L _ (HsList _))) = True
isHsListL _                        = False

isStringL :: Code -> Bool
isStringL (LForm (L _ (Atom (AString _)))) = True
isStringL _                                = False

isUnitL :: Code -> Bool
isUnitL (LForm (L _ (Atom AUnit))) = True
isUnitL _                          = False
