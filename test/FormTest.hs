-- | Tests for forms.
module FormTest where

import Control.DeepSeq
import Control.Exception
import Data.Data
import Data.List (isPrefixOf, isSubsequenceOf)
import Test.Hspec
import Test.QuickCheck
import Text.Show.Functions ()

import Language.SK.Expand
import Language.SK.Form
import Language.SK.GHC
import Language.SK.Homoiconic
import Language.SK.Lexer
import Language.SK.Reader

import qualified Data.ByteString.Lazy.Char8 as BL

formTests :: Spec
formTests = do
  mapM_ readShow
    ["foo" ,"\a" ,"12345" ,"6.789"
    ,"(foo bar buzz)"
    ,"(\\a \\sp \\\\ \"bcd\")"
    ,"[\\bel \\bs \\ff \\lf \\cr \\ht \\vt]"
    ,"[()]"
    ,"(1 -2 345 6.789 0.001)"
    ]

  readShowFormProp

  fracTest 1.23
  fracTest (-1.23)
  fracTest 0
  fracTest 1e-9

  showTest
  functorTest "(a \"foo\" \\x [True False])"
  foldableTest

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

  homoiconicTest (AInteger 42)
  homoiconicTest ()
  homoiconicTest 'x'
  homoiconicTest "string"
  homoiconicTest (42 :: Int)
  homoiconicTest (42 :: Integer)
  homoiconicTest (0.123456789 :: Double)
  homoiconicTest (1.234 :: Float)
  homoiconicTest ([1,2,3] :: [Int])
  homoiconicTest (Atom (AInteger 42))
  homoiconicTest (parseE "(foo bar buzz)")
  homoiconicTest [True, False]
  homoiconicTest [EQ, LT, GT]
  homoiconicTest (Just (42 :: Int))
  homoiconicTest [Just 'a', Nothing, Just 'b']
  homoiconicTest [Right True, Left "foo"]
  homoiconicTest (Just 'x', [Right False, Left "foo"])
  homoiconicTest (Just 'x', [Right False, Left "foo"], EQ)
  homoiconicTest (Just 'x', [Right False, Left "foo"], EQ, (42::Int))
  homoiconicTest (Just 'x', [Right False, Left "foo"], EQ, (42::Int)
                 ,False)
  homoiconicTest (Just 'x', [Right False, Left "foo"], EQ, (42::Int)
                 ,False, Just [Right (Just EQ), Left (3.1 :: Double)])

  fromCodeTest Foo

  unquoteSpliceTest

readShow :: String -> Spec
readShow str =
  describe ("read and show `" ++ str ++ "'") $
    it "should match the input" $
      show (parseE str) `shouldBe` str

readShowFormProp :: Spec
readShowFormProp =
  describe "read and show form property" $
    it "should match the input" $
      property (\form ->
                  form == form && parseE (show form) `eqForm` form)

eqForm :: Code -> Code -> Bool
eqForm a b =
  case (unLocLForm a, unLocLForm b) of
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
    it ("should be 55 for " ++ str1) $ do
      fsum (parseE str1) `shouldBe` 55
    it "should be 0 for TEnd" $ do
      let sp = UnhelpfulSpan (fsLit "<foldableTest>")
      fsum (LForm (L sp TEnd)) `shouldBe` 0
  describe "length of nil" $ do
    it "should be 0" $
      length nil `shouldBe` 0

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

homoiconicTest :: (Eq a, Show a, Homoiconic a) => a -> Spec
homoiconicTest x =
  describe ("to/from code " ++ show x) $
   it "should match the input" $
     case fromCode (toCode x) of
       Just y -> y `shouldBe` x

data Foo = Foo deriving (Eq, Show)

instance Homoiconic Foo where
  toCode foo = toCode (aSymbol (show foo))

fromCodeTest :: Foo -> Spec
fromCodeTest foo =
  describe "default toCode implementation" $
    it "should return Nothing" $
      (fromCode nil :: Maybe Foo) `shouldBe` Nothing

unquoteSpliceTest :: Spec
unquoteSpliceTest = do
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
  case runSP sexpr mb_path (BL.pack str) of
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
