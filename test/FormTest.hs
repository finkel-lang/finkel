-- | Tests for forms.
module FormTest where

import Data.List (isPrefixOf, isSubsequenceOf)
import qualified Data.ByteString.Lazy.Char8 as BL
import Test.Hspec
import Test.QuickCheck

import Language.SK.Homoiconic
import Language.SK.Form
import Language.SK.GHC
import Language.SK.Lexer
import Language.SK.Reader
import Language.SK.Run
import Language.SK.SKC

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

  functorTest "(a \"foo\" \\x [True False])"

  nameTest "foo"
  nameTest "bar-buzz-quux"

  eqTest "(a \"bcd\" \\e [f g] (h i))"

  locationTest Nothing "foo"
  locationTest (Just "locationTest") "foo"

  lengthTest 3 "(a b c)"
  lengthTest 5 "(a (b (c)) d e)"
  lengthTest 1 "()"
  lengthTest 8 "[a (b (c d e) [f g]) h]"

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

  gensymTest

readShow :: String -> Spec
readShow str =
  describe ("read and show `" ++ str ++ "'") $
    it "should match the input" $
      show (parseE str) `shouldBe` str

readShowFormProp :: Spec
readShowFormProp =
  describe "read and show form property" $
    it "should match the input" $
      let f form = parseE (show form) `eqForm` form
      in  property f

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

fracTest :: Double -> Spec
fracTest x =
  describe ("read and show a fractional number `" ++ show x ++ "'") $
    it "should match the input" $
       show (aFractional x) `shouldBe` show x

functorTest :: String -> Spec
functorTest str =
  describe ("Functor instance of Code `" ++ str ++ "'") $
    it "should obey the Functor law" $
       let c = parseE str
       in  fmap id c `shouldBe` c

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

parseE :: String -> Code
parseE = parseE' Nothing

parseE' :: Maybe FilePath -> String -> Code
parseE' mb_path str =
  case runSP sexpr mb_path (BL.pack str) of
    Right (expr, _) -> expr
    Left err        -> error err

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
