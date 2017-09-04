-- | Tests for forms.
module FormTest where

import Data.List (isPrefixOf, isSubsequenceOf)
import qualified Data.ByteString.Lazy.Char8 as BL
import Test.Hspec

import Language.SK.Homoiconic
import Language.SK.Form
import Language.SK.Lexer
import Language.SK.Reader

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

readShow :: String -> Spec
readShow str =
  describe ("read and show `" ++ str ++ "'") $
    it "should match the input" $
      show (parseE str) `shouldBe` str

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
  describe ("parsing same string twice") $
   it "should result in equal codes" $
     let c1 = parseE str
         c2 = parseE str
     in  c1 == c2 `shouldBe` True

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
  describe ("to/from code " ++ show x) $ do
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
