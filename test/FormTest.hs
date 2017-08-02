-- | Tests for forms.
module FormTest where

import qualified Data.ByteString.Lazy.Char8 as BL
import Test.Hspec

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

  lengthTest 3 "(a b c)"
  lengthTest 5 "(a (b (c)) d e)"
  lengthTest 1 "()"

  codishTest (AInteger 42)
  codishTest ()
  codishTest 'x'
  codishTest "string"
  codishTest (42 :: Int)
  codishTest (42 :: Integer)
  codishTest (0.123456789 :: Double)
  codishTest ([1,2,3] :: [Int])
  codishTest (Atom (AInteger 42))
  codishTest (parseE "(foo bar buzz)")

readShow :: String -> Spec
readShow str =
  describe ("read and show `" ++ str ++ "'") $
    it "should match the input" $
      show (parseE str) `shouldBe` str

lengthTest :: Int -> String -> Spec
lengthTest n str =
  describe ("length of " ++ str) $
   it ("should be " ++ show n) $
     length (parseE str) `shouldBe` n

codishTest :: (Eq a, Show a, Codish a) => a -> Spec
codishTest x =
  describe ("to/from code " ++ show x) $ do
   it "should match the input" $
     case fromCode (toCode x) of
       Just y -> y `shouldBe` x

parseE :: String -> Code
parseE str =
  case runSP sexpr Nothing (BL.pack str) of
    Right (expr, _) -> expr
    Left err        -> error err
