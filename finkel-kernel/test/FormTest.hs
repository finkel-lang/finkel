{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | Tests for forms.
module FormTest where

#include "ghc_modules.h"

-- base
import           Control.Applicative          (Alternative (..))
import           Data.Char                    (toUpper)
import           Data.Complex
import           Data.Data
import qualified Data.Fixed                   as Fixed
import           Data.Functor.Compose
import           Data.Functor.Const
import           Data.Functor.Identity
import qualified Data.Functor.Product         as Product
import qualified Data.Functor.Sum             as Sum
import           Data.Int
import           Data.List                    (isPrefixOf, isSubsequenceOf)
import           Data.List.NonEmpty           (NonEmpty (..))
import           Data.Monoid
import           Data.Ratio
import qualified Data.Semigroup               as Semigroup
import           Data.Version
import           Data.Word
import           Numeric.Natural
import           Text.Show.Functions          ()

#if !MIN_VERSION_ghc(8,4,0)
import           Data.Monoid                  ((<>))
#endif

-- binary
import           Data.Binary                  (decode, encode)

-- deepseq
import           Control.DeepSeq

-- ghc
import           GHC_Data_FastString          (fsLit, unpackFS)
import           GHC_Data_StringBuffer        (stringToStringBuffer)
import           GHC_Driver_Session           (HasDynFlags (..))
import           GHC_Types_Basic              (SourceText (..), fl_value)
import           GHC_Types_SrcLoc             (GenLocated (..), SrcSpan (..),
                                               noSrcSpan)
import           GHC_Utils_Outputable         (showPpr)

#if MIN_VERSION_ghc(9,0,0)
import           GHC_Types_SrcLoc             (UnhelpfulSpanReason (..))
#endif

-- transformers
import           Control.Monad.Trans.State

-- hspec
import           Test.Hspec

-- QuickCheck
import           Test.QuickCheck

-- finkel-kernel
import           Language.Finkel.Fnk
import           Language.Finkel.Form
import           Language.Finkel.Homoiconic
import           Language.Finkel.Lexer
import           Language.Finkel.Reader
import           Language.Finkel.SpecialForms

-- Internal
import           Orphan                       ()

formTests :: Spec
formTests = do
  mapM_ readShow
    [ "foo", "\a", "12345", "6.789", "0.001"
    , "(foo bar buzz)"
    , "(\\a \\\\SP \\\\ \"bcd\")"
    , "[\\\\BEL \\\\BS \\\\FF \\\\LF \\\\CR \\\\HT \\\\VT]"
    , "[()]"
    , "(1 -2 345 6.789 0.001)" ]

  readUnicodeStringProp
  readShowFormProp

  dataInstanceTests
  qFunctionTests

  fracTest 1.23
  fracTest (-1.23)
  fracTest 0
  fracTest 1e-9

  showTest
  pprTest
  functorTest "(a \"foo\" \\x [True False])"
  applicativeTest
  monadTest
  foldableTest
  traversableTest
  binaryTest

  eqTest "(a \"bcd\" \\e [f g] (h i))"
  eqPropTest

  locationTest Nothing "foo"
  locationTest (Just "locationTest") "foo"
  asLocOfTest

  lengthTest 3 "(a b c)"
  lengthTest 5 "(a (b (c)) d e)"
  lengthTest 1 "()"
  lengthTest 8 "[a (b (c d e) [f g]) h]"
  lengthTest 1 "foo"

  homoiconicTests
  rnfTest
  listTest
  numTest
  fractionalTest
  monoidTest
  alternativeTest

  fromCodeTest

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
                  in  parseE (show str) == toCode (aString NoSourceText str))

readShowFormProp :: Spec
readShowFormProp =
  describe "read and show form property" $
    it "should match the input" $
      property (\form ->
                  form == form && parseE (show form) `eqForm` form)

dataInstanceTests :: Spec
dataInstanceTests = do
  let gfoldl_self atom =
         gfoldl (\(Just f) x -> return (f x)) return atom
      t_gfoldl_self x = gfoldl_self x `shouldBe` Just x
      t_show_constr x y = show (toConstr x) `shouldBe` y
  describe "Data instance for Atom" $ do
    let aunit = AUnit
        asym = ASymbol (fsLit "foo")
        achar = AChar NoSourceText 'a'
        astr = AString NoSourceText (fsLit "string")
        aint = AInteger (mkIntegralLit (42 :: Int))
        afrac = aFractional (1.23 :: Double)
    it "should return Just self with simple gfoldl" $ do
      t_gfoldl_self aunit
      t_gfoldl_self asym
      t_gfoldl_self achar
      t_gfoldl_self astr
      t_gfoldl_self aint
      t_gfoldl_self afrac
    it "should show itself with toConstr" $ do
      t_show_constr aunit "AUnit"
      t_show_constr asym "ASymbol"
      t_show_constr achar "AChar"
      t_show_constr astr "AString"
      t_show_constr aint "AInteger"
      t_show_constr afrac "AFractional"
    it "should return AUnit with simple gunfold" $ do
      gunfold (const Nothing) Just (toConstr AUnit) `shouldBe` Just AUnit
    it "should return AUnit constr" $ do
      let dtype = dataTypeOf AUnit
          cnstr = toConstr AUnit
      readConstr dtype "AUnit" `shouldBe` Just cnstr
  describe "Data instance for Form" $ do
    let fatom = Atom AUnit
        qc c = qChar c "" 0 0 0 0
        flist = List [qc 'a', qc 'b']
        fhslist = HsList [qc 'a', qc 'b']
        ftend :: Form Atom
        ftend = TEnd
    it "should return Just self with simple gfoldl" $ do
      t_gfoldl_self fatom
      t_gfoldl_self flist
      t_gfoldl_self fhslist
      t_gfoldl_self ftend
    it "should show itself with toConstr" $ do
      t_show_constr fatom "Atom"
      t_show_constr flist "List"
      t_show_constr fhslist "HsList"
      t_show_constr ftend "TEnd"
    it "should return TEnd with simple gunfold" $
      gunfold (const Nothing) Just (toConstr ftend) `shouldBe` Just ftend
    it "should return Atom constr" $ do
      let dtype = dataTypeOf fatom
          cnstr = toConstr fatom
      readConstr dtype "Atom" `shouldBe` Just cnstr
    it "should return same result from dataCast1 and gcast1" $ do
      (dataCast1 [TEnd] :: Maybe [Form Atom]) `shouldBe` Just [TEnd]
  describe "Data instance for LForm" $ do
    let qc = qChar 'x' "" 0 0 0 0
        d1, d2, d3 :: Data a => a
        d1 = fromConstr (toConstr noSrcSpan)
        d2 = fromConstrB (fromConstr (toConstr AUnit))
                         (toConstr (Atom AUnit))
        d3 = evalState m 0
          where
            m = fromConstrM act (toConstr (L noSrcSpan (Atom AUnit)))
            act :: Data d => State Int d
            act = do
              i <- get
              modify succ
              case i of
                0 -> return d1
                1 -> return d2
                _ -> error ("index " ++ show i ++ " for LForm")
        d4 = fromConstrB d3 (toConstr qu)
        gc1 :: Data a => Maybe [LForm a]
        gc1 =
          let a :: Data a => a
              a = fromConstr (toConstr AUnit)
          in  dataCast1 [LForm (L noSrcSpan (Atom a))]
        qu = qUnit "" 0 0 0 0
    it "should return Just self with simple gfoldl" $ do
      t_gfoldl_self qc
    it "should show itself with toConstr" $ do
      t_show_constr qc "LForm"
    it "should return LForm constr" $ do
      let dtype = dataTypeOf qc
          cnstr = toConstr qc
      readConstr dtype "LForm" `shouldBe` Just cnstr
    it "should construct qUnit from constructors" $ do
      d4 `shouldBe` qu
    it "should return qUnit from dataCast1" $
      gc1 `shouldBe` Just [qu]

qFunctionTests :: Spec
qFunctionTests = do
  describe "qSymbol function" $
    it "should equal to quoted symbol" $
      qSymbol "foo" "" 0 0 0 0 `shouldBe` toCode (ASymbol (fsLit "foo"))
  describe "qChar function" $
    it "should equal to quoted char" $
      qChar 'x' "" 0 0 0 0 `shouldBe` toCode 'x'
  describe "qString function" $
    it "should equal to quoted string" $
      qString "foo" "" 0 0 0 0 `shouldBe` toCode "foo"
  describe "qInteger function" $
    it "should equal to quoted integer" $
      qInteger 42 "" 0 0 0 0 `shouldBe` toCode (42 :: Integer)
  describe "qFractional function" $
    it "should equal to quoted fractional" $
      qFractional (1.23 :: Double) "" 0 0 0 0
        `shouldBe` toCode (1.23 :: Double)
  describe "qUnit function" $
    it "should equal to quoted unit" $
      qUnit "" 0 0 0 0 `shouldBe` toCode ()

  let qc x = qChar x "" 0 0 0 0
  describe "qList function" $
    it "should equal to quoted codes" $
      let xs = [qc 'a', qc 'b']
      in qList xs "" 0 0 0 0 `shouldBe` toCode (List xs)
  describe "qHsList function" $
    it "should equal to quoted haskell list" $
      let xs = [qc 'a', qc 'b']
      in  qHsList xs "" 0 0 0 0 `shouldBe` toCode (HsList xs)

fracTest :: Double -> Spec
fracTest x =
  describe ("read and show a fractional number `" ++ show x ++ "'") $
    it "should match the input" $
       show (aFractional x) `shouldBe` show x

showTest :: Spec
showTest =
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

applicativeTest :: Spec
applicativeTest = do
  let atom_a = AChar NoSourceText 'a'
      char_a = toCode atom_a
      al2 = qList [char_a, char_a] "" 0 0 0 0
      ahl2 = qHsList [char_a, char_a] "" 0 0 0 0
      unit = toCode ()
      f1 a b = (a,b)
      a_pair = lf (Atom (atom_a, atom_a))
      a_pair_ls = mk_a_pairs List 2 -- lf (List [a_pair, a_pair])
      a_pair_hls = mk_a_pairs HsList 2 -- lf (HsList [a_pair, a_pair])
      mk_a_pairs f n = lf (f (replicate n a_pair))
      lf = LForm . genSrc
      tend :: Form Atom
      tend = TEnd

  describe "pure" $
    it "should result in atom" $
      toCode (pure AUnit :: Form Atom) `shouldBe` unit

  describe "<*>" $ do
    it "should apply f1 to atom and atom" $
      f1 <$> char_a <*> char_a `shouldBe` a_pair
    it "should apply f1 to atom and list" $
      f1 <$> char_a <*> al2 `shouldBe` a_pair_ls
    it "should apply f1 to atom and hslist" $
      f1 <$> char_a <*> ahl2 `shouldBe` a_pair_hls
    it "should aply f1 to list and atom" $
      f1 <$> al2 <*> char_a `shouldBe` a_pair_ls
    it "should apply f1 to list and list" $
      f1 <$> al2 <*> al2 `shouldBe` mk_a_pairs List 4
    it "should apply f1 to list and hslist" $
      f1 <$> al2 <*> ahl2 `shouldBe` mk_a_pairs List 4
    it "should aply f1 to list and atom" $
      f1 <$> ahl2 <*> char_a `shouldBe` a_pair_hls
    it "should apply f1 to list and list" $
      f1 <$> ahl2 <*> al2 `shouldBe` mk_a_pairs HsList 4
    it "should apply f1 to list and list" $
      f1 <$> ahl2 <*> ahl2 `shouldBe` mk_a_pairs HsList 4
    it "should result in TEnd" $ do
      f1 <$> char_a <*> lf tend `shouldBe` lf TEnd
      f1 <$> lf tend <*> char_a `shouldBe` lf TEnd

monadTest :: Spec
monadTest = do
  let f1 x = case x of
               AChar st c -> AChar st (toUpper c)
               _          -> x
      qh x = qHsList x "" 0 0 0 0
      ql x = qList x "" 0 0 0 0
  describe "bind" $ do
    it "should apply f1 to atom" $
      do {x <- toCode 'x'; return (f1 x)} `shouldBe` toCode 'X'
    it "should apply f1 to list" $
      do {x <- ql [toCode 'x', toCode 'x']; return (f1 x)} `shouldBe`
         ql [toCode 'X', toCode 'X']
    it "should apply f1 to hslist" $
      do {x <- qh [toCode 'x',toCode 'x']; return (f1 x)} `shouldBe`
         qh [toCode 'X', toCode 'X']
    it "should apply f1 to TEnd" $
      do {x <- toCode (TEnd :: Form Atom); return (f1 x)} `shouldBe`
         toCode (TEnd :: Form Atom)

foldableTest :: Spec
foldableTest = do
  let fsum = foldr (\x acc -> case x of
                                AInteger il -> acc + il_value il
                                _           -> acc)
                   0
  describe "taking sum of 1 to 10 with foldr" $ do
    let str1 = "(1 2 3 4 5 6 7 8 9 10)"
    it ("should be 55 for " ++ str1) $
      fsum (parseE str1) `shouldBe` 55
    it "should be 0 for TEnd" $ do
#if MIN_VERSION_ghc(9,0,0)
      let sp = UnhelpfulSpan (UnhelpfulOther (fsLit "<foldableTest>"))
#else
      let sp = UnhelpfulSpan (fsLit "<foldableTest>")
#endif
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

binaryTest :: Spec
binaryTest = do
  describe "Instances of Get and Put from binary" $ do
    it "should return the original value" $ do
      let to_from_bin :: Code -> Bool
          to_from_bin x = decode (encode x) == x
      property to_from_bin

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
#if MIN_VERSION_ghc(9,0,0)
          sp = UnhelpfulSpan (UnhelpfulOther (fsLit "<eqPropTest>"))
#else
          sp = UnhelpfulSpan (fsLit "<eqPropTest>")
#endif
      property g
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

asLocOfTest :: Spec
asLocOfTest =
  describe ("apply asLocOf to code") $ do
    it "should return the value of first arg" $
      property (\ a b -> asLocOf a b == a)
    it "should use the location of second arg" $
      property (\ a b -> case (asLocOf a b, b) of
                   (LForm (L l1 _), LForm (L l2 _)) -> l1 == l2)

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
                   Just y  -> y `shouldBe` x
                   Nothing -> error ("got Nothing with " ++ show x)
  t (aIntegral (42 :: Int))
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
#if !MIN_VERSION_ghc(9,0,0)
  t (Semigroup.Option (Just "foo"))
#endif
  t (Semigroup.WrapMonoid True)
  t (42 :: Natural)
  t (Atom (aIntegral (42 :: Int)))
  t (LForm (genSrc (Atom (aIntegral (42 :: Int)))))
  t [True, False]
  t [EQ, LT, GT]
  t (Just (42 :: Int))
  t [Right True, Left "foo"]
  t (Just 'x', [Right False, Left "foo"])
  t (Just 'x', [Right False, Left "foo"], EQ)
  t (Just 'x', [Right False, Left "foo"], EQ, 42::Int)
  t (Just 'x', [Right False, Left "foo"], EQ, 42::Int ,False)
  t (Just 'x', [Right False, Left "foo"], EQ, 42::Int
    ,False, Just [Right (Just EQ), Left (3.1 :: Double)])

rnfTest :: Spec
rnfTest = do
  describe "rnf of arbitrary form" $
    it "should return ()" $
       property (rnf :: Code -> ())
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

cInt :: Int -> Code
cInt = toCode

cDouble :: Double -> Code
cDouble = toCode

numTest :: Spec
numTest =
  describe "Num instance for Code" $ do
    it "should evaluate +" $ do
      cInt 2 + cInt 40 `shouldBe` 42
      cInt 2 + cDouble 40 `shouldBe` 42.0
      cDouble 2 + cInt 40 `shouldBe` 42.0
      cDouble 2.0 + cDouble 40.0 `shouldBe` 42.0
    it "should evaluate * for AInteger" $ do
      cInt 6 * cInt 7  `shouldBe` 42
      cDouble 6 * cDouble 7 `shouldBe` 42.0
    it "should evalue - for AInteger" $ do
      cInt 50 - cInt 8 `shouldBe` 42
      cDouble 50 - cDouble 8 `shouldBe` 42.0
    it "should evaluate abs" $ do
      abs (cInt (-42)) `shouldBe` 42
      abs (cDouble (-42)) `shouldBe` 42.0
    it "should evaluate signum" $ do
      signum (cInt (-42)) `shouldBe` -1
      signum (cDouble (-42)) `shouldBe` -1.0
    it "should evaluate fromInteger" $
      fromInteger 42 `shouldBe` cInt 42
    it "should result to nil with invalid values" $ do
      3 + toCode 'a' `shouldBe` nil
      signum (toCode 'a') `shouldBe` nil

fractionalTest :: Spec
fractionalTest =
  describe "Fractional instance for Code" $ do
    it "should evaluate /" $ do
      cInt 84 / cInt 2 `shouldBe` 42.0
      cDouble 84.0 / cDouble 2.0 `shouldBe` 42.0
    it "should evaluate recip" $ do
      recip (cInt 4) `shouldBe` 0.25
      recip (cDouble 4.0) `shouldBe` 0.25

monoidTest :: Spec
monoidTest = do
  let a = toCode 'a'
      b = toCode 'b'
      lst = toCode (List [a,b])
      hslst = toCode (HsList [a,b])
  describe "Atom <> XXX" $ do
    it "should result in '(a b)" $
      a <> b `shouldBe` lst
    it "should result in '(a a b)" $
      a <> lst `shouldBe` toCode (List [a,a,b])
    it "should result in '(a a b)" $
      a <> hslst `shouldBe` toCode (List [a,a,b])
  describe "List <> XXX" $ do
    it "should result in '(a b a)" $
      lst <> a `shouldBe` toCode (List [a,b,a])
    it "should result in '(a b a b)" $
      lst <> lst `shouldBe` toCode (List [a,b,a,b])
    it "should result in '(a b a b)" $
      lst <> hslst `shouldBe` toCode (List [a,b,a,b])
  describe "HsList <> XXX" $ do
    it "should result in '(a b a)" $
      hslst <> a `shouldBe` toCode (List [a,b,a])
    it "should result in '(a b a b)" $
      hslst <> lst `shouldBe` toCode (List [a,b,a,b])
    it "should result in '(a b a b)" $
      hslst <> hslst `shouldBe` toCode (List [a,b,a,b])
  describe "TEnd" $ do
    it "should result in non-TEnd" $ do
      let at = Atom True
      at <> TEnd `shouldBe` at
      TEnd <> at `shouldBe` at
  describe "mempty" $ do
    it "should be empty list" $
      mempty `shouldBe` (List [] :: Form Atom)
    it "should be nil" $
      mempty `shouldBe` nil

alternativeTest :: Spec
alternativeTest =
  describe "Alternative" $ do
    describe "empty" $ do
      it "should be nil for Code" $
        empty `shouldBe` nil
      it "should be empty form for Form" $
        empty `shouldBe` (mempty :: Form Atom)
    describe "<|>" $ do
      let a = toCode 'a'
          b = toCode 'b'
          c = toCode 'c'
      it "should append elements for Code" $
        let ab = toCode (List [a,b])
            cb = toCode (List [c,b])
        in  ab <|> cb `shouldBe` toCode (List [a,b,c,b])
      it "should append elements for Form Atom" $
        let ab = List [a,b]
            cb = List [c,b]
        in  ab <|> cb `shouldBe` List [a,b,c,b]

data Foo = Foo deriving (Eq, Show)

instance ToCode Foo where
  toCode foo = toCode (aSymbol (show foo))

instance FromCode Foo

fromCodeTest :: Spec
fromCodeTest = do
  describe "default toCode implementation" $
    it "should return Nothing" $
      (fromCode nil :: Maybe Foo) `shouldBe` Nothing

  describe "getting Nothing from fromCode" $ do
    let ng title x = it title $ x `shouldBe` Nothing
        q x = qSymbol x "" 0 0 0 0
        foo = q "foo"
        foo1 = qList [q "Foo", q "a"] "" 0 0 0 0
        foo2 = qList [q "Foo", q "a", q "b"] "" 0 0 0 0

    it "should result to Nothing with explicit Nothing" $ do
      fromCode (q "Nothing") `shouldBe` (Just Nothing :: Maybe (Maybe ()))
      toCode (Nothing :: Maybe ()) `shouldBe` q "Nothing"

    ng "()" (fromCode foo :: Maybe ())
    ng "Int" (fromCode foo :: Maybe Int)
    ng "Char" (fromCode nil :: Maybe Char)
    ng "[Int]" (fromCode nil :: Maybe [Int])
    ng "[Char]" (fromCode foo :: Maybe [Char])
    ng "Bool" (fromCode foo :: Maybe Bool)
    ng "Ordering" (fromCode foo :: Maybe Ordering)
    ng "Maybe ()" (fromCode foo1 :: Maybe (Maybe ()))
    ng "Either String ()" (fromCode foo1 :: Maybe (Either String ()))
    ng "(,)" (fromCode foo :: Maybe ((), ()))
    ng "(,,)" (fromCode foo :: Maybe ((), (), ()))
    ng "(,,,)" (fromCode foo :: Maybe ((), (), (), ()))
    ng "(,,,,)" (fromCode foo :: Maybe ((), (), (), (), ()))
    ng "(,,,,,)" (fromCode foo :: Maybe ((), (), (), (), (), ()))
    ng "Data.Functor.Sum.Sum Maybe (Either String) ()"
       (fromCode foo1 :: Maybe (Sum.Sum Maybe (Either String) ()))
    ng "Data.Functor.Product.Product Maybe (Either String) ()"
       (fromCode foo2 :: Maybe (Product.Product Maybe (Either String) ()))
    ng "Sum ()" (fromCode foo1 :: Maybe (Sum ()))
    ng "Proxy ()" (fromCode foo :: Maybe (Proxy ()))
    ng "Atom" (fromCode foo1 :: Maybe Atom)

data D1 = D1a | D1b | D1c
  deriving (Eq, Show, Data, Typeable)

instance ToCode D1
instance FromCode D1

data D2 a = D2a a | D2b a a
  deriving (Eq, Show, Data, Typeable)

instance Data a => ToCode (D2 a)
instance Data a => FromCode (D2 a)

data D3 a b = D3a Int a b
  deriving (Eq, Show, Data, Typeable)

instance (Data a, Data b) => ToCode (D3 a b)
instance (Data a, Data b) => FromCode (D3 a b)

data D4 a = D4a (a, a, a, a)
  deriving (Eq, Show, Data, Typeable)

instance Data a => ToCode (D4 a)
instance Data a => FromCode (D4 a)

dataToCodeTest :: Spec
dataToCodeTest = do
  let s = toCode . aSymbol
  describe "D1 to Code" $
    it "should match D1 values" $ do
      let e1 = toCode (HsList (map s ["D1a", "D1b", "D1c"]))
      toCode [D1a, D1b, D1c] `shouldBe` e1
  describe "D2 to Code" $
   it "should match `D2 Char' values"$ do
     let e2 = toCode
                (HsList
                   [ toCode (List [s "D2a", toCode 'x'])
                   , toCode (List [s "D2b", toCode 'y', toCode 'z'])])
     toCode [D2a 'x', D2b 'y' 'z'] `shouldBe` e2
  describe "D2 with D1 to Code" $
   it "should match `D2 D1' values" $ do
     let e2b = toCode (List [s "D2b", s "D1a", s "D1b"])
     toCode (D2b D1a D1b) `shouldBe` e2b
  describe "D2 with Double to Code" $
   it "should match `D2 Double' values" $ do
     let e2c = toCode (List [s "D2a", toCode (1.23 :: Double)])
     toCode (D2a (1.23 :: Double)) `shouldBe` e2c
  describe "D3 to Code" $
   it "should match `D3' value" $ do
     let e3 = toCode (List [ s "D3a"
                           , toCode (42 :: Int)
                           , toCode False
                           , toCode 'a' ])
     toCode (D3a 42 False 'a') `shouldBe` e3
  describe "D4 to Code" $
   it "should match `D4' value" $ do
      let e4 = toCode (List [ s "D4a"
                            , toCode (List [ s ","
                                           , toCode 'w'
                                           , toCode 'x'
                                           , toCode 'y'
                                           , toCode 'z'])])
      toCode (D4a ('w', 'x', 'y', 'z')) `shouldBe` e4

unquoteSpliceTest :: Spec
unquoteSpliceTest =
  describe "unquote splicing List" $
    it "should return list contents" $
      property
        (\form ->
           (isListL form || isHsListL form ||
            isStringL form || isUnitL form)
           ==> (0 <= length (unquoteSplice form)))

eqForm :: Code -> Code -> Bool
eqForm a b =
  case (unCode a, unCode b) of
    -- Ignoring rounding error for fractional literals.
    (Atom (AFractional x), Atom (AFractional y))
      -> abs (fl_value x - fl_value y) <= toRational epsilon

    -- Recursively compare with `eqForm' for 'List' and 'HsList'.
    (List [], List []) -> True
    (List (x:xs), List (y:ys)) ->
      let ql z = qList z "" 0 0 0 0
      in  eqForm x y && eqForm (ql xs) (ql ys)

    (HsList [], HsList []) -> True
    (HsList (x:xs), HsList (y:ys)) ->
      let qh z = qHsList z "" 0 0 0 0
      in  eqForm x y && eqForm (qh xs) (qh ys)

    -- Treating empty 'List' and Atom symbol 'nil' as same value.
    (Atom (ASymbol sym), List []) | sym == fsLit "nil" -> True
    (List [], Atom (ASymbol sym)) | sym == fsLit "nil" -> True

    -- Using '==' for other Atom values.
    (Atom x, Atom y) -> x == y

    _ -> False
  where
    epsilon :: Double
    epsilon = 1e-7

pprTest :: Spec
pprTest =
  describe "ppr" $
    it "should return expected String" $ do
      let str0 = "(1 2.34 () #'a \"foo\" [foo bar buzz])"
      str1 <- runPpr str0
      str1 `shouldBe` str0

runPpr :: String -> IO String
runPpr str =
  runFnk (do dflags <- getDynFlags
             return (showPpr dflags (parseE str)))
         defaultFnkEnv

parseE :: String -> Code
parseE = parseE' Nothing

parseE' :: Maybe FilePath -> String -> Code
parseE' mb_path str =
  let inp = stringToStringBuffer str
  in  case runSP sexpr mb_path inp of
        Right (expr, _) -> expr
        Left err        -> error err

isListL :: Code -> Bool
isListL (LForm (L _ (List _))) = True
isListL _                      = False

isHsListL :: Code -> Bool
isHsListL (LForm (L _ (HsList _))) = True
isHsListL _                        = False

isStringL :: Code -> Bool
isStringL (LForm (L _ (Atom (AString _ _)))) = True
isStringL _                                  = False

isUnitL :: Code -> Bool
isUnitL (LForm (L _ (Atom AUnit))) = True
isUnitL _                          = False
