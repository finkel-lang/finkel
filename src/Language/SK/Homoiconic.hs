{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Module containing 'Homoiconic' typeclass and its instances
-- declarations.
module Language.SK.Homoiconic
  ( Homoiconic(..)
  ) where

-- base
import Data.Complex
import Data.Data
import Data.Fixed
import Data.Functor.Compose
import Data.Functor.Const
import Data.Functor.Identity
import Data.Int
import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid
import Data.Ratio
import Data.Version
import Numeric.Natural
import Data.Word
import qualified Data.Functor.Product as Product
import qualified Data.Functor.Sum as Sum
import qualified Data.Semigroup as Semigroup

-- internal
import Language.SK.GHC
import Language.SK.Form


-- -------------------------------------------------------------------
--
-- Homoiconic type class
--
-- -------------------------------------------------------------------

-- Instance data types of Formable class could be inserted to
-- S-expression with `unquote' and `unquote-splice'.

-- | Class for handling Haskell value as code, and vice versa.
--
-- Instances should satisfy following law:
--
-- > fromCode (toCode x) == Just x
--
class Homoiconic a where

  -- | Convert Haskell value as 'Code'.
  toCode :: a -> Code
  {-# INLINE toCode #-}

  default toCode :: Data a => a -> Code
  toCode = dataToCode

  -- | Convert 'Code' to 'Just' Haskell value, or 'Nothing' if the code
  -- could not be converted.
  fromCode :: Code -> Maybe a
  fromCode _ = Nothing
  {-# INLINE fromCode #-}

  listToCode :: [a] -> Code
  listToCode xs =
     let xs' = map toCode xs
         l = getLoc (mkLocatedForm xs')
     in  LForm (L l (HsList xs'))
  {-# INLINE listToCode #-}

  listFromCode :: Code -> Maybe [a]
  listFromCode xs =
    case unCode xs of
      HsList as -> mapM fromCode as
      _         -> Nothing
  {-# INLINE listFromCode #-}

--
-- Prelude
--

instance Homoiconic () where
  toCode _ = LForm (genSrc (Atom AUnit))
  fromCode a =
    case unCode a of
      Atom AUnit -> Just ()
      _          -> Nothing

instance Homoiconic Char where
  toCode = LForm . genSrc . Atom . AChar
  fromCode a =
    case unCode a of
      Atom (AChar x)  -> Just x
      _               -> Nothing
  listToCode = LForm . genSrc . Atom . AString
  listFromCode a = case unCode a of
                     Atom (AString s) -> Just s
                     _                -> Nothing

instance Homoiconic Int where
  toCode = integralToCode
  fromCode = integralFromCode

instance Homoiconic Word where
  toCode = integralToCode
  fromCode = integralFromCode

instance Homoiconic Integer where
  toCode = LForm . genSrc . Atom . AInteger
  fromCode a =
    case unCode a of
      Atom (AInteger n) -> Just n
      _                 -> Nothing

instance Homoiconic Float where
  toCode = realFracToCode
  fromCode = fractionalFromCode

instance Homoiconic Double where
  toCode = realFracToCode
  fromCode = fractionalFromCode

instance Homoiconic a => Homoiconic [a] where
  toCode = listToCode
  fromCode = listFromCode

instance Homoiconic Bool where
  toCode = showAsSymbolCode
  fromCode a =
    case unCode a of
      Atom (ASymbol sym) | sym == "True"  -> Just True
                         | sym == "False" -> Just False
      _                                   -> Nothing

instance Homoiconic Ordering where
  toCode = showAsSymbolCode
  fromCode a =
    case unCode a of
      Atom (ASymbol sym) | sym == "EQ" -> Just EQ
                         | sym == "LT" -> Just LT
                         | sym == "GT" -> Just GT
      _                                -> Nothing

instance Homoiconic a => Homoiconic (Maybe a) where
  toCode a =
    case a of
      Nothing -> toCode (aSymbol "Nothing")
      Just x  -> toCode1 "Just" x
  fromCode a =
    case unCode a of
      Atom (ASymbol "Nothing") -> Just Nothing
      List [LForm (L _ (Atom (ASymbol "Just"))), x] -> Just (fromCode x)
      _                                             -> Nothing

instance (Homoiconic a, Homoiconic b) => Homoiconic (Either a b) where
  toCode a =
    case a of
      Right x -> toCode1 "Right" x
      Left x  -> toCode1 "Left" x
  fromCode a =
    case unCode a of
      List [LForm (L _ (Atom (ASymbol x))), y]
        | x == "Right" -> fmap Right (fromCode y)
        | x == "Left"  -> fmap Left (fromCode y)
      _                -> Nothing

instance (Homoiconic a, Homoiconic b) => Homoiconic (a, b) where
  toCode (a1, a2) = toCode2 "," a1 a2
  fromCode = fromCode2 "," (\x y -> (x, y))

instance (Homoiconic a, Homoiconic b, Homoiconic c)
         => Homoiconic (a, b, c) where
  toCode (a1, a2, a3) =
    toCode (List [symbolCode ",", toCode a1, toCode a2, toCode a3])
  fromCode a =
    case unCode a of
      List [LForm (L _ (Atom (ASymbol ","))), a1, a2, a3]
        -> (\b1 b2 b3 -> (b1, b2, b3)) <$>
           fromCode a1 <*> fromCode a2 <*> fromCode a3
      _ -> Nothing

instance (Homoiconic a, Homoiconic b, Homoiconic c, Homoiconic d)
         => Homoiconic (a, b, c, d) where
  toCode (a1, a2, a3, a4) =
    toCode (List [ symbolCode ",", toCode a1, toCode a2, toCode a3
                 , toCode a4])
  fromCode a =
    case unCode a of
      List [LForm (L _ (Atom (ASymbol ","))), a1, a2, a3, a4]
        -> (\b1 b2 b3 b4 -> (b1, b2, b3, b4)) <$>
           fromCode a1 <*> fromCode a2 <*> fromCode a3 <*> fromCode a4
      _ -> Nothing

instance (Homoiconic a, Homoiconic b, Homoiconic c, Homoiconic d,
          Homoiconic e)
         => Homoiconic (a, b, c, d, e) where
  toCode (a1, a2, a3, a4, a5) =
    toCode (List [ symbolCode ",", toCode a1, toCode a2, toCode a3
                 , toCode a4, toCode a5])
  fromCode a =
    case unCode a of
      List [LForm (L _ (Atom (ASymbol ","))), a1, a2, a3, a4, a5]
        -> (\b1 b2 b3 b4 b5 -> (b1, b2, b3, b4, b5)) <$>
           fromCode a1 <*> fromCode a2 <*> fromCode a3 <*>
           fromCode a4 <*> fromCode a5
      _ -> Nothing

instance (Homoiconic a, Homoiconic b, Homoiconic c, Homoiconic d,
          Homoiconic e, Homoiconic f)
         => Homoiconic (a, b, c, d, e, f) where
  toCode (a1, a2, a3, a4, a5, a6) =
    toCode (List [ symbolCode ",", toCode a1, toCode a2, toCode a3
                 , toCode a4, toCode a5, toCode a6])
  fromCode a =
    case unCode a of
      List [LForm (L _ (Atom (ASymbol ","))), a1, a2, a3, a4, a5, a6]
        -> (\b1 b2 b3 b4 b5 b6 -> (b1, b2, b3, b4, b5, b6)) <$>
           fromCode a1 <*> fromCode a2 <*> fromCode a3 <*>
           fromCode a4 <*> fromCode a5 <*> fromCode a6
      _ -> Nothing

--
-- Data.Complex
--

instance Homoiconic a => Homoiconic (Complex a) where
  toCode (a :+ b) = toCode2 ":+" a b
  fromCode = fromCode2 ":+" (:+)

--
-- Data.Fixed
--

instance Homoiconic (Fixed a) where
  toCode (MkFixed a) = toCode1 "MkFixed" a
  fromCode = fromCode1 "MkFixed" MkFixed

--
-- Data.Functor.Compose

instance Homoiconic (f (g a)) => Homoiconic (Compose f g a) where
  toCode (Compose a) = toCode1 "Compose" a
  fromCode = fromCode1 "Compose" Compose

--
-- Data.Functor.Const
--

instance Homoiconic a => Homoiconic (Const a b) where
  toCode (Const a) = toCode1 "Const" a
  fromCode = fromCode1 "Const" Const

--
-- Data.Functor.Identity
--

instance Homoiconic a=> Homoiconic (Identity a) where
  toCode (Identity a) = toCode1 "Identity" a
  fromCode = fromCode1 "Identity" Identity

--
-- Data.Functor.Product
--

instance (Homoiconic (f a), Homoiconic (g a))
         => Homoiconic (Product.Product f g a) where
  toCode (Product.Pair a b) = toCode2 "Pair" a b
  fromCode = fromCode2 "Pair" Product.Pair

--
-- Data.Functor.Sum
--

instance (Homoiconic (f a), Homoiconic (g a))
         => Homoiconic (Sum.Sum f g a) where
  toCode a =
    case a of
      Sum.InL x -> toCode1 "InL" x
      Sum.InR x -> toCode1 "InR" x
  fromCode a =
    case unCode a of
      List [LForm (L _ (Atom (ASymbol tag))), b]
        | tag == "InL" -> Sum.InL <$> fromCode b
        | tag == "InR" -> Sum.InR <$> fromCode b
      _ -> Nothing

--
-- Data.Int
--

instance Homoiconic Int8 where
  toCode = integralToCode
  fromCode = integralFromCode

instance Homoiconic Int16 where
  toCode = integralToCode
  fromCode = integralFromCode

instance Homoiconic Int32 where
  toCode = integralToCode
  fromCode = integralFromCode

instance Homoiconic Int64 where
  toCode = integralToCode
  fromCode = integralFromCode

--
-- Data.List.NonEmpty
--

instance Homoiconic a => Homoiconic (NonEmpty a) where
  toCode (a :| as) = toCode2 ":|" a as
  fromCode = fromCode2 ":|" (:|)

--
-- Data.Monoid
--

instance Homoiconic All where
  toCode (All a) = toCode1 "All" a
  fromCode = fromCode1 "All" All

instance Homoiconic (f a) => Homoiconic (Alt f a) where
  toCode (Alt a) = toCode1 "Alt" a
  fromCode = fromCode1 "Alt" Alt

instance Homoiconic Any where
  toCode (Any a) = toCode1 "Any" a
  fromCode = fromCode1 "Any" Any

instance Homoiconic a => Homoiconic (Dual a) where
  toCode (Dual a) = toCode1 "Dual" a
  fromCode = fromCode1 "Dual" Dual

instance Homoiconic a => Homoiconic (First a) where
  toCode (First a) = toCode1 "First" a
  fromCode = fromCode1 "First" First

instance Homoiconic a => Homoiconic (Last a) where
  toCode (Last a) = toCode1 "Last" a
  fromCode = fromCode1 "Last" Last

instance Homoiconic a => Homoiconic (Product a) where
  toCode (Product a) = toCode1 "Product" a
  fromCode = fromCode1 "Product" Product

instance Homoiconic a => Homoiconic (Sum a) where
  toCode (Sum a) = toCode1 "Sum" a
  fromCode = fromCode1 "Sum" Sum

--
-- Data.Proxy
--

instance Homoiconic a => Homoiconic (Proxy a) where
  toCode _ = symbolCode "Proxy"
  fromCode a = case unCode a of
                 Atom (ASymbol "Proxy") -> Just Proxy
                 _ -> Nothing

--
-- Data.Version
--

instance Homoiconic Version where
  toCode (Version b t) = toCode2 "Version" b t
  fromCode = fromCode2 "Version" Version

--
-- Data.Ratio
--

instance (Integral a, Homoiconic a) => Homoiconic (Ratio a) where
  toCode a =
    let n = toCode (numerator a)
        d = toCode (denominator a)
    in toCode (List [symbolCode ":%", n, d])
  fromCode = fromCode2 ":%" (%)

--
-- Data.Semigroup
--

instance (Homoiconic a, Homoiconic b)
         => Homoiconic (Semigroup.Arg a b) where
  toCode (Semigroup.Arg a b) = toCode2 "Arg" a b
  fromCode = fromCode2 "Arg" Semigroup.Arg

instance (Homoiconic a) => Homoiconic (Semigroup.First a) where
  toCode (Semigroup.First a) = toCode1 "First" a
  fromCode = fromCode1 "First" Semigroup.First

instance (Homoiconic a) => Homoiconic (Semigroup.Last a) where
  toCode (Semigroup.Last a) = toCode1 "Last" a
  fromCode = fromCode1 "Last" Semigroup.Last

instance (Homoiconic a) => Homoiconic (Semigroup.Max a) where
  toCode (Semigroup.Max a) = toCode1 "Max" a
  fromCode = fromCode1 "Max" Semigroup.Max

instance (Homoiconic a) => Homoiconic (Semigroup.Min a) where
  toCode (Semigroup.Min a) = toCode1 "Min" a
  fromCode = fromCode1 "Min" Semigroup.Min

instance (Homoiconic a) => Homoiconic (Semigroup.Option a) where
  toCode (Semigroup.Option a) = toCode1 "Option" a
  fromCode = fromCode1 "Option" Semigroup.Option

instance (Homoiconic a) => Homoiconic (Semigroup.WrappedMonoid a) where
  toCode (Semigroup.WrapMonoid a) = toCode1 "WrapMonoid" a
  fromCode = fromCode1 "WrapMonoid" Semigroup.WrapMonoid

--
-- Data.Word
--

instance Homoiconic Word8 where
  toCode = integralToCode
  fromCode = integralFromCode

instance Homoiconic Word16 where
  toCode = integralToCode
  fromCode = integralFromCode

instance Homoiconic Word32 where
  toCode = integralToCode
  fromCode = integralFromCode

instance Homoiconic Word64 where
  toCode = integralToCode
  fromCode = integralFromCode

--
-- Numeric.Natural
--

instance Homoiconic Natural where
  toCode = integralToCode
  fromCode = integralFromCode

--
-- Language.SK.Form
--

instance Homoiconic Atom where
  toCode = LForm . genSrc . Atom
  fromCode a =
    case unCode a of
      Atom x -> Just x
      _      -> Nothing

instance Homoiconic (Form Atom) where
  toCode = LForm . genSrc
  fromCode = Just . unCode

instance Homoiconic (LForm Atom) where
  toCode = id
  fromCode = Just


-- -------------------------------------------------------------------
--
-- Auxiliary
--
-- -------------------------------------------------------------------

realFracToCode :: (Real a, Show a) => a -> Code
realFracToCode a =
  LForm (genSrc (Atom (AFractional (FL (show a) (toRational a)))))

fractionalFromCode :: Fractional a => Code -> Maybe a
fractionalFromCode a =
  case unCode a of
    Atom (AFractional x) -> Just (fromRational (fl_value x))
    _                    -> Nothing

symbolCode :: String -> Code
symbolCode = LForm . genSrc . Atom . aSymbol

showAsSymbolCode :: Show a => a -> Code
showAsSymbolCode = symbolCode . show

integralToCode :: Integral a => a -> Code
integralToCode = LForm . genSrc . Atom . AInteger . fromIntegral

integralFromCode :: Integral a => Code -> Maybe a
integralFromCode a =
  case unCode a of
    Atom (AInteger n) -> Just (fromIntegral n)
    _                 -> Nothing

dataToCode :: Data d => d -> Code
dataToCode x =
  let constr = toConstr x
      isTupleStr cs = case cs of
                        '(':cs1 -> go cs1
                        _       -> False
                        where go xs = case xs of
                                        ',':xs' -> go xs'
                                        ')':[]  -> True
                                        _       -> False
      cstr = case showConstr constr of
               str | isTupleStr str -> ","
                   | otherwise  -> str
      hd = toCode (aSymbol cstr)
  in  case constrRep constr of
         IntConstr n   -> toCode (AInteger n)
         FloatConstr f -> toCode (aFractional
                                    (fromRational f :: Double))
         CharConstr c  -> toCode c
         _             ->
           case gmapQ dataToCode x of
             [] -> hd
             _  -> toCode (List (hd:gmapQ dataToCode x))

toCode1 :: (Homoiconic a) => FastString -> a -> Code
toCode1 tag arg1 =
  toCode (List [LForm (genSrc (Atom (ASymbol tag))), toCode arg1])
{-# INLINABLE toCode1 #-}

toCode2 :: (Homoiconic a, Homoiconic b) =>
           FastString -> a -> b -> Code
toCode2 tag arg1 arg2 =
  toCode (List [ LForm (genSrc (Atom (ASymbol tag)))
               , toCode arg1, toCode arg2 ])
{-# INLINABLE toCode2 #-}

fromCode1 :: (Homoiconic a) =>
             FastString -> (a -> h) -> Code -> Maybe h
fromCode1 tag f a =
  case unCode a of
    List [LForm (L _ (Atom (ASymbol tag'))), x]
      | tag == tag' -> f <$> fromCode x
    _               -> Nothing
{-# INLINABLE fromCode1 #-}

fromCode2 :: (Homoiconic a, Homoiconic b) =>
             FastString -> (a -> b -> h) -> Code -> Maybe h
fromCode2 tag f a =
  case unCode a of
    List [LForm (L _ (Atom (ASymbol tag'))), x, y]
      | tag == tag' -> f <$> fromCode x <*> fromCode y
    _               -> Nothing
{-# INLINABLE fromCode2 #-}
