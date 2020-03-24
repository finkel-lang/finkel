{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Module containing 'Homoiconic' type class and its instances
-- declarations.
module Language.Finkel.Homoiconic
  ( ToCode(..)
  , FromCode(..)
  ) where

-- base
import           Data.Complex          (Complex (..))
import           Data.Data
import           Data.Fixed            (Fixed (..))
import           Data.Functor.Compose  (Compose (..))
import           Data.Functor.Const    (Const (..))
import           Data.Functor.Identity (Identity (..))
import           Data.Int              (Int16, Int32, Int64, Int8)
import           Data.List.NonEmpty    (NonEmpty (..))
import           Data.Monoid           (All (..), Alt (..), Any (..),
                                        Dual (..), First (..), Last (..),
                                        Product (..), Sum (..))
import           Data.Ratio            (Ratio, denominator, numerator, (%))
import           Data.Version          (Version (..))
import           Data.Word             (Word16, Word32, Word64, Word8)
import           Numeric.Natural       (Natural)

import qualified Data.Functor.Product  as Product
import qualified Data.Functor.Sum      as Sum
import qualified Data.Semigroup        as Semigroup

-- ghc
import           BasicTypes            (SourceText(..), fl_value)
import           FastString            (FastString, unpackFS)
import           SrcLoc                (GenLocated (..), getLoc)

-- internal
import           Language.Finkel.Form


-- -------------------------------------------------------------------
--
-- Homoiconic type class
--
-- -------------------------------------------------------------------

-- | Class for handling Haskell value as code.
--
-- The function 'listToCode' is used when handling Haskell values
-- specially (e.g., 'Char'). This function have default implementation
-- which simply applies 'toCode' to elements of the argument list.
--
class ToCode a where
  -- | Convert Haskell value to 'Code'.
  toCode :: a -> Code
  {-# INLINE toCode #-}
  default toCode :: Data a => a -> Code
  toCode = dataToCode

  -- | Convert list of Haskell values to 'Code'.
  listToCode :: [a] -> Code
  listToCode xs =
     let xs' = map toCode xs
         l = getLoc (mkLocatedForm xs')
     in  LForm (L l (HsList xs'))
  {-# INLINE listToCode #-}

-- | Class for handling code as Haskell value.
--
-- The function 'listFromCode' is used when handling list of code
-- specially. This function have default implementation which simply
-- applies 'fromCode' to elements of the elements of the 'HsList'.
--
class FromCode a where

  -- | Convert 'Code' to 'Just' Haskell value, or 'Nothing' if the code
  -- could not be converted.
  fromCode :: Code -> Maybe a
  fromCode _ = Nothing
  {-# INLINE fromCode #-}

  -- | Convert 'Code' to 'Just' list of Haskell values, or 'Nothing' if
  -- the code could not be converted.
  listFromCode :: Code -> Maybe [a]
  listFromCode xs =
    case unCode xs of
      HsList as -> mapM fromCode as
      _         -> Nothing
  {-# INLINE listFromCode #-}

--
-- Prelude
--

instance ToCode () where
  toCode _ = LForm (genSrc (Atom AUnit))

instance FromCode () where
  fromCode a =
    case unCode a of
      Atom AUnit -> Just ()
      _          -> Nothing

instance ToCode Char where
  toCode = LForm . genSrc . Atom . AChar NoSourceText
  listToCode = LForm . genSrc . Atom . aString

instance FromCode Char where
  fromCode a =
    case unCode a of
      Atom (AChar _ x) -> Just x
      _                -> Nothing
  listFromCode a = case unCode a of
                     Atom (AString s) -> Just (unpackFS s)
                     _                -> Nothing

instance ToCode Int where
  toCode = integralToCode

instance FromCode Int where
  fromCode = integralFromCode

instance ToCode Word where
  toCode = integralToCode

instance FromCode Word where
  fromCode = integralFromCode

instance ToCode Integer where
  toCode = LForm . genSrc . Atom . AInteger

instance FromCode Integer where
  fromCode a =
    case unCode a of
      Atom (AInteger n) -> Just n
      _                 -> Nothing

instance ToCode Float where
  toCode = realFracToCode

instance FromCode Float where
  fromCode = fractionalFromCode

instance ToCode Double where
  toCode = realFracToCode

instance FromCode Double where
  fromCode = fractionalFromCode

instance ToCode a => ToCode [a] where
  toCode = listToCode

instance FromCode a => FromCode [a] where
  fromCode = listFromCode

instance ToCode Bool where
  toCode = showAsSymbolCode

instance FromCode Bool where
  fromCode a =
    case unCode a of
      Atom (ASymbol sym) | sym == "True"  -> Just True
                         | sym == "False" -> Just False
      _                                   -> Nothing

instance ToCode Ordering where
  toCode = showAsSymbolCode

instance FromCode Ordering where
  fromCode a =
    case unCode a of
      Atom (ASymbol sym) | sym == "EQ" -> Just EQ
                         | sym == "LT" -> Just LT
                         | sym == "GT" -> Just GT
      _                                -> Nothing

instance ToCode a => ToCode (Maybe a) where
  toCode a =
    case a of
      Nothing -> toCode (aSymbol "Nothing")
      Just x  -> toCode1 "Just" x

instance FromCode a => FromCode (Maybe a) where
  fromCode a =
    case unCode a of
      Atom (ASymbol "Nothing")                      -> Just Nothing
      List [LForm (L _ (Atom (ASymbol "Just"))), x] -> Just (fromCode x)
      _                                             -> Nothing

instance (ToCode a, ToCode b) => ToCode (Either a b) where
  toCode a =
    case a of
      Right x -> toCode1 "Right" x
      Left x  -> toCode1 "Left" x

instance (FromCode a, FromCode b) => FromCode (Either a b) where
  fromCode a =
    case unCode a of
      List [LForm (L _ (Atom (ASymbol x))), y]
        | x == "Right" -> fmap Right (fromCode y)
        | x == "Left"  -> fmap Left (fromCode y)
      _                -> Nothing

instance (ToCode a, ToCode b) => ToCode (a, b) where
  toCode (a1, a2) = toCode2 "," a1 a2

instance (FromCode a, FromCode b) => FromCode (a, b) where
  fromCode = fromCode2 "," (\x y -> (x, y))

instance (ToCode a, ToCode b, ToCode c)
         => ToCode (a, b, c) where
  toCode (a1, a2, a3) =
    toCode (List [symbolCode ",", toCode a1, toCode a2, toCode a3])

instance (FromCode a, FromCode b, FromCode c)
         => FromCode (a, b, c) where
  fromCode a =
    case unCode a of
      List [LForm (L _ (Atom (ASymbol ","))), a1, a2, a3]
        -> (\b1 b2 b3 -> (b1, b2, b3)) <$>
           fromCode a1 <*> fromCode a2 <*> fromCode a3
      _ -> Nothing

instance (ToCode a, ToCode b, ToCode c, ToCode d)
         => ToCode (a, b, c, d) where
  toCode (a1, a2, a3, a4) =
    toCode (List [ symbolCode ",", toCode a1, toCode a2, toCode a3
                 , toCode a4])

instance (FromCode a, FromCode b, FromCode c, FromCode d)
         => FromCode (a, b, c, d) where
  fromCode a =
    case unCode a of
      List [LForm (L _ (Atom (ASymbol ","))), a1, a2, a3, a4]
        -> (\b1 b2 b3 b4 -> (b1, b2, b3, b4)) <$>
           fromCode a1 <*> fromCode a2 <*> fromCode a3 <*> fromCode a4
      _ -> Nothing

instance (ToCode a, ToCode b, ToCode c, ToCode d, ToCode e)
         => ToCode (a, b, c, d, e) where
  toCode (a1, a2, a3, a4, a5) =
    toCode (List [ symbolCode ",", toCode a1, toCode a2, toCode a3
                 , toCode a4, toCode a5])

instance (FromCode a, FromCode b, FromCode c, FromCode d, FromCode e)
         => FromCode (a, b, c, d, e) where
  fromCode a =
    case unCode a of
      List [LForm (L _ (Atom (ASymbol ","))), a1, a2, a3, a4, a5]
        -> (\b1 b2 b3 b4 b5 -> (b1, b2, b3, b4, b5)) <$>
           fromCode a1 <*> fromCode a2 <*> fromCode a3 <*>
           fromCode a4 <*> fromCode a5
      _ -> Nothing

instance (ToCode a, ToCode b, ToCode c, ToCode d, ToCode e, ToCode f)
         => ToCode (a, b, c, d, e, f) where
  toCode (a1, a2, a3, a4, a5, a6) =
    toCode (List [ symbolCode ",", toCode a1, toCode a2, toCode a3
                 , toCode a4, toCode a5, toCode a6])

instance ( FromCode a, FromCode b, FromCode c, FromCode d, FromCode e
         , FromCode f)
         => FromCode (a, b, c, d, e, f) where
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

instance ToCode a => ToCode (Complex a) where
  toCode (a :+ b) = toCode2 ":+" a b

instance FromCode a => FromCode (Complex a) where
  fromCode = fromCode2 ":+" (:+)

--
-- Data.Fixed
--

instance ToCode (Fixed a) where
  toCode (MkFixed a) = toCode1 "MkFixed" a

instance FromCode (Fixed a) where
  fromCode = fromCode1 "MkFixed" MkFixed

--
-- Data.Functor.Compose

instance ToCode (f (g a)) => ToCode (Compose f g a) where
  toCode (Compose a) = toCode1 "Compose" a

instance FromCode (f (g a)) => FromCode (Compose f g a) where
  fromCode = fromCode1 "Compose" Compose

--
-- Data.Functor.Const
--

instance ToCode a => ToCode (Const a b) where
  toCode (Const a) = toCode1 "Const" a

instance FromCode a => FromCode (Const a b) where
  fromCode = fromCode1 "Const" Const

--
-- Data.Functor.Identity
--

instance ToCode a=> ToCode (Identity a) where
  toCode (Identity a) = toCode1 "Identity" a

instance FromCode a=> FromCode (Identity a) where
  fromCode = fromCode1 "Identity" Identity

--
-- Data.Functor.Product
--

instance (ToCode (f a), ToCode (g a))
         => ToCode (Product.Product f g a) where
  toCode (Product.Pair a b) = toCode2 "Pair" a b

instance (FromCode (f a), FromCode (g a))
         => FromCode (Product.Product f g a) where
  fromCode = fromCode2 "Pair" Product.Pair

--
-- Data.Functor.Sum
--

instance (ToCode (f a), ToCode (g a)) => ToCode (Sum.Sum f g a) where
  toCode a =
    case a of
      Sum.InL x -> toCode1 "InL" x
      Sum.InR x -> toCode1 "InR" x

instance (FromCode (f a), FromCode (g a)) => FromCode (Sum.Sum f g a) where
  fromCode a =
    case unCode a of
      List [LForm (L _ (Atom (ASymbol tag))), b]
        | tag == "InL" -> Sum.InL <$> fromCode b
        | tag == "InR" -> Sum.InR <$> fromCode b
      _ -> Nothing

--
-- Data.Int
--

instance ToCode Int8 where
  toCode = integralToCode

instance FromCode Int8 where
  fromCode = integralFromCode

instance ToCode Int16 where
  toCode = integralToCode

instance FromCode Int16 where
  fromCode = integralFromCode

instance ToCode Int32 where
  toCode = integralToCode

instance FromCode Int32 where
  fromCode = integralFromCode

instance ToCode Int64 where
  toCode = integralToCode

instance FromCode Int64 where
  fromCode = integralFromCode

--
-- Data.List.NonEmpty
--

instance ToCode a => ToCode (NonEmpty a) where
  toCode (a :| as) = toCode2 ":|" a as

instance FromCode a => FromCode (NonEmpty a) where
  fromCode = fromCode2 ":|" (:|)

--
-- Data.Monoid
--

instance ToCode All where
  toCode (All a) = toCode1 "All" a

instance FromCode All where
  fromCode = fromCode1 "All" All

instance ToCode (f a) => ToCode (Alt f a) where
  toCode (Alt a) = toCode1 "Alt" a

instance FromCode (f a) => FromCode (Alt f a) where
  fromCode = fromCode1 "Alt" Alt

instance ToCode Any where
  toCode (Any a) = toCode1 "Any" a

instance FromCode Any where
  fromCode = fromCode1 "Any" Any

instance ToCode a => ToCode (Dual a) where
  toCode (Dual a) = toCode1 "Dual" a

instance FromCode a => FromCode (Dual a) where
  fromCode = fromCode1 "Dual" Dual

instance ToCode a => ToCode (First a) where
  toCode (First a) = toCode1 "First" a

instance FromCode a => FromCode (First a) where
  fromCode = fromCode1 "First" First

instance ToCode a => ToCode (Last a) where
  toCode (Last a) = toCode1 "Last" a

instance FromCode a => FromCode (Last a) where
  fromCode = fromCode1 "Last" Last

instance ToCode a => ToCode (Product a) where
  toCode (Product a) = toCode1 "Product" a

instance FromCode a => FromCode (Product a) where
  fromCode = fromCode1 "Product" Product

instance ToCode a => ToCode (Sum a) where
  toCode (Sum a) = toCode1 "Sum" a

instance FromCode a => FromCode (Sum a) where
  fromCode = fromCode1 "Sum" Sum

--
-- Data.Proxy
--

instance ToCode a => ToCode (Proxy a) where
  toCode _ = symbolCode "Proxy"

instance FromCode a => FromCode (Proxy a) where
  fromCode a = case unCode a of
                 Atom (ASymbol "Proxy") -> Just Proxy
                 _                      -> Nothing

--
-- Data.Version
--

instance ToCode Version where
  toCode (Version b t) = toCode2 "Version" b t

instance FromCode Version where
  fromCode = fromCode2 "Version" Version

--
-- Data.Ratio
--

instance (Integral a, ToCode a) => ToCode (Ratio a) where
  toCode a =
    let n = toCode (numerator a)
        d = toCode (denominator a)
    in toCode (List [symbolCode ":%", n, d])

instance (Integral a, FromCode a) => FromCode (Ratio a) where
  fromCode = fromCode2 ":%" (%)


--
-- Data.Semigroup
--

instance (ToCode a, ToCode b) => ToCode (Semigroup.Arg a b) where
  toCode (Semigroup.Arg a b) = toCode2 "Arg" a b

instance (FromCode a, FromCode b) => FromCode (Semigroup.Arg a b) where
  fromCode = fromCode2 "Arg" Semigroup.Arg

instance ToCode a => ToCode (Semigroup.First a) where
  toCode (Semigroup.First a) = toCode1 "First" a

instance FromCode a => FromCode (Semigroup.First a) where
  fromCode = fromCode1 "First" Semigroup.First

instance ToCode a => ToCode (Semigroup.Last a) where
  toCode (Semigroup.Last a) = toCode1 "Last" a

instance FromCode a => FromCode (Semigroup.Last a) where
  fromCode = fromCode1 "Last" Semigroup.Last

instance ToCode a => ToCode (Semigroup.Max a) where
  toCode (Semigroup.Max a) = toCode1 "Max" a

instance FromCode a => FromCode (Semigroup.Max a) where
  fromCode = fromCode1 "Max" Semigroup.Max

instance ToCode a => ToCode (Semigroup.Min a) where
  toCode (Semigroup.Min a) = toCode1 "Min" a

instance FromCode a => FromCode (Semigroup.Min a) where
  fromCode = fromCode1 "Min" Semigroup.Min

instance ToCode a => ToCode (Semigroup.Option a) where
  toCode (Semigroup.Option a) = toCode1 "Option" a

instance FromCode a => FromCode (Semigroup.Option a) where
  fromCode = fromCode1 "Option" Semigroup.Option

instance ToCode a => ToCode (Semigroup.WrappedMonoid a) where
  toCode (Semigroup.WrapMonoid a) = toCode1 "WrapMonoid" a

instance FromCode a => FromCode (Semigroup.WrappedMonoid a) where
  fromCode = fromCode1 "WrapMonoid" Semigroup.WrapMonoid

--
-- Data.Word
--

instance ToCode Word8 where
  toCode = integralToCode

instance FromCode Word8 where
  fromCode = integralFromCode

instance ToCode Word16 where
  toCode = integralToCode

instance FromCode Word16 where
  fromCode = integralFromCode

instance ToCode Word32 where
  toCode = integralToCode

instance FromCode Word32 where
  fromCode = integralFromCode

instance ToCode Word64 where
  toCode = integralToCode

instance FromCode Word64 where
  fromCode = integralFromCode

--
-- Numeric.Natural
--

instance ToCode Natural where
  toCode = integralToCode

instance FromCode Natural where
  fromCode = integralFromCode

--
-- Language.Finkel.Form
--

instance ToCode Atom where
  toCode = LForm . genSrc . Atom

instance FromCode Atom where
  fromCode a =
    case unCode a of
      Atom x -> Just x
      _      -> Nothing

instance ToCode (Form Atom) where
  toCode = LForm . genSrc

instance FromCode (Form Atom) where
  fromCode = Just . unCode

instance ToCode (LForm Atom) where
  toCode = id

instance FromCode (LForm Atom) where
  fromCode = Just


-- -------------------------------------------------------------------
--
-- Auxiliary
--
-- -------------------------------------------------------------------

realFracToCode :: (Real a, Show a) => a -> Code
realFracToCode a = LForm (genSrc (Atom (aFractional a)))

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

toCode1 :: ToCode a => FastString -> a -> Code
toCode1 tag arg1 =
  toCode (List [LForm (genSrc (Atom (ASymbol tag))), toCode arg1])
{-# INLINABLE toCode1 #-}

toCode2 :: (ToCode a, ToCode b) =>
           FastString -> a -> b -> Code
toCode2 tag arg1 arg2 =
  toCode (List [ LForm (genSrc (Atom (ASymbol tag)))
               , toCode arg1, toCode arg2 ])
{-# INLINABLE toCode2 #-}

fromCode1 :: (FromCode a) =>
             FastString -> (a -> h) -> Code -> Maybe h
fromCode1 tag f a =
  case unCode a of
    List [LForm (L _ (Atom (ASymbol tag'))), x]
      | tag == tag' -> f <$> fromCode x
    _               -> Nothing
{-# INLINABLE fromCode1 #-}

fromCode2 :: (FromCode a, FromCode b) =>
             FastString -> (a -> b -> h) -> Code -> Maybe h
fromCode2 tag f a =
  case unCode a of
    List [LForm (L _ (Atom (ASymbol tag'))), x, y]
      | tag == tag' -> f <$> fromCode x <*> fromCode y
    _               -> Nothing
{-# INLINABLE fromCode2 #-}
