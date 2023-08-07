{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
-- | Module containing 'Homoiconic' and 'FromCode' type classes and its instance
-- declarations.
module Language.Finkel.Homoiconic
  ( -- * Homoiconic class
    Homoiconic(..)
  , fromCode
  , Result(..)

    -- * Generic functions
  , genericToCode
  , genericFromCode
  , genericParseCode

    -- * Generic classes
  , GToCode(..)
  , GParseCode(..)

    -- * Data.Data function
  , dataToCode
  ) where

#include "ghc_modules.h"

-- base
import           Control.Applicative   (Alternative (..))
import           Data.Complex          (Complex (..))
import           Data.Data
import           Data.Fixed            (Fixed (..))
import           Data.Functor.Compose  (Compose (..))
import           Data.Functor.Const    (Const (..))
import           Data.Functor.Identity (Identity (..))
import           Data.Int              (Int16, Int32, Int64, Int8)
import           Data.List.NonEmpty    (NonEmpty (..))
import           Data.Monoid           (All (..), Alt (..), Any (..), Dual (..),
                                        First (..), Last (..), Product (..),
                                        Sum (..))
import           Data.Ratio            (Ratio, denominator, numerator, (%))
import           Data.Version          (Version (..))
import           Data.Word             (Word16, Word32, Word64, Word8)
import           GHC.Generics          (C, Constructor (..), D, Generic (..),
                                        K1 (..), M1 (..), S, U1 (..), V1,
                                        (:*:) (..), (:+:) (..))
import           Numeric.Natural       (Natural)

import qualified Data.Functor.Product  as Product
import qualified Data.Functor.Sum      as Sum
import qualified Data.Semigroup        as Semigroup

#if !MIN_VERSION_ghc(8,8,0)
import           Control.Monad.Fail    (MonadFail (..))
import           Prelude               hiding (fail)
#endif

-- ghc
import           GHC_Data_FastString   (FastString, unpackFS)
import           GHC_Types_SrcLoc      (GenLocated (..), SrcSpan, getLoc)

-- Internal
import           Language.Finkel.Form


-- -------------------------------------------------------------------
--
-- Homoiconic type class
--
-- -------------------------------------------------------------------

-- | Class for handling Haskell value as 'Code'.
--
-- Instance of 'Homoiconic' should satisfy the law:
--
-- @
-- 'parseCode' ('toCode' x) ≡ 'Success' x
-- @
--
-- The function 'listToCode' and 'parseHsListCode' are used when handling
-- Haskell list values specially (e.g., 'Char'). These functions have default
-- implementations, which simply applies 'toCode' to elements of the argument
-- list, and which parses elements of 'HsList', respectively.
--
-- One can implement 'Homoiconic' instance with 'GHC.Generics.Generic', e.g.:
--
-- @
-- {-# LANGUAGE DeriveGeneric #-}
--
-- data MyData
--   = MyInt Int
--   | MyChar Char
--   deriving (Generic)
--
-- instance Homoiconic MyData
-- @
--
-- Sample snippet using above @MyData@:
--
-- >>> toCode (MyInt 42)
-- (MyInt 42)
-- >>> fromCode (toCode (MyChar 'a')) :: Maybe MyData
-- Just (MyChar 'a')
---
class Homoiconic a where
  -- | Convert Haskell value to 'Code'.
  toCode :: a -> Code
  {-# INLINE toCode #-}

  default toCode :: (Generic a, GToCode (Rep a)) => a -> Code
  toCode = genericToCode

  -- | Convert list of Haskell values to 'Code'.
  listToCode :: [a] -> Code
  listToCode xs =
     let xs' = map toCode xs
         l = getLoc (mkLocatedForm xs')
     in  LForm (L l (HsList xs'))
  {-# INLINE listToCode #-}

  -- | Convert 'Code' to Haskell value, or 'Failure' if the code could
  -- not be converted.
  parseCode :: Code -> Result a
  {-# INLINE parseCode #-}

  default parseCode :: (Generic a, GParseCode (Rep a)) => Code -> Result a
  parseCode = genericParseCode

  -- | Convert 'Code' to list of Haskell values, or 'Failure' if the code
  -- could not be converted.
  parseHsListCode :: Code -> Result [a]
  parseHsListCode xs =
    case unCode xs of
      HsList as -> mapM parseCode as
      _         -> fail "got non HsList value"
  {-# INLINE parseHsListCode #-}

-- | Like 'parseCode', but the result wrapped with 'Maybe' instead of 'Result'.
fromCode :: Homoiconic a => Code -> Maybe a
fromCode code = case parseCode code of
  Success a -> Just a
  _         -> Nothing


-- -------------------------------------------------------------------
--
-- Instances of Homoiconic
--
-- -------------------------------------------------------------------

--
-- Prelude
--

instance Homoiconic () where
  toCode _ = LForm (genSrc (Atom AUnit))
  parseCode a =
    case unCode a of
      Atom AUnit -> pure ()
      _          -> failedToParse "()"

instance Homoiconic Char where
  toCode = LForm . genSrc . Atom . AChar NoSourceText
  listToCode = LForm . genSrc . Atom . aString NoSourceText
  parseCode a =
    case unCode a of
      Atom (AChar _ x) -> pure x
      _                -> failedToParse "Char"
  parseHsListCode a = case unCode a of
                      Atom (AString _ s) -> pure (unpackFS s)
                      _                  -> failedToParse "String"

instance Homoiconic Int where
  toCode = integralToCode
  parseCode = integralFromCode

instance Homoiconic Word where
  toCode = integralToCode
  parseCode = integralFromCode

instance Homoiconic Integer where
  toCode = integralToCode
  parseCode = integralFromCode

instance Homoiconic Float where
  toCode = realFracToCode
  parseCode = fractionalFromCode

instance Homoiconic Double where
  toCode = realFracToCode
  parseCode = fractionalFromCode

instance Homoiconic a => Homoiconic [a] where
  toCode = listToCode
  parseCode = parseHsListCode

instance Homoiconic Bool where
  toCode = showAsSymbolCode
  parseCode a =
    case unCode a of
      Atom (ASymbol sym) | sym == "True"  -> pure True
                         | sym == "False" -> pure False
      _                                   -> failedToParse "Bool"

instance Homoiconic Ordering where
  toCode = showAsSymbolCode
  parseCode a =
    case unCode a of
      Atom (ASymbol sym) | sym == "EQ" -> pure EQ
                         | sym == "LT" -> pure LT
                         | sym == "GT" -> pure GT
      _                                -> failedToParse "Ordering"

instance Homoiconic a => Homoiconic (Maybe a) where
  toCode a =
    case a of
      Nothing -> toCode (aSymbol "Nothing")
      Just x  -> toCode1 "Just" x
  parseCode a =
    case unCode a of
      Atom (ASymbol "Nothing")                      -> pure Nothing
      List [LForm (L _ (Atom (ASymbol "Just"))), x] -> pure <$> parseCode x
      _                                             -> failedToParse "Maybe"

instance (Homoiconic a, Homoiconic b) => Homoiconic (Either a b) where
  toCode a =
    case a of
      Right x -> toCode1 "Right" x
      Left x  -> toCode1 "Left" x
  parseCode a =
    case unCode a of
      List [LForm (L _ (Atom (ASymbol x))), y]
        | x == "Right" -> fmap Right (parseCode y)
        | x == "Left"  -> fmap Left (parseCode y)
      _                -> failedToParse "Either"

instance (Homoiconic a, Homoiconic b) => Homoiconic (a, b) where
  toCode (a1, a2) = toCode2 "," a1 a2
  parseCode = parseCode2 "," (,)

instance (Homoiconic a, Homoiconic b, Homoiconic c)
         => Homoiconic (a, b, c) where
  toCode (a1, a2, a3) =
    toCode (List [symbolCode ",", toCode a1, toCode a2, toCode a3])
  parseCode a =
    case unCode a of
      List [LForm (L _ (Atom (ASymbol ","))), a1, a2, a3]
        -> (,,) <$> parseCode a1 <*> parseCode a2 <*> parseCode a3
      _ -> failedToParse "(,,)"

instance (Homoiconic a, Homoiconic b, Homoiconic c, Homoiconic d)
         => Homoiconic (a, b, c, d) where
  toCode (a1, a2, a3, a4) =
    toCode (List [ symbolCode ",", toCode a1, toCode a2, toCode a3
                 , toCode a4])
  parseCode a =
    case unCode a of
      List [LForm (L _ (Atom (ASymbol ","))), a1, a2, a3, a4]
        -> (,,,) <$>
           parseCode a1 <*> parseCode a2 <*> parseCode a3 <*> parseCode a4
      _ -> failedToParse "(,,,)"

instance (Homoiconic a, Homoiconic b, Homoiconic c, Homoiconic d, Homoiconic e)
         => Homoiconic (a, b, c, d, e) where
  toCode (a1, a2, a3, a4, a5) =
    toCode (List [ symbolCode ",", toCode a1, toCode a2, toCode a3
                 , toCode a4, toCode a5])
  parseCode a =
    case unCode a of
      List [LForm (L _ (Atom (ASymbol ","))), a1, a2, a3, a4, a5]
        -> (,,,,) <$>
           parseCode a1 <*> parseCode a2 <*> parseCode a3 <*>
           parseCode a4 <*> parseCode a5
      _ -> failedToParse "(,,,,)"

instance (Homoiconic a, Homoiconic b, Homoiconic c, Homoiconic d, Homoiconic e, Homoiconic f)
         => Homoiconic (a, b, c, d, e, f) where
  toCode (a1, a2, a3, a4, a5, a6) =
    toCode (List [ symbolCode ",", toCode a1, toCode a2, toCode a3
                 , toCode a4, toCode a5, toCode a6])
  parseCode a =
    case unCode a of
      List [LForm (L _ (Atom (ASymbol ","))), a1, a2, a3, a4, a5, a6]
        -> (,,,,,) <$>
           parseCode a1 <*> parseCode a2 <*> parseCode a3 <*>
           parseCode a4 <*> parseCode a5 <*> parseCode a6
      _ -> failedToParse "(,,,,,)"


--
-- Data.Complex
--

instance Homoiconic a => Homoiconic (Complex a) where
  toCode (a :+ b) = toCode2 ":+" a b
  parseCode = parseCode2 ":+" (:+)

--
-- Data.Fixed
--

instance Homoiconic (Fixed a) where
  toCode (MkFixed a) = toCode1 "MkFixed" a
  parseCode = parseCode1 "MkFixed" MkFixed

--
-- Data.Functor.Compose

instance Homoiconic (f (g a)) => Homoiconic (Compose f g a) where
  toCode (Compose a) = toCode1 "Compose" a
  parseCode = parseCode1 "Compose" Compose

--
-- Data.Functor.Const
--

instance Homoiconic a => Homoiconic (Const a b) where
  toCode (Const a) = toCode1 "Const" a
  parseCode = parseCode1 "Const" Const

--
-- Data.Functor.Identity
--

instance Homoiconic a=> Homoiconic (Identity a) where
  toCode (Identity a) = toCode1 "Identity" a
  parseCode = parseCode1 "Identity" Identity

--
-- Data.Functor.Product
--

instance (Homoiconic (f a), Homoiconic (g a))
         => Homoiconic (Product.Product f g a) where
  toCode (Product.Pair a b) = toCode2 "Pair" a b
  parseCode = parseCode2 "Pair" Product.Pair

--
-- Data.Functor.Sum
--

instance (Homoiconic (f a), Homoiconic (g a)) => Homoiconic (Sum.Sum f g a) where
  toCode a =
    case a of
      Sum.InL x -> toCode1 "InL" x
      Sum.InR x -> toCode1 "InR" x
  parseCode a =
    case unCode a of
      List [LForm (L _ (Atom (ASymbol tag))), b]
        | tag == "InL" -> Sum.InL <$> parseCode b
        | tag == "InR" -> Sum.InR <$> parseCode b
      _ -> failedToParse "Sum"

--
-- Data.Int
--

instance Homoiconic Int8 where
  toCode = integralToCode
  parseCode = integralFromCode

instance Homoiconic Int16 where
  toCode = integralToCode
  parseCode = integralFromCode

instance Homoiconic Int32 where
  toCode = integralToCode
  parseCode = integralFromCode

instance Homoiconic Int64 where
  toCode = integralToCode
  parseCode = integralFromCode

--
-- Data.List.NonEmpty
--

instance Homoiconic a => Homoiconic (NonEmpty a) where
  toCode (a :| as) = toCode2 ":|" a as
  parseCode = parseCode2 ":|" (:|)

--
-- Data.Monoid
--

instance Homoiconic All where
  toCode (All a) = toCode1 "All" a
  parseCode = parseCode1 "All" All

instance Homoiconic (f a) => Homoiconic (Alt f a) where
  toCode (Alt a) = toCode1 "Alt" a
  parseCode = parseCode1 "Alt" Alt

instance Homoiconic Any where
  toCode (Any a) = toCode1 "Any" a
  parseCode = parseCode1 "Any" Any

instance Homoiconic a => Homoiconic (Dual a) where
  toCode (Dual a) = toCode1 "Dual" a
  parseCode = parseCode1 "Dual" Dual

instance Homoiconic a => Homoiconic (First a) where
  toCode (First a) = toCode1 "First" a
  parseCode = parseCode1 "First" First

instance Homoiconic a => Homoiconic (Last a) where
  toCode (Last a) = toCode1 "Last" a
  parseCode = parseCode1 "Last" Last

instance Homoiconic a => Homoiconic (Product a) where
  toCode (Product a) = toCode1 "Product" a
  parseCode = parseCode1 "Product" Product

instance Homoiconic a => Homoiconic (Sum a) where
  toCode (Sum a) = toCode1 "Sum" a
  parseCode = parseCode1 "Sum" Sum

--
-- Data.Proxy
--

instance Homoiconic a => Homoiconic (Proxy a) where
  toCode _ = symbolCode "Proxy"
  parseCode a = case unCode a of
                 Atom (ASymbol "Proxy") -> pure Proxy
                 _                      -> failedToParse "Proxy"

--
-- Data.Version
--

instance Homoiconic Version where
  toCode (Version b t) = toCode2 "Version" b t
  parseCode = parseCode2 "Version" Version

--
-- Data.Ratio
--

instance (Integral a, Homoiconic a) => Homoiconic (Ratio a) where
  toCode a =
    let n = toCode (numerator a)
        d = toCode (denominator a)
    in toCode (List [symbolCode ":%", n, d])
  parseCode = parseCode2 ":%" (%)


--
-- Data.Semigroup
--

instance (Homoiconic a, Homoiconic b) => Homoiconic (Semigroup.Arg a b) where
  toCode (Semigroup.Arg a b) = toCode2 "Arg" a b
  parseCode = parseCode2 "Arg" Semigroup.Arg

instance Homoiconic a => Homoiconic (Semigroup.First a) where
  toCode (Semigroup.First a) = toCode1 "First" a
  parseCode = parseCode1 "First" Semigroup.First

instance Homoiconic a => Homoiconic (Semigroup.Last a) where
  toCode (Semigroup.Last a) = toCode1 "Last" a
  parseCode = parseCode1 "Last" Semigroup.Last

instance Homoiconic a => Homoiconic (Semigroup.Max a) where
  toCode (Semigroup.Max a) = toCode1 "Max" a
  parseCode = parseCode1 "Max" Semigroup.Max

instance Homoiconic a => Homoiconic (Semigroup.Min a) where
  toCode (Semigroup.Min a) = toCode1 "Min" a
  parseCode = parseCode1 "Min" Semigroup.Min

#if !MIN_VERSION_ghc(9,0,0)
instance Homoiconic a => Homoiconic (Semigroup.Option a) where
  toCode (Semigroup.Option a) = toCode1 "Option" a
  parseCode = parseCode1 "Option" Semigroup.Option
#endif

instance Homoiconic a => Homoiconic (Semigroup.WrappedMonoid a) where
  toCode (Semigroup.WrapMonoid a) = toCode1 "WrapMonoid" a
  parseCode = parseCode1 "WrapMonoid" Semigroup.WrapMonoid

--
-- Data.Word
--

instance Homoiconic Word8 where
  toCode = integralToCode
  parseCode = integralFromCode

instance Homoiconic Word16 where
  toCode = integralToCode
  parseCode = integralFromCode

instance Homoiconic Word32 where
  toCode = integralToCode
  parseCode = integralFromCode

instance Homoiconic Word64 where
  toCode = integralToCode
  parseCode = integralFromCode

--
-- Numeric.Natural
--

instance Homoiconic Natural where
  toCode = integralToCode
  parseCode = integralFromCode

--
-- Language.Finkel.Form
--

instance Homoiconic Atom where
  toCode = LForm . genSrc . Atom
  parseCode a =
    case unCode a of
      Atom x -> pure x
      _      -> failedToParse "Atom"

instance Homoiconic (Form Atom) where
  toCode = LForm . genSrc
  parseCode = pure . unCode

instance Homoiconic (LForm Atom) where
  toCode = id
  parseCode = pure


-- -------------------------------------------------------------------
--
-- Generic toCode
--
-- -------------------------------------------------------------------

-- | Generic variant of 'toCode'.
genericToCode :: (Generic a, GToCode (Rep a)) => a -> Code
genericToCode = unCodeArgs . gToCode . from
{-# INLINABLE genericToCode #-}

-- | To distinguish arguments of constructor from non-argument.
data CodeArgs
  = NonArg Code
  | Args [Code]

unCodeArgs :: CodeArgs -> Code
unCodeArgs ca = case ca of
  NonArg c -> c
  Args cs  -> toCode (List cs)
{-# INLINABLE unCodeArgs #-}

instance Semigroup.Semigroup CodeArgs where
  Args xs <> Args ys   = Args (xs Semigroup.<> ys)
  Args xs <> NonArg y  = Args (xs Semigroup.<> [y])
  NonArg x <> Args ys  = Args (x : ys)
  NonArg x <> NonArg y = Args [x, y]
  {-# INLINE (<>) #-}

-- | For making 'Code' with 'Generic' instances.
class GToCode f where
  gToCode :: f a -> CodeArgs

instance GToCode V1 where
  gToCode _ = NonArg undefined
  {-# INLINE gToCode #-}

instance GToCode U1 where
  gToCode U1 = NonArg nil
  {-# INLINE gToCode #-}

instance (GToCode f, GToCode g) => GToCode (f :+: g) where
  gToCode lr = case lr of
    L1 x -> gToCode x
    R1 x -> gToCode x
  {-# INLINE gToCode #-}

instance (GToCode f, GToCode g) => GToCode (f :*: g) where
  gToCode (f :*: g) = gToCode f Semigroup.<> gToCode g
  {-# INLINE gToCode #-}

instance Homoiconic c => GToCode (K1 i c) where
  gToCode (K1 x) = NonArg (toCode x)
  {-# INLINE gToCode #-}

instance GToCode f => GToCode (M1 D c f) where
  gToCode (M1 x) = gToCode x
  {-# INLINE gToCode #-}

instance (Constructor c, GToCode f) => GToCode (M1 C c f) where
  gToCode m1@(M1 x) =
    let constr = toCode (aSymbol (conName m1))
    in  case gToCode x of
      NonArg c -> if null c
        then NonArg constr
        else NonArg (toCode (List [constr, c]))
      Args cs -> NonArg (toCode (List (constr : cs)))
  {-# INLINE gToCode #-}

instance GToCode f => GToCode (M1 S c f) where
  gToCode (M1 x) = gToCode x
  {-# INLINE gToCode #-}


-- -------------------------------------------------------------------
--
-- Generic FromCode
--
-- -------------------------------------------------------------------

-- | Generic variant of 'fromCode'.
genericFromCode :: (Generic a, GParseCode (Rep a)) => Code -> Maybe a
genericFromCode x = case genericParseCode x of
  Success a -> Just a
  _         -> Nothing
{-# INLINABLE genericFromCode #-}

-- | Generic function to get result value from 'Code'.
genericParseCode :: (Generic a, GParseCode (Rep a)) => Code -> Result a
genericParseCode =
  let f a xs = if null xs
                 then pure (to a)
                 else fail "Unexpected leftover"
  in  runCodeP gParseCode fail f
{-# INLINABLE genericParseCode #-}

-- | For getting value from 'Code' with 'Generic' instances.
class GParseCode f where
  gParseCode :: CodeP (f a)

instance GParseCode V1 where
  gParseCode = pure undefined
  {-# INLINE gParseCode #-}

instance GParseCode U1 where
  gParseCode = pure U1
  {-# INLINE gParseCode #-}

instance (GParseCode f, GParseCode g) => GParseCode (f :+: g) where
  gParseCode = fmap L1 gParseCode <|> fmap R1 gParseCode
  {-# INLINE gParseCode #-}

instance (GParseCode f, GParseCode g) => GParseCode (f :*: g) where
  gParseCode = (:*:) <$> gParseCode <*> gParseCode
  {-# INLINE gParseCode #-}

instance Homoiconic c => GParseCode (K1 i c) where
  gParseCode =
    unconsP (\l c cs ->
              case parseCode c of
                Success a -> contP (K1 a) (LForm (L l (List cs)))
                _         -> failP ("Unexpected: " ++ show c))
  {-# INLINE gParseCode #-}

instance GParseCode f => GParseCode (M1 D c f) where
  gParseCode = fmap M1 gParseCode
  {-# INLINE gParseCode #-}

instance {-# OVERLAPPABLE #-} Constructor c => GParseCode (M1 C c U1) where
  gParseCode =
    let c1 :: M1 C c U1 a
        c1 = undefined
    in  eqP (toCode (aSymbol (conName c1))) *> fmap M1 gParseCode
  {-# INLINE gParseCode #-}

instance {-# OVERLAPPABLE #-} (Constructor c, GParseCode f)
  => GParseCode (M1 C c f) where
  gParseCode =
    let c1 :: M1 C c f a
        c1 = undefined
    in  eqCarP (toCode (aSymbol (conName c1))) *> fmap M1 gParseCode
  {-# INLINE gParseCode #-}

instance GParseCode f => GParseCode (M1 S c f) where
  gParseCode = fmap M1 gParseCode
  {-# INLINE gParseCode #-}


-- -------------------------------------------------------------------
--
-- Code parser for GParseCode
--
-- -------------------------------------------------------------------

-- | Dedicated data type to hold parsed result of 'Code'.
--
-- Using dedicated data type when parsing 'Code' data type for 'parseCode'. This
-- data type is intentionally not defined as an instance of 'Homoiconic', so
-- that the user defined data types can tell the parse error from explicit
-- failure constructor of the target type, e,g, 'Nothing' for 'Maybe', 'Left'
-- for 'Either', ... etc.
data Result a
  = Success a
  | Failure String
  deriving (Eq, Show)

instance Functor Result where
  fmap f r = case r of
    Success a -> Success (f a)
    Failure e -> Failure e
  {-# INLINE fmap #-}

instance Applicative Result where
  pure = Success
  {-# INLINE pure #-}
  f <*> m = f >>= flip fmap m
  {-# INLINE (<*>) #-}

instance Monad Result where
  m >>= k = case m of
    Success a -> k a
    Failure e -> Failure e
  {-# INLINE (>>=) #-}

instance MonadFail Result where
  fail = Failure
  {-# INLINE fail #-}

failedToParse :: String -> Result a
failedToParse ty = Failure ("Failed to parse " ++ ty)
{-# INLINABLE failedToParse #-}

-- | Simple parser for 'Code'.
newtype CodeP a =
  CodeP {runCodeP :: forall r. (String -> r) -- On failure
                  -> (a -> Code -> r)        -- On success
                  -> Code                    -- Input
                  -> r}

instance Functor CodeP where
  fmap f p = CodeP (\err go -> runCodeP p err (go . f))
  {-# INLINE fmap #-}

instance Applicative CodeP where
  pure a = CodeP (\_ go -> go a)
  {-# INLINE pure #-}

  f <*> p = f >>= flip fmap p
  {-# INLINE (<*>) #-}

instance Monad CodeP where
  m >>= k = CodeP (\err go -> runCodeP m err (\a -> runCodeP (k a) err go))
  {-# INLINE (>>=) #-}

instance Alternative CodeP where
  empty = failP "Alternative.empty"
  {-# INLINE empty #-}

  p1 <|> p2 =
    CodeP (\err go cs ->
            runCodeP p1 (\_ -> runCodeP p2 err go cs) go cs)
  {-# INLINE (<|>) #-}

failP :: String -> CodeP a
failP msg = CodeP (\err _ _ -> err msg)
{-# INLINABLE failP #-}

contP :: a -> Code -> CodeP a
contP a cs = CodeP (\_ go _ -> go a cs)
{-# INLINEABLE contP #-}

unconsP :: (SrcSpan -> Code -> [Code] -> CodeP a) -> CodeP a
unconsP f =
  CodeP (\err go cs ->
    case cs of
      LForm (L l (List (x : xs))) -> runCodeP (f l x xs) err go cs
      _                           -> err "Not a list")
{-# INLINEABLE unconsP #-}

eqP :: Code -> CodeP ()
eqP x =
  CodeP (\err go cs ->
    if cs == x
      then go () nil
      else err ("eqP: unexpected " ++ show cs))
{-# INLINABLE eqP #-}

eqCarP :: Code -> CodeP ()
eqCarP x =
  unconsP (\l c cs ->
             if x == c
               then contP () (LForm (L l (List cs)))
               else failP ("eqCarP: unexpected " ++ show c))
{-# INLINABLE eqCarP #-}


-- -------------------------------------------------------------------
--
-- Data to Code
--
-- -------------------------------------------------------------------

dataToCode :: Data d => d -> Code
dataToCode x =
  let constr = toConstr x
      isTupleStr cs = case cs of
                        '(':cs1 -> go cs1
                        _       -> False
                        where go xs = case xs of
                                        ',':xs' -> go xs'
                                        [')']   -> True
                                        _       -> False
      cstr = case showConstr constr of
               str | isTupleStr str -> ","
                   | otherwise  -> str
      hd = toCode (aSymbol cstr)
  in  case constrRep constr of
         IntConstr n   -> toCode (aIntegral n)
         FloatConstr f -> toCode (aFractional (fromRational f :: Double))
         CharConstr c  -> toCode c
         _             ->
           case gmapQ dataToCode x of
             [] -> hd
             _  -> toCode (List (hd:gmapQ dataToCode x))


-- -------------------------------------------------------------------
--
-- Auxiliary
--
-- -------------------------------------------------------------------

realFracToCode :: (Real a, Show a) => a -> Code
realFracToCode = LForm . genSrc . Atom . aFractional
{-# INLINABLE realFracToCode #-}

fractionalFromCode :: Fractional a => Code -> Result a
fractionalFromCode a =
  case unCode a of
    Atom (AFractional x) -> pure (fromRational (fl_value x))
    _                    -> failedToParse "fractional"
{-# INLINABLE fractionalFromCode #-}

symbolCode :: String -> Code
symbolCode = LForm . genSrc . Atom . aSymbol
{-# INLINABLE symbolCode #-}

showAsSymbolCode :: Show a => a -> Code
showAsSymbolCode = symbolCode . show
{-# INLINABLE showAsSymbolCode #-}

integralToCode :: Integral a => a -> Code
integralToCode = LForm . genSrc . Atom . aIntegral
{-# INLINABLE integralToCode #-}

integralFromCode :: Integral a => Code -> Result a
integralFromCode a =
  case unCode a of
    Atom (AInteger n) -> pure (fromIntegral (il_value n))
    _                 -> failedToParse "integral"

toCode1 :: Homoiconic a => FastString -> a -> Code
toCode1 tag arg1 =
  toCode (List [LForm (genSrc (Atom (ASymbol tag))), toCode arg1])
{-# INLINABLE toCode1 #-}

toCode2 :: (Homoiconic a, Homoiconic b) => FastString -> a -> b -> Code
toCode2 tag arg1 arg2 =
  toCode (List [ LForm (genSrc (Atom (ASymbol tag)))
               , toCode arg1, toCode arg2 ])
{-# INLINABLE toCode2 #-}

parseCode1 :: (Homoiconic a) => FastString -> (a -> h) -> Code -> Result h
parseCode1 tag f a =
  case unCode a of
    List [LForm (L _ (Atom (ASymbol tag'))), x]
      | tag == tag' -> f <$> parseCode x
    _               -> failedToParse (unpackFS tag)
{-# INLINABLE parseCode1 #-}

parseCode2 :: (Homoiconic a, Homoiconic b)
          => FastString -> (a -> b -> h) -> Code -> Result h
parseCode2 tag f a =
  case unCode a of
    List [LForm (L _ (Atom (ASymbol tag'))), x, y]
      | tag == tag' -> f <$> parseCode x <*> parseCode y
    _               -> failedToParse (unpackFS tag)
{-# INLINABLE parseCode2 #-}
