{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
-- | S-expression form data.
module Language.Finkel.Form
  (
  -- * Types
    Code
  , Atom(..)
  , Form(..)
  , LForm(..)

  -- * Constructor functions
  , qSymbol
  , qChar
  , qString
  , qInteger
  , qFractional
  , qUnit
  , qList
  , qHsList
  , nil

  -- * Auxiliary functions
  , aFractional
  , aIntegral
  , aSymbol
  , aString
  , genSrc
  , mkLocatedForm
  , showLoc
  , toListL
  , unCode

  -- * Re-export
  , IntegralLit (..)
  , mkIntegralLit
  ) where

-- base
import Data.Data       (Data, Typeable)
import Data.Function   (on)
import GHC.Generics    (Generic)

-- ghc
import BasicTypes      (FractionalLit (..), SourceText (..))
import FastString      (FastString, fsLit, unpackFS)
import Outputable      (Outputable (..), brackets, cat, char, double,
                        doubleQuotes, fsep, integer, parens, text)
import SrcLoc          (GenLocated (..), Located, SrcSpan (..), combineLocs,
                        combineSrcSpans, srcSpanFile, srcSpanStartCol,
                        srcSpanStartLine)

#if MIN_VERSION_ghc(8,4,0)
import BasicTypes      (IntegralLit (..), mkFractionalLit, mkIntegralLit)
#endif

-- deepseq
import Control.DeepSeq (NFData (..))


-- -------------------------------------------------------------------
--
-- Form data type
--
-- -------------------------------------------------------------------

-- | Atom in tokens.
data Atom
  = AUnit
  | ASymbol                {-# UNPACK #-} !FastString
  | AChar       SourceText {-# UNPACK #-} !Char
  | AString     SourceText {-# UNPACK #-} !FastString
  | AInteger               {-# UNPACK #-} !IntegralLit
  | AFractional            {-# UNPACK #-} !FractionalLit
  deriving (Data, Typeable, Generic)

instance Eq Atom where
  AUnit         == AUnit         = True
  ASymbol x     == ASymbol y     = x == y
  AChar _ x     == AChar _ y     = x == y
  AString _ x   == AString _ y   = x == y
  AInteger x    == AInteger y    = x == y
  AFractional x == AFractional y = x == y
  _             == _             = False
  {-# INLINE (==) #-}

instance Show Atom where
  showsPrec d x =
    case x of
      AUnit -> showString "()"
      ASymbol s -> showString (unpackFS s)
      AChar _ c -> showString $ case c of
        '\a' -> "#'\\BEL"
        '\b' -> "#'\\BS"
        '\f' -> "#'\\FF"
        '\n' -> "#'\\LF"
        '\r' -> "#'\\CR"
        '\t' -> "#'\\HT"
        '\v' -> "#'\\VT"
        ' '  -> "#'\\SP"
        _    -> ['#', '\'', c]
      AString _ s -> showsPrec d s
      AInteger il -> showsPrec d (il_value il)
      AFractional f -> showString (fl_text_compat f)

instance NFData Atom where
  rnf x =
    case x of
      AUnit         -> ()
      ASymbol fs    -> seq fs ()
      AChar _ c     -> seq c ()
      AString _ str -> seq str ()
      AInteger i    -> rnf (il_value i)
      AFractional y -> seq y ()

instance Outputable Atom where
  ppr form =
    case form of
      AUnit         -> ppr ()
      ASymbol x     -> ppr x
      AChar _ x     -> cat [text "#'",  char x]
      AString _ x   -> doubleQuotes (ppr x)
      AInteger x    -> integer (il_value x)
      AFractional x -> double (fromRational (fl_value x))

-- | Form type. Also used as token. Elements of recursive structures
-- contain location information.
data Form a
  = Atom a           -- ^ S-expression atom.
  | List [LForm a]   -- ^ S-expression list.
  | HsList [LForm a] -- ^ Haskell list.
  | TEnd             -- ^ End of token.
  deriving (Eq, Data, Typeable, Generic)

instance Show a => Show (Form a) where
  showsPrec d form =
    case form of
      Atom a    -> showsPrec d a
      List xs   -> showL (Just "nil") '(' ')' xs
      HsList xs -> showL Nothing '[' ']' xs
      TEnd      -> showString "TEnd"
    where
      showL mb_nil open close xs next =
        case xs of
          []    -> maybe (open : close : next)
                         (++ next)
                         mb_nil
          x:xs' -> open : shows x (showL' close xs' next)
      showL' close xs next =
        case xs of
          []   -> close : next
          y:ys -> ' ' : shows y (showL' close ys next)

instance Functor Form where
  fmap f form =
    case form of
      Atom a    -> Atom (f a)
      List xs   -> List (map (fmap f) xs)
      HsList xs -> HsList (map (fmap f) xs)
      TEnd      -> TEnd
  {-# INLINE fmap #-}

instance Foldable Form where
  foldr f z form =
    case form of
      TEnd    -> z
      Atom x  -> f x z
      List xs ->
        case xs of
          []   -> z
          y:ys -> foldr f (foldr f z (List ys)) (unCode y)
      HsList xs ->
        case xs of
          []   -> z
          y:ys -> foldr f (foldr f z (HsList ys)) (unCode y)
  {-# INLINE foldr #-}

instance Traversable Form where
  traverse f form =
    case form of
      Atom x    -> fmap Atom (f x)
      List xs   -> fmap List (traverse (traverse f) xs)
      HsList xs -> fmap HsList (traverse (traverse f) xs)
      TEnd      -> pure TEnd
  {-# INLINE traverse #-}

instance NFData a => NFData (Form a) where
  rnf x =
    case x of
      Atom a    -> rnf a
      List as   -> rnf as
      HsList as -> rnf as
      TEnd      -> ()

instance Outputable a => Outputable (Form a) where
  ppr x =
    case x of
      Atom a    -> ppr a
      List xs   -> parens (fsep (map ppr xs))
      HsList xs -> brackets (fsep (map ppr xs))
      TEnd      -> text ""

instance Num (Form Atom) where
  (+) = nop2 aIntegral (+) (+)
  (*) = nop2 aIntegral (*) (*)
  negate = nop1 aIntegral negate negate
  abs = nop1 aIntegral abs abs
  signum = nop1 aIntegral signum signum
  fromInteger = Atom . aIntegral

instance Fractional (Form Atom) where
  (/) = nop2 aDouble ((/) `on` fromIntegral) (/)
  recip = nop1 aDouble (recip . fromInteger) recip
  fromRational = Atom . aDouble. fromRational

#if MIN_VERSION_ghc(8,4,0)
instance Semigroup (Form a) where
  Atom a <> Atom b = List [atomForm a, atomForm b]
  Atom a <> List bs = List (atomForm a : bs)
  Atom a <> HsList bs = List (atomForm a : bs)

  List as <> Atom b = List (as <> [atomForm b])
  List as <> List bs = List (as <> bs)
  List as <> HsList bs = List (as <> bs)

  HsList as <> Atom b = List (as <> [atomForm b])
  HsList as <> List bs = List (as <> bs)
  HsList as <> HsList bs = List (as <> bs)

  TEnd <> b = b
  a <> TEnd = a
  {-# INLINE (<>) #-}

instance Monoid (Form a) where
  mempty = List []
  {-# INLINE mempty #-}
#else
instance Monoid (Form a) where
  Atom a `mappend` Atom b = List [atomForm a, atomForm b]
  Atom a `mappend` List bs = List (atomForm a : bs)
  Atom a `mappend` HsList bs = List (atomForm a : bs)

  List as `mappend` Atom b = List (as `mappend` [atomForm b])
  List as `mappend` List bs = List (as `mappend` bs)
  List as `mappend` HsList bs = List (as `mappend` bs)

  HsList as `mappend` Atom b = List (as `mappend` [atomForm b])
  HsList as `mappend` List bs = List (as `mappend` bs)
  HsList as `mappend` HsList bs = List (as `mappend` bs)

  TEnd `mappend` b = b
  a `mappend` TEnd = a
  {-# INLINE mappend #-}

  mempty = List []
  {-# INLINE mempty #-}
#endif

-- | Newtype wrapper for located 'Form'.
newtype LForm a = LForm {unLForm :: Located (Form a)}
  deriving (Data, Typeable, Generic)

instance Eq a => Eq (LForm a) where
  LForm (L _ a) == LForm (L _ b) = a == b

instance Show a => Show (LForm a) where
  showsPrec d (LForm (L _ a)) = showsPrec d a

instance Functor LForm where
  fmap f (LForm (L l a)) = LForm (L l (fmap f a))
  {-# INLINE fmap #-}

instance Foldable LForm where
  foldr f z (LForm (L _ form)) = foldr f z form
  {-# INLINE foldr #-}

instance Traversable LForm where
  traverse f (LForm (L l form)) = fmap (LForm . L l) (traverse f form)
  {-# INLINE traverse #-}

instance NFData a => NFData (LForm a) where
  rnf (LForm (L l a)) = rnf l `seq` rnf a

instance Outputable a => Outputable (LForm a) where
  ppr (LForm (L _ a)) = ppr a

#if MIN_VERSION_ghc(8,4,0)
instance Semigroup (LForm a) where
  LForm (L l a) <> LForm (L r b) = LForm (L (combineSrcSpans l r) (a <> b))
  {-# INLINE (<>) #-}

instance Monoid (LForm a) where
  mempty = LForm (genSrc mempty)
  {-# INLINE mempty #-}
#else
instance Monoid (LForm a) where
  LForm (L l a) `mappend` LForm (L r b) =
    LForm (L (combineSrcSpans l r) (a `mappend` b))
  {-# INLINE mappend #-}
  mempty = LForm (genSrc mempty)
  {-# INLINE mempty #-}
#endif

-- | Type synonym for code data.
--
-- The 'Code' data is the fundamental data type used in the entire compilation
-- work.  The 'Code' is used to represed data from parsed source file, and used
-- for input and output of macros transformer functions. List of 'Code' data are
-- converted to Haskell AST via syntax parser.
--
-- Since 'Code' is returned from parsed source file, source code location
-- information is attached to 'Code'.
--
type Code = LForm Atom

instance Num Code where
  (+) = liftLF2 (+)
  (*) = liftLF2 (*)
  negate = liftLF negate
  abs = liftLF abs
  signum  = liftLF signum
  fromInteger = LForm . genSrc . fromInteger

instance Fractional Code where
  (/) = liftLF2 (/)
  recip = liftLF recip
  fromRational = LForm . genSrc . fromRational

liftLF :: (Form Atom -> Form Atom) -> Code -> Code
liftLF f (LForm (L l a)) = LForm (L l (f a))
{-# INLINE liftLF #-}

liftLF2 :: (Form Atom -> Form Atom -> Form Atom) -> Code -> Code -> Code
liftLF2 f (LForm (L l1 a)) (LForm (L _l2 b)) = LForm (L l1 (f a b))
{-# INLINE liftLF2 #-}

-- -------------------------------------------------------------------
--
-- Constructor functions
--
-- -------------------------------------------------------------------

-- | Make quoted symbol from 'String'.
qSymbol :: String -> Code
qSymbol = quoted . Atom . aSymbol

-- | Make quoted char from 'Char'.
qChar :: Char -> Code
qChar = quoted . Atom . AChar NoSourceText

-- | Make quoted string from 'String'.
qString :: String -> Code
qString = quoted . Atom . aString NoSourceText

-- | Make quoted integer from 'Integer'.
qInteger :: Integer -> Code
qInteger = quoted . Atom . AInteger . mkIntegralLit

-- | Make quoted fractional from read value.
qFractional :: (Real a, Show a) => a -> Code
qFractional = quoted . Atom . aFractional

-- | Make quoted unit.
qUnit :: Code
qUnit = quoted (Atom AUnit)

-- | Make quoted list from list of 'Code'.
qList :: [Code] -> Code
qList = quoted . List

-- | Make quoted haskell list from list of 'Code'.
qHsList :: [Code] -> Code
qHsList = quoted . HsList

-- | Auxiliary function to construct 'ASymbol' atom.
aSymbol :: String -> Atom
aSymbol = ASymbol . fsLit
{-# INLINE aSymbol #-}

-- | Auxiliary function to construct 'AString' atom.
aString :: SourceText -> String -> Atom
aString st = AString st . fsLit
{-# INLINE aString #-}

-- | Auxiliary function to construct an 'Atom' containing
-- 'FractionalLit' value from literal fractional numbers.
aFractional :: (Real a, Show a) => a -> Atom
aFractional x = AFractional $! mkFractionalLit x
{-# SPECIALIZE aFractional :: Double -> Atom #-}
{-# SPECIALIZE aFractional :: Float -> Atom #-}

-- | Type fixed variant of 'aFractional'.
aDouble :: Double -> Atom
aDouble = aFractional
{-# INLINE aDouble #-}

aIntegral :: Integral a => a -> Atom
aIntegral x = AInteger $! mkIntegralLit x
{-# SPECIALIZE aIntegral :: Integer -> Atom #-}
{-# SPECIALIZE aIntegral :: Int -> Atom #-}

-- | A form with empty 'List'.
nil :: Code
nil = LForm (genSrc (List []))

quoted :: Form Atom -> Code
quoted = LForm . L (UnhelpfulSpan (fsLit "<quoted code>"))
{-# INLINE quoted #-}


-- -------------------------------------------------------------------
--
-- Auxiliary
--
-- -------------------------------------------------------------------

-- | String representation of located data.
showLoc :: LForm a -> String
showLoc (LForm (L l _)) =
  case l of
    RealSrcSpan r    ->
      unpackFS (srcSpanFile r) ++ ":" ++
      show (srcSpanStartLine r) ++ ":" ++
      show (srcSpanStartCol r) ++ ": "
    UnhelpfulSpan fs -> unpackFS fs ++ ": "

-- | Make 'List' from given code. When the given argument was already a 'List',
-- the given 'List' is returned. If the argument was 'HsList', converted to
-- 'List'. Otherwise, 'List' with single element.
toListL :: Code -> Code
toListL orig@(LForm (L l form)) =
  case form of
    List _    -> orig
    HsList xs -> LForm (L l (List xs))
    _         -> LForm (L l (List [orig]))
{-# INLINE toListL #-}

-- | Unwrap 'LForm' to 'Form'.
unCode :: LForm a -> Form a
unCode (LForm (L _ a)) = a
{-# INLINE unCode #-}

-- | Attach location to mark generated code.
genSrc :: a -> Located a
genSrc = L (UnhelpfulSpan (fsLit "<finkel generated code>"))
{-# INLINE genSrc #-}

-- | Make located list from list of located elements.
--
-- When the argument is not null, the resulting list has a combined location of
-- locations in the argument list elements.
mkLocatedForm :: [LForm a] -> Located [LForm a]
mkLocatedForm [] = genSrc []
mkLocatedForm ms = L (combineLocs (unLForm (head ms))
                                  (unLForm (last ms)))
                     ms

-- | Lift given argument to 'LForm'.
atomForm :: a -> LForm a
atomForm = LForm . genSrc . Atom
{-# INLINE atomForm #-}

-- | Unary numeric operation helper.
nop1 :: (a -> Atom)
     -> (Integer -> a)
     -> (Rational -> Rational)
     -> Form Atom -> Form Atom
nop1 c f _ (Atom (AInteger il))    = Atom (c (f (il_value il)))
nop1 _ _ f (Atom (AFractional fl)) = Atom (aFractional (f (fl_value fl)))
nop1 _ _ _ _                       = List []

-- | Binary numeric operation helper.
nop2 :: (a -> Atom)
     -> (Integer -> Integer -> a)
     -> (Rational -> Rational -> Rational)
     -> Form Atom -> Form Atom -> Form Atom
nop2 c f _ (Atom (AInteger il1)) (Atom (AInteger il2)) =
  Atom (c (on f il_value il1 il2))
nop2 _ _ f (Atom (AFractional fl1)) (Atom (AInteger il2)) =
  Atom (aFractional (f (fl_value fl1) (fromIntegral (il_value il2))))
nop2 _ _ f (Atom (AInteger il1)) (Atom (AFractional fl2)) =
  Atom (aFractional (f (fromIntegral (il_value il1)) (fl_value fl2)))
nop2 _ _ f (Atom (AFractional fl1)) (Atom (AFractional fl2)) =
  Atom (aFractional (on f fl_value fl1 fl2))
nop2 _ _ _ _ _ = List []

fl_text_compat :: FractionalLit -> String
fl_text_compat fl = str
  where
#if MIN_VERSION_ghc(8,4,0)
    str = case fl_text fl of
            NoSourceText -> error "fractional literal with no source"
            SourceText s -> s
#else
    str = fl_text fl
#endif

#if !MIN_VERSION_ghc(8,4,0)
-- | 'mkFractionalLit' did not exist in 8.2.x.
mkFractionalLit :: Real a => a -> FractionalLit
mkFractionalLit x = FL (show (realToFrac x :: Double)) (toRational x)

-- | IntegralLit back ported to 8.2.x.
data IntegralLit
  = IL { il_text  :: SourceText
       , il_neg   :: Bool
       , il_value :: Integer
       }
  deriving (Data, Show)

mkIntegralLit :: Integral a => a -> IntegralLit
mkIntegralLit i = IL { il_text = SourceText (show i_integer)
                     , il_neg = i < 0
                     , il_value = i_integer }
  where
    i_integer :: Integer
    i_integer = toInteger i

instance Eq IntegralLit where
  (==) = (==) `on` il_value

instance Ord IntegralLit where
  compare = compare `on` il_value
#endif
