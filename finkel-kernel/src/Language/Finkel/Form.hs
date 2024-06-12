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
  , QuoteFn
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
  , withLocInfo
  , asLocOf

  -- * Re-export
  , IntegralLit (..)
  , mkIntegralLit
  , FractionalLit(..)
#if MIN_VERSION_ghc(9,2,0)
  , fl_value
#endif
  , SourceText(..)
  ) where

#include "ghc_modules.h"

-- base
import           Control.Applicative             (Alternative (..))
import           Control.Monad                   (MonadPlus (..))
import           Data.Data                       (Data, Typeable)
import           Data.Function                   (on)
import           Data.Maybe                      (fromMaybe)
import           GHC.Generics                    (Generic)

-- binary
import           Data.Binary                     (Binary (..), Get, Put,
                                                  getWord8, putWord8)

-- ghc
import           GHC_Data_FastString             (FastString, fsLit, unpackFS)
import           GHC_Types_SourceText            (SourceText (..))
import           GHC_Types_SrcLoc                (GenLocated (..), Located,
                                                  RealSrcSpan (..),
                                                  SrcSpan (..), combineLocs,
                                                  combineSrcSpans, mkRealSrcLoc,
                                                  mkRealSrcSpan, mkSrcLoc,
                                                  mkSrcSpan, srcSpanEndCol,
                                                  srcSpanEndLine, srcSpanFile,
                                                  srcSpanFileName_maybe,
                                                  srcSpanStartCol,
                                                  srcSpanStartLine)
import           GHC_Utils_Outputable            (Outputable (..), brackets,
                                                  cat, char, double,
                                                  doubleQuotes, fsep, integer,
                                                  parens, text)

#if MIN_VERSION_ghc(9,4,0)
import qualified GHC.Data.Strict                 as Strict
#endif

#if MIN_VERSION_ghc(9,0,0)
import           GHC_Types_SrcLoc                (BufPos (..), BufSpan (..),
                                                  UnhelpfulSpanReason (..),
                                                  unhelpfulSpanFS)
#endif

#if MIN_VERSION_ghc(8,4,0)
import           GHC_Types_SourceText            (IntegralLit (..),
                                                  mkIntegralLit)
#endif

-- deepseq
import           Control.DeepSeq                 (NFData (..))

-- Internal
import           Language.Finkel.Data.FastString (getFastString, putFastString)
import           Language.Finkel.Data.SourceText (getSourceText, putSourceText)
import           Language.Finkel.Form.Fractional


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
      AFractional f -> showString (showFractionalList f)

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

-- | Newtype wrapper for located 'Form'.
newtype LForm a = LForm {unLForm :: Located (Form a)}
  deriving (Data, Typeable, Generic)

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


-- ------------------------------------------------------------------------
--
-- Instances
--
-- ------------------------------------------------------------------------

instance Eq a => Eq (LForm a) where
  LForm (L _ a) == LForm (L _ b) = a == b
  {-# INLINE (==) #-}

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

instance Show a => Show (LForm a) where
  showsPrec d (LForm (L _ a)) = showsPrec d a
  {-# INLINE showsPrec #-}

instance Functor Form where
  fmap f form =
    case form of
      Atom a    -> Atom (f a)
      List xs   -> List (map (fmap f) xs)
      HsList xs -> HsList (map (fmap f) xs)
      TEnd      -> TEnd
  {-# INLINE fmap #-}

instance Functor LForm where
  fmap f (LForm (L l a)) = LForm (L l (fmap f a))
  {-# INLINE fmap #-}

instance Applicative Form where
  pure = Atom
  {-# INLINE pure #-}

  Atom f <*> Atom a        = Atom (f a)
  Atom f <*> List as       = List (map (fmap f) as)
  Atom f <*> HsList as     = HsList (map (fmap f) as)

  List fs <*> a@(Atom _)   = List (fmap apLF fs <*> [a])
  List fs <*> List as      = List (fmap (<*>) fs <*> as)
  List fs <*> HsList as    = List (fmap (<*>) fs <*> as)

  HsList fs <*> a@(Atom _) = HsList (fmap apLF fs <*> [a])
  HsList fs <*> List as    = HsList (fmap (<*>) fs <*> as)
  HsList fs <*> HsList as  = HsList (fmap (<*>) fs <*> as)

  TEnd <*> _               = TEnd
  _ <*> TEnd               = TEnd
  {-# INLINE (<*>) #-}

instance Applicative LForm where
  pure = LForm . genSrc . pure
  {-# INLINE pure #-}

  LForm (L l f) <*> LForm (L _ a) = LForm (L l (f <*> a))
  {-# INLINE (<*>) #-}

instance Monad Form where
  m >>= k =
    case m of
      Atom a    -> k a
      List as   -> List (map (liftLF (>>= k)) as)
      HsList as -> HsList (map (liftLF (>>= k)) as)
      TEnd      -> TEnd
  {-# INLINE (>>=) #-}

instance Monad LForm where
  LForm (L l a) >>= k = LForm (L l (a >>= (unCode . k)))
  {-# INLINE (>>=) #-}

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

instance Foldable LForm where
  foldr f z (LForm (L _ form)) = foldr f z form
  {-# INLINE foldr #-}

instance Traversable Form where
  traverse f form =
    case form of
      Atom x    -> fmap Atom (f x)
      List xs   -> fmap List (traverse (traverse f) xs)
      HsList xs -> fmap HsList (traverse (traverse f) xs)
      TEnd      -> pure TEnd
  {-# INLINE traverse #-}

instance Traversable LForm where
  traverse f (LForm (L l form)) = fmap (LForm . L l) (traverse f form)
  {-# INLINE traverse #-}

instance NFData a => NFData (Form a) where
  rnf x =
    case x of
      Atom a    -> rnf a
      List as   -> rnf as
      HsList as -> rnf as
      TEnd      -> ()

instance NFData a => NFData (LForm a) where
  rnf (LForm (L l a)) = rnf l `seq` rnf a

instance Outputable a => Outputable (Form a) where
  ppr x =
    case x of
      Atom a    -> ppr a
      List xs   -> parens (fsep (map ppr xs))
      HsList xs -> brackets (fsep (map ppr xs))
      TEnd      -> text ""

instance Outputable a => Outputable (LForm a) where
  ppr (LForm (L _ a)) = ppr a

#if MIN_VERSION_ghc(8,4,0)
instance Semigroup (Form a) where
  Atom a <> Atom b       = List [atomForm a, atomForm b]
  Atom a <> List bs      = List (atomForm a : bs)
  Atom a <> HsList bs    = List (atomForm a : bs)

  List as <> Atom b      = List (as <> [atomForm b])
  List as <> List bs     = List (as <> bs)
  List as <> HsList bs   = List (as <> bs)

  HsList as <> Atom b    = List (as <> [atomForm b])
  HsList as <> List bs   = List (as <> bs)
  HsList as <> HsList bs = List (as <> bs)

  TEnd <> b              = b
  a <> TEnd              = a
  {-# INLINE (<>) #-}

instance Semigroup (LForm a) where
  LForm (L l a) <> LForm (L r b) = LForm (L (combineSrcSpans l r) (a <> b))
  {-# INLINE (<>) #-}

instance Monoid (Form a) where
  mempty = List []
  {-# INLINE mempty #-}

instance Monoid (LForm a) where
  mempty = LForm (genSrc mempty)
  {-# INLINE mempty #-}
#else
instance Monoid (Form a) where
  Atom a `mappend` Atom b       = List [atomForm a, atomForm b]
  Atom a `mappend` List bs      = List (atomForm a : bs)
  Atom a `mappend` HsList bs    = List (atomForm a : bs)

  List as `mappend` Atom b      = List (as `mappend` [atomForm b])
  List as `mappend` List bs     = List (as `mappend` bs)
  List as `mappend` HsList bs   = List (as `mappend` bs)

  HsList as `mappend` Atom b    = List (as `mappend` [atomForm b])
  HsList as `mappend` List bs   = List (as `mappend` bs)
  HsList as `mappend` HsList bs = List (as `mappend` bs)

  TEnd `mappend` b              = b
  a `mappend` TEnd              = a
  {-# INLINE mappend #-}

  mempty = List []
  {-# INLINE mempty #-}

instance Monoid (LForm a) where
  LForm (L l a) `mappend` LForm (L r b) =
    LForm (L (combineSrcSpans l r) (a `mappend` b))
  {-# INLINE mappend #-}

  mempty = LForm (genSrc mempty)
  {-# INLINE mempty #-}
#endif

instance Alternative Form where
  empty = mempty
  {-# INLINE empty #-}
  (<|>) = mappend
  {-# INLINE (<|>) #-}

instance Alternative LForm where
  empty = mempty
  {-# INLINE empty #-}
  (<|>) = mappend
  {-# INLINE (<|>) #-}

instance MonadPlus Form

instance MonadPlus LForm

instance Num (Form Atom) where
  (+) = nop2 aIntegral (+) (+)
  {-# INLINE (+) #-}
  (*) = nop2 aIntegral (*) (*)
  {-# INLINE (*) #-}
  negate = nop1 aIntegral negate negate
  {-# INLINE negate #-}
  abs = nop1 aIntegral abs abs
  {-# INLINE abs #-}
  signum = nop1 aIntegral signum signum
  {-# INLINE signum #-}
  fromInteger = Atom . aIntegral
  {-# INLINE fromInteger #-}

instance Num Code where
  (+) = liftLF2 (+)
  {-# INLINE (+) #-}
  (*) = liftLF2 (*)
  {-# INLINE (*) #-}
  negate = liftLF negate
  {-# INLINE negate #-}
  abs = liftLF abs
  {-# INLINE abs #-}
  signum  = liftLF signum
  {-# INLINE signum #-}
  fromInteger = LForm . genSrc . fromInteger
  {-# INLINE fromInteger #-}

instance Fractional (Form Atom) where
  (/) = nop2 aDouble ((/) `on` fromIntegral) (/)
  {-# INLINE (/) #-}
  recip = nop1 aDouble (recip . fromInteger) recip
  {-# INLINE recip #-}
  fromRational = Atom . aDouble. fromRational
  {-# INLINE fromRational #-}

instance Fractional Code where
  (/) = liftLF2 (/)
  {-# INLINE (/) #-}
  recip = liftLF recip
  {-# INLINE recip #-}
  fromRational = LForm . genSrc . fromRational
  {-# INLINE fromRational #-}


-- -------------------------------------------------------------------
--
-- Instances for classes from binary package
--
-- -------------------------------------------------------------------

instance Binary Atom where
  put x = case x of
    AUnit          -> putWord8 0
    ASymbol fs     -> putWord8 1 >> putFastString fs
    AChar st c     -> putWord8 2 >> putSourceText st >> put c
    AString st fs  -> putWord8 3 >> putSourceText st >> putFastString fs
    AInteger il    -> putWord8 4 >> putIntegralLit il
    AFractional fl -> putWord8 5 >> putFractionalLit fl
  {-# INLINE put #-}

  get = do
    t <- getWord8
    case t of
      0 -> pure AUnit
      1 -> ASymbol <$> getFastString
      2 -> AChar <$> getSourceText <*> get
      3 -> AString <$> getSourceText <*> getFastString
      4 -> AInteger <$> getIntegralLit
      5 -> AFractional <$> getFractionalLit
      _ -> error $ "get: unknown tag " ++ show t
  {-# INLINE get #-}

putIntegralLit :: IntegralLit -> Put
putIntegralLit il =
  putSourceText (il_text il) *> put (il_neg il) *> put (il_value il)
{-# INLINABLE putIntegralLit #-}

getIntegralLit :: Get IntegralLit
getIntegralLit = IL <$> getSourceText <*> get <*> get
{-# INLINABLE getIntegralLit #-}

instance Binary a => Binary (Form a) where
  put form = case form of
    Atom x    -> putWord8 0 *> put x
    List xs   -> putWord8 1 *> put xs
    HsList xs -> putWord8 2 *> put xs
    TEnd      -> putWord8 3
  {-# INLINE put #-}

  get = do
    t <- getWord8
    case t of
      0 -> Atom <$> get
      1 -> List <$> get
      2 -> HsList <$> get
      3 -> pure TEnd
      _ -> error $ "getForm: unknown tag " ++ show t
  {-# INLINE get #-}

instance Binary a => Binary (LForm a) where
  put (LForm (L l a)) = putSrcSpan l *> put a
  {-# INLINE put #-}

  get = LForm <$> (L <$> getSrcSpan <*> get)
  {-# INLINE get #-}

#if MIN_VERSION_ghc(9,0,0)
putSrcSpan :: SrcSpan -> Put
putSrcSpan s = case s of
  RealSrcSpan p mb     -> putWord8 0 *> putRealSrcSpan p *> putMbBufSpan mb
  UnhelpfulSpan reason -> putWord8 1 *> putUnhelpfulSpanReason reason

getSrcSpan :: Get SrcSpan
getSrcSpan = do
  t <- getWord8
  case t of
    0 -> RealSrcSpan <$> getRealSrcSpan <*> getMbBufSpan
    1 -> UnhelpfulSpan <$> getUnhelpfulSpanReason
    _ -> error $ "getSrcSpan: unknown tag " ++ show t

#  if MIN_VERSION_ghc(9,4,0)
putMbBufSpan :: Strict.Maybe BufSpan -> Put
putMbBufSpan mb_bs = case mb_bs of
  Strict.Just (BufSpan s e) -> putWord8 0 *> putBufPos s *> putBufPos e
  Strict.Nothing            -> putWord8 1

getMbBufSpan :: Get (Strict.Maybe BufSpan)
getMbBufSpan = do
  t <- getWord8
  case t of
    0 -> Strict.Just <$> (BufSpan <$> getBufPos <*> getBufPos)
    1 -> pure Strict.Nothing
    _ -> error $ "getMbBufSpan: unknown tag " ++ show t
#  else
putMbBufSpan :: Maybe BufSpan -> Put
putMbBufSpan mb_bs = case mb_bs of
  Just (BufSpan s e) -> putWord8 0 *> putBufPos s *> putBufPos e
  Nothing            -> putWord8 1

getMbBufSpan :: Get (Maybe BufSpan)
getMbBufSpan = do
  t <- getWord8
  case t of
    0 -> Just <$> (BufSpan <$> getBufPos <*> getBufPos)
    1 -> pure Nothing
    _ -> error $ "getMbBufSpan: unknown tag " ++ show t
#  endif

{-# INLINABLE putMbBufSpan #-}
{-# INLINABLE getMbBufSpan #-}

putBufPos :: BufPos -> Put
putBufPos (BufPos p) = put p
{-# INLINABLE putBufPos #-}

getBufPos :: Get BufPos
getBufPos = BufPos <$> get
{-# INLINABLE getBufPos #-}

putUnhelpfulSpanReason :: UnhelpfulSpanReason -> Put
putUnhelpfulSpanReason r = case r of
  UnhelpfulNoLocationInfo -> putWord8 0
  UnhelpfulWiredIn        -> putWord8 1
  UnhelpfulInteractive    -> putWord8 2
  UnhelpfulGenerated      -> putWord8 3
  UnhelpfulOther fs       -> putWord8 4 *> putFastString fs
{-# INLINABLE putUnhelpfulSpanReason #-}

getUnhelpfulSpanReason :: Get UnhelpfulSpanReason
getUnhelpfulSpanReason = do
  t <- getWord8
  case t of
    0 -> pure UnhelpfulNoLocationInfo
    1 -> pure UnhelpfulWiredIn
    2 -> pure UnhelpfulInteractive
    3 -> pure UnhelpfulGenerated
    4 -> UnhelpfulOther <$> getFastString
    _ -> error $ "getUnhelpfulSpanReason: unknown tag " ++ show t
{-# INLINABLE getUnhelpfulSpanReason #-}

#else
putSrcSpan :: SrcSpan -> Put
putSrcSpan s = case s of
  RealSrcSpan rs   -> putWord8 0 *> putRealSrcSpan rs
  UnhelpfulSpan fs -> putWord8 1 *> putFastString fs

getSrcSpan :: Get SrcSpan
getSrcSpan = do
  t <- getWord8
  case t of
    0 -> RealSrcSpan <$> getRealSrcSpan
    1 -> UnhelpfulSpan <$> getFastString
    _ -> error $ "getSrcSpan: unknown tag " ++ show t
#endif

{-# INLINABLE putSrcSpan #-}
{-# INLINABLE getSrcSpan #-}

putRealSrcSpan :: RealSrcSpan -> Put
putRealSrcSpan rs = do
  putFastString (srcSpanFile rs)
  put (srcSpanStartLine rs)
  put (srcSpanStartCol rs)
  put (srcSpanEndLine rs)
  put (srcSpanEndCol rs)
{-# INLINEABLE putRealSrcSpan #-}

getRealSrcSpan :: Get RealSrcSpan
getRealSrcSpan = do
  fs <- getFastString
  mkRealSrcSpan
    <$> (mkRealSrcLoc fs <$> get <*> get)
    <*> (mkRealSrcLoc fs <$> get <*> get)
{-# INLINEABLE getRealSrcSpan #-}

-- -------------------------------------------------------------------
--
-- Constructor functions
--
-- -------------------------------------------------------------------

-- | Type synonym for functions for quoting form.
type QuoteFn
  = String -- ^ File name.
  -> Int -- ^ Start line.
  -> Int -- ^ Start column.
  -> Int -- ^ End line.
  -> Int -- ^ End column.
  -> Code

-- | Make quoted symbol from 'String'.
qSymbol :: String -> QuoteFn
qSymbol = quotedWithLoc . Atom . aSymbol
{-# INLINABLE qSymbol #-}

-- | Make quoted char from 'Char'.
qChar :: Char -> QuoteFn
qChar = quotedWithLoc . Atom . AChar NoSourceText
{-# INLINABLE qChar #-}

-- | Make quoted string from 'String'.
qString :: String -> QuoteFn
qString = quotedWithLoc . Atom . aString NoSourceText
{-# INLINABLE qString #-}

-- | Make quoted integer from 'Integer'.
qInteger :: Integer -> QuoteFn
qInteger = quotedWithLoc . Atom . AInteger . mkIntegralLit
{-# INLINABLE qInteger #-}

-- | Make quoted fractional from 'Real' value.
qFractional :: (Real a, Show a) => a -> QuoteFn
qFractional = quotedWithLoc . Atom . aFractional
{-# INLINABLE qFractional #-}

-- | Make quoted unit.
qUnit :: QuoteFn
qUnit = quotedWithLoc (Atom AUnit)
{-# INLINABLE qUnit #-}

-- | Make quoted list from list of 'Code'.
qList :: [Code] -> QuoteFn
qList = quotedWithLoc . List
{-# INLINABLE qList #-}

-- | Make quoted haskell list from list of 'Code'.
qHsList :: [Code] -> QuoteFn
qHsList = quotedWithLoc . HsList
{-# INLINABLE qHsList #-}

-- -- | Make quoted symbol from 'String'.
-- | Auxiliary function to construct 'ASymbol' atom.
aSymbol :: String -> Atom
aSymbol = ASymbol . fsLit
{-# INLINABLE aSymbol #-}

-- | Auxiliary function to construct 'AString' atom.
aString :: SourceText -> String -> Atom
aString st = AString st . fsLit
{-# INLINABLE aString #-}

-- | Auxiliary function to construct an 'Atom' containing
-- 'FractionalLit' value from literal fractional numbers.
aFractional :: (Real a, Show a) => a -> Atom
aFractional x = AFractional $! mkFractionalLit' x
{-# SPECIALIZE aFractional :: Double -> Atom #-}
{-# SPECIALIZE aFractional :: Float -> Atom #-}

-- | Type fixed variant of 'aFractional'.
aDouble :: Double -> Atom
aDouble = aFractional
{-# INLINABLE aDouble #-}

aIntegral :: Integral a => a -> Atom
aIntegral x = AInteger $! mkIntegralLit x
{-# SPECIALIZE aIntegral :: Integer -> Atom #-}
{-# SPECIALIZE aIntegral :: Int -> Atom #-}

-- | A form with empty 'List'.
nil :: Code
nil = LForm (genSrc (List []))
{-# INLINABLE nil #-}

quotedWithLoc :: Form Atom -> QuoteFn
quotedWithLoc x file start_line start_col end_line end_col =
  let file_fs = fsLit file
      span_start = mkSrcLoc file_fs start_line start_col
      span_end = mkSrcLoc file_fs end_line end_col
      l = mkSrcSpan span_start span_end
  in  LForm (L l x)
{-# INLINABLE quotedWithLoc #-}

-- From ghc 9.0.1, a new field with 'Maybe Int' was added to RealSrcSpan
-- constructor of SrcLoc data type.

#if __GLASGOW_HASKELL__ >= 900
#define _MB_BUF_POS _
#else
#define _MB_BUF_POS {- empty -}
#endif

-- | Apply given functions to file name, start line, start column, end line, and
-- end column.
withLocInfo ::
    SrcSpan -- ^ Source code span to get location info.
    -> (FastString -> a) -- ^ Function applied to file name.
    -> (Int -> b) -- ^ Function applied to lines and columns.
    -> (a, b, b, b, b)
withLocInfo l f_file f_n =
  let file = f_file (fromMaybe (fsLit "<noloc>") (srcSpanFileName_maybe l))
      sl = get_n srcSpanStartLine
      sc = get_n srcSpanStartCol
      el = get_n srcSpanEndLine
      ec = get_n srcSpanEndCol
      get_n getter = case l of
        RealSrcSpan rspan _MB_BUF_POS -> f_n $! getter rspan
        _                             -> f_n 0
  in  (file, sl, sc, el, ec)
{-# INLINABLE withLocInfo #-}

-- | Return the first arg, with location information from the second arg.
asLocOf :: Code -> Code -> Code
asLocOf (LForm (L _ a)) (LForm (L l _)) = LForm (L l a)
{-# INLINABLE asLocOf #-}


-- -------------------------------------------------------------------
--
-- Auxiliary
--
-- -------------------------------------------------------------------

finkelUnhelpfulSpan :: SrcSpan
#if MIN_VERSION_ghc(9,0,0)
finkelUnhelpfulSpan =
  UnhelpfulSpan (UnhelpfulOther (fsLit "<finkel generated code>"))
#else
finkelUnhelpfulSpan =
  UnhelpfulSpan (fsLit "<finkel generated code>")
#endif
{-# INLINABLE finkelUnhelpfulSpan #-}

-- | String representation of located data.
showLoc :: LForm a -> String
showLoc (LForm (L l _)) =
  case l of
    RealSrcSpan r _MB_BUF_POS  ->
      unpackFS (srcSpanFile r) ++ ":" ++
      show (srcSpanStartLine r) ++ ":" ++
      show (srcSpanStartCol r) ++ ": "
#if MIN_VERSION_ghc(9,0,0)
    UnhelpfulSpan uh -> unpackFS (unhelpfulSpanFS uh) ++ ": "
#else
    UnhelpfulSpan fs -> unpackFS fs ++ ": "
#endif
{-# INLINABLE showLoc #-}

-- | Make 'List' from given code. When the given argument was already a 'List',
-- the given 'List' is returned. If the argument was 'HsList', converted to
-- 'List'. Otherwise, 'List' with single element.
toListL :: Code -> Code
toListL orig@(LForm (L l form)) =
  case form of
    List _    -> orig
    HsList xs -> LForm (L l (List xs))
    _         -> LForm (L l (List [orig]))
{-# INLINABLE toListL #-}

-- | Unwrap 'LForm' to 'Form'.
unCode :: LForm a -> Form a
unCode (LForm (L _ a)) = a
{-# INLINABLE unCode #-}

-- | Attach location to mark generated code.
genSrc :: a -> Located a
genSrc = L finkelUnhelpfulSpan
{-# INLINABLE genSrc #-}

-- | Make located list from list of located elements.
--
-- When the argument is not null, the resulting list has a combined location of
-- locations in the argument list elements.
mkLocatedForm :: [LForm a] -> Located [LForm a]
mkLocatedForm []        = genSrc []
mkLocatedForm ms@(hd:_) = L (combineLocs (unLForm hd) (unLForm (last ms))) ms
{-# INLINABLE mkLocatedForm #-}

-- | Lift given argument to 'LForm'.
atomForm :: a -> LForm a
atomForm = LForm . genSrc . Atom
{-# INLINABLE atomForm #-}

-- | Apply function taking single 'Form' to 'LForm'.
liftLF :: (Form a -> Form b) -> LForm a -> LForm b
liftLF f (LForm (L l a)) = LForm (L l (f a))
{-# INLINABLE liftLF #-}

-- | Apply function taking two 'Form's to 'LForm's.
liftLF2 :: (Form a -> Form b -> Form c) -> LForm a -> LForm b -> LForm c
liftLF2 f (LForm (L l1 a)) (LForm (L _l2 b)) = LForm (L l1 (f a b))
{-# INLINABLE liftLF2 #-}

-- | Apply functoni in 'LForm' to 'Form'.
apLF :: LForm (a -> b) -> Form a -> LForm b
apLF (LForm (L l f)) b = LForm (L l (f <*> b))
{-# INLINABLE apLF #-}

-- | Unary numeric operation helper.
nop1 :: (a -> Atom)
     -> (Integer -> a)
     -> (Rational -> Rational)
     -> Form Atom -> Form Atom
nop1 c f _ (Atom (AInteger il))    = Atom (c (f (il_value il)))
nop1 _ _ f (Atom (AFractional fl)) = Atom (aFractional (f (fl_value fl)))
nop1 _ _ _ _                       = List []
{-# INLINABLE nop1 #-}

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
{-# INLINABLE nop2 #-}

#if !MIN_VERSION_ghc(8,4,0)
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
