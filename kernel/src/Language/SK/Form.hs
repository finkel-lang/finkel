{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
-- | S-expression form data for source code.
module Language.SK.Form
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
  , aSymbol
  , genSrc
  , haskellOpChars
  , mkLocatedForm
  , showLoc
  , symbolName
  , symbolNameFS
  , toListL
  , unCode
  ) where

-- base
import Data.Data (Data, Typeable)
import GHC.Generics (Generic)

-- ghc
import BasicTypes ( FractionalLit (..)
#if MIN_VERSION_ghc(8,4,0)
                  , SourceText(..)
                  , mkFractionalLit
#endif
                  )
import FastString (FastString, fsLit, unpackFS)
import SrcLoc ( GenLocated(..), Located, SrcSpan(..), combineLocs
              , srcSpanFile, srcSpanStartCol, srcSpanStartLine )

-- deepseq
import Control.DeepSeq (NFData(..))


-- -------------------------------------------------------------------
--
-- Form data type
--
-- -------------------------------------------------------------------

-- | Type synonym for code data.
--
-- The 'Code' data is the fundamental data type used in the entire
-- compilation work.  The 'Code' is used to represed data from parsed
-- source file, and used for input and output of macros transformer
-- functions. List of 'Code' data are converted to Haskell AST via
-- syntax parser.
--
-- Since 'Code' is returned from parsed source file, source code
-- location information is attached to 'Code'.
--
type Code = LForm Atom

-- | Atom in tokens.
data Atom
  = AUnit
  | ASymbol {-# UNPACK #-} !FastString
  | AChar {-# UNPACK #-} !Char
  | AString String
  | AInteger Integer
  | AFractional FractionalLit
  deriving (Eq, Data, Typeable, Generic)

instance Show Atom where
  showsPrec d x =
    case x of
      AUnit -> showString "()"
      ASymbol s -> showString (unpackFS s)
      AChar c -> showString $ case c of
        '\a' -> "\\\\BEL"
        '\b' -> "\\\\BS"
        '\f' -> "\\\\FF"
        '\n' -> "\\\\LF"
        '\r' -> "\\\\CR"
        '\t' -> "\\\\HT"
        '\v' -> "\\\\VT"
        ' '  -> "\\\\SP"
        _    -> ['\\', c]
      AString s -> showsPrec d s
      AInteger i -> showsPrec d i
      AFractional f -> showString (fl_text_compat f)

instance NFData Atom where
  rnf x =
    case x of
      AUnit         -> ()
      ASymbol fs    -> seq fs ()
      AChar c       -> seq c ()
      AString str   -> rnf str
      AInteger i    -> rnf i
      AFractional y -> seq y ()

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
      Atom a -> Atom (f a)
      List xs -> List (map (fmap f) xs)
      HsList xs -> HsList (map (fmap f) xs)
      TEnd -> TEnd
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
  traverse f (LForm (L l form)) =
    fmap (LForm . L l) (traverse f form)
  {-# INLINE traverse #-}

instance NFData a => NFData (LForm a) where
  rnf (LForm (L l a)) = rnf l `seq` rnf a

-- | Make quoted symbol from 'String'.
qSymbol :: String -> Code
qSymbol = quoted . Atom . aSymbol

-- | Make quoted char from 'Char'.
qChar :: Char -> Code
qChar = quoted . Atom . AChar

-- | Make quoted string from 'String'.
qString :: String -> Code
qString = quoted . Atom . AString

-- | Make quoted integer from 'Integer'.
qInteger :: Integer -> Code
qInteger = quoted . Atom . AInteger

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

-- | Auxiliary function to construct an 'Atom' containing
-- 'FractionalLit' value from literal fractional numbers.
aFractional :: (Real a, Show a) => a -> Atom
aFractional x = AFractional $! mkFractionalLit x
{-# SPECIALIZE aFractional :: Double -> Atom #-}
{-# SPECIALIZE aFractional :: Float -> Atom #-}

-- | A form with empty 'List'.
nil :: Code
nil = LForm (genSrc (List []))

-- | String representation of located data.
showLoc :: LForm a -> String
showLoc (LForm (L l _)) =
  case l of
    RealSrcSpan r    ->
      unpackFS (srcSpanFile r) ++ ":" ++
      show (srcSpanStartLine r) ++ ":" ++
      show (srcSpanStartCol r) ++ ": "
    UnhelpfulSpan fs -> unpackFS fs ++ ": "

-- | Extract string from given atom when the atom was 'ASymbol',
-- otherwise error.
symbolName :: Code -> String
symbolName = unpackFS . symbolNameFS

-- | Like 'symbolName', but returns 'FastString'.
symbolNameFS :: Code -> FastString
symbolNameFS (LForm (L _ (Atom (ASymbol name)))) = name
symbolNameFS x = error ("symbolName: got " ++ show x)

-- | Make 'List' from given code. When the given argument was already a
-- 'List', the given 'List' is returned. If the argument was 'HsList',
-- converted to 'List'. Otherwise, 'List' with single element.
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
genSrc = L skSrcSpan
{-# INLINE genSrc #-}

-- | Source code span for generated code.
skSrcSpan :: SrcSpan
skSrcSpan = UnhelpfulSpan (fsLit "<sk generated code>")
{-# INLINE skSrcSpan #-}

-- | Make located list from list of located elements.
--
-- When the argument is not null, the resulting list has a combined
-- location of locations in the argument list elements.
mkLocatedForm :: [LForm a] -> Located [LForm a]
mkLocatedForm [] = genSrc []
mkLocatedForm ms = L (combineLocs (unLForm (head ms))
                                  (unLForm (last ms)))
                     ms

quoted :: Form Atom -> Code
quoted = LForm . L (UnhelpfulSpan (fsLit "<quoted code>"))
{-# INLINE quoted #-}

-- -------------------------------------------------------------------
--
-- Auxiliary
--
-- -------------------------------------------------------------------

-- | Characters used in Haskell operators.
haskellOpChars :: [Char]
haskellOpChars = "!#$%&*+./<=>?@^|-~:"

fl_text_compat :: FractionalLit -> String
fl_text_compat fl =
#if MIN_VERSION_ghc(8,4,0)
  case fl_text fl of
    NoSourceText -> error "fractional literal with no source"
    SourceText s -> s
#else
  fl_text fl
#endif

#if !MIN_VERSION_ghc(8,4,0)
-- | 'mkFractionalLit' did not exist in 8.2.x.
mkFractionalLit :: Real a => a -> FractionalLit
mkFractionalLit x = FL (show (realToFrac x :: Double)) (toRational x)
#endif
