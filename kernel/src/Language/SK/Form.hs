{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
-- | Form and Atom data.
module Language.SK.Form
  ( -- * The S-expression form
    Atom(..)
  , Form(..)
  , LForm(..)
  , Code

  , aFractional
  , aSymbol
  , nil
  , symbolName
  , symbolNameFS
  , toListL

  , unLoc
  , unCode
  , genSrc
  , getLoc
  , showLoc
  , mkSkSrcSpan
  , mkLocatedForm
  , skSrcSpan
  , quoted

  , haskellOpChars

  -- * Reexported data from GHC
  , GenLocated(..)
  , SrcLoc(..)
  , SrcSpan(..)
  , mkFractionalLit
  , mkSrcLoc
  , mkSrcSpan
  , fsLit
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
import SrcLoc ( GenLocated(..), Located, SrcLoc(..)
              , SrcSpan(..), combineLocs, getLoc, mkSrcLoc, mkSrcSpan
              , srcSpanFile, srcSpanStartCol, srcSpanStartLine
              , unLoc )

-- deepseq
import Control.DeepSeq (NFData(..))


-- -------------------------------------------------------------------
--
-- Form data type
--
-- -------------------------------------------------------------------

-- | Atom in tokens.
data Atom
  = AUnit
  | ASymbol {-# UNPACK #-} !FastString
  | AChar {-# UNPACK #-} !Char
  | AString String
  | AInteger Integer
  | AFractional FractionalLit
  | AComment String
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
      AComment _ -> showString ""

instance NFData Atom where
  rnf x =
    case x of
      AUnit         -> ()
      ASymbol fs    -> seq fs ()
      AChar c       -> seq c ()
      AString str   -> rnf str
      AInteger i    -> rnf i
      AFractional y -> seq y ()
      AComment str  -> rnf str

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

-- | Type synonym for code data.
type Code = LForm Atom

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

toListL :: Code -> Code
toListL orig@(LForm (L l form)) =
  case form of
    List _ -> orig
    HsList xs -> LForm (L l (List xs))
    _ -> LForm (L l (List [orig]))

unCode :: LForm a -> Form a
unCode (LForm (L _ a)) = a
{-# INLINE unCode #-}

mkSkSrcSpan :: String -> SrcSpan
mkSkSrcSpan = UnhelpfulSpan . fsLit
{-# INLINE mkSkSrcSpan #-}

skSrcSpan :: SrcSpan
skSrcSpan = mkSkSrcSpan "<sk generated code>"
{-# INLINE skSrcSpan #-}

genSrc :: a -> Located a
genSrc = L skSrcSpan
{-# INLINE genSrc #-}

quoted :: Form Atom -> Code
quoted = LForm . L (UnhelpfulSpan (fsLit "<quoted code>"))
{-# INLINE quoted #-}

mkLocatedForm :: [LForm a] -> Located [LForm a]
mkLocatedForm [] = genSrc []
mkLocatedForm ms = L (combineLocs (unLForm (head ms))
                                  (unLForm (last ms)))
                     ms

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
