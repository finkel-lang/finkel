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
  , mkSrcLoc
  , mkSrcSpan
  , fsLit
  ) where

-- base
import Data.Data (Data, Typeable)
import GHC.Generics (Generic)

-- deepseq
import Control.DeepSeq (NFData(..))

-- QuickCheck
import Test.QuickCheck ( Arbitrary(..), CoArbitrary(..), Gen
                       , arbitraryUnicodeChar, elements
                       , getUnicodeString, listOf
                       , oneof, scale, variant )

-- Internal
import Language.SK.GHC

-- -------------------------------------------------------------------
--
-- Form data type
--
-- -------------------------------------------------------------------

-- | Atom in tokens.
data Atom
  = AUnit
  | ASymbol FastString
  | AChar Char
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
      AFractional f -> showString (fl_text f)
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

instance Arbitrary Atom where
   -- XXX: Unicode symbols are not generated yet.
  arbitrary =
    oneof [ return AUnit
          , aSymbol <$> symbolG
          , AChar <$> arbitraryUnicodeChar
          , AString <$> stringG
          , AInteger <$> arbitrary
          , aFractional <$> (arbitrary :: Gen Double) ]
    where
      headChars = ['A' .. 'Z'] ++ ['a' .. 'z'] ++ haskellOpChars'
      haskellOpChars' = '_' : filter (`notElem` "#-|") haskellOpChars
      tailChars = headChars ++ "0123456789'-"
      symbolG = do
        x <- elements headChars
        xs <- listOf (elements tailChars)
        return (x:xs)
      stringG = getUnicodeString <$> arbitrary

instance CoArbitrary Atom where
  coarbitrary x =
    case x of
      AUnit         -> var 0
      ASymbol sym   -> var 1 . coarbitrary (unpackFS sym)
      AChar c       -> var 2 . coarbitrary c
      AString str   -> var 3 . coarbitrary str
      AInteger i    -> var 4 . coarbitrary i
      AFractional d -> var 5 . coarbitrary (fl_value d)
      AComment str  -> var 6 . coarbitrary str
    where
      var :: Int -> Gen a -> Gen a
      var = variant

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

instance Traversable Form where
  traverse f form =
    case form of
      Atom x    -> fmap Atom (f x)
      List xs   -> fmap List (traverse (traverse f) xs)
      HsList xs -> fmap HsList (traverse (traverse f) xs)
      TEnd      -> pure TEnd

instance NFData a => NFData (Form a) where
  rnf x =
    case x of
      Atom a    -> rnf a
      List as   -> rnf as
      HsList as -> rnf as
      TEnd      -> ()

instance Arbitrary a => Arbitrary (Form a) where
  arbitrary =
    oneof [Atom <$> arbitrary
          ,List <$> listOf (scale (`div` 3) arbitrary)
          ,HsList <$> listOf (scale (`div` 3) arbitrary)]
  shrink x =
    case x of
      Atom _    -> []
      List xs   -> map unCode xs ++ [List xs'|xs' <- shrink xs]
      HsList xs -> map unCode xs ++ [HsList xs'|xs' <- shrink xs]
      TEnd      -> []

instance CoArbitrary a => CoArbitrary (Form a) where
  coarbitrary x =
    case x of
      Atom y    -> var 0 . coarbitrary y
      List ys   -> var 1 . coarbitrary ys
      HsList ys -> var 2 . coarbitrary ys
      TEnd      -> var 3
    where
      var :: Int -> Gen a -> Gen a
      var = variant

-- | Newtype wrapper for located 'Form'.
newtype LForm a = LForm {unLForm :: Located (Form a)}
  deriving (Data, Typeable, Generic)

instance Eq a => Eq (LForm a) where
  LForm (L _ a) == LForm (L _ b) = a == b

instance Show a => Show (LForm a) where
  showsPrec d (LForm (L _ a)) = showsPrec d a

instance Functor LForm where
  fmap f (LForm (L l a)) = LForm (L l (fmap f a))

instance Foldable LForm where
  foldr f z (LForm (L _ form)) = foldr f z form

instance Traversable LForm where
  traverse f (LForm (L l form)) =
    fmap (LForm . L l) (traverse f form)

instance NFData a => NFData (LForm a) where
  rnf (LForm (L l a)) = rnf l `seq` rnf a

instance Arbitrary a => Arbitrary (LForm a) where
  arbitrary = (LForm . L skSrcSpan) <$> arbitrary

instance CoArbitrary a => CoArbitrary (LForm a) where
  coarbitrary (LForm (L _ form)) = coarbitrary form

-- | Type synonym for code data.
type Code = LForm Atom

-- | Auxiliary function to construct 'ASymbol' atom.
aSymbol :: String -> Atom
aSymbol = ASymbol . fsLit

-- | Auxiliary function to construct an 'Atom' containing
-- 'FractionalLit' value from literal fractional numbers.
aFractional :: (Real a, Show a) => a -> Atom
aFractional x = AFractional $! FL (show x) (toRational x)
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
