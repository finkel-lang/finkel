{-# LANGUAGE DeriveDataTypeable #-}
-- | Form and Atom data.
module Language.SK.Form
  ( -- * The S-expression form
    Atom(..)
  , Form(..)
  , LForm(..)
  , Code

  , aFractional
  , aSymbol
  , symbolName
  , symbolNameFS
  , toListL

  , unLoc
  , unLocLForm
  , genSrc
  , getLoc
  , showLoc
  , mkSkSrcSpan
  , mkLocatedForm
  , skSrcSpan
  , quoted

  -- * Reexported data from GHC
  , GenLocated(..)
  , SrcLoc(..)
  , SrcSpan(..)
  , mkSrcLoc
  , mkSrcSpan
  , fsLit
  ) where

-- From base
import Data.Data

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
  deriving (Eq, Data, Typeable)

instance Show Atom where
  show x =
    case x of
      AUnit -> "()"
      ASymbol s -> unpackFS s
      AChar c -> case c of
        '\a' -> "\\bel"
        '\b' -> "\\bs"
        '\f' -> "\\ff"
        '\n' -> "\\lf"
        '\r' -> "\\cr"
        '\t' -> "\\ht"
        '\v' -> "\\vt"
        ' '  -> "\\sp"
        _    -> ['\\',c]
      AString s -> show s
      AInteger i -> show i
      AFractional f -> fl_text f
      AComment _ -> ""

-- | Form type. Also used as token. Elements of recursive structures
-- contain location information.
data Form a
  = Atom a           -- ^ S-expression atom.
  | List [LForm a]   -- ^ S-expression list.
  | HsList [LForm a] -- ^ Haskell list.
  | TEnd             -- ^ End of token.
  deriving (Eq, Data, Typeable)

newtype LForm a = LForm {unLForm :: Located (Form a)}
  deriving (Data, Typeable)

instance Eq a => Eq (LForm a) where
  LForm (L _ a) == LForm (L _ b) = a == b

type Code = LForm Atom

instance Show a => Show (Form a) where
  show form =
    case form of
      Atom a -> show a
      List xs -> mkList "(" xs ")"
      HsList xs -> mkList "[" xs "]"
      TEnd -> "TEnd"
    where
      mkList open xs close =
        open ++ unwords (map (show . unLocLForm) xs) ++ close

instance Show a => Show (LForm a) where
  show (LForm (L _ a)) = show a

instance Functor Form where
  fmap f form =
    case form of
      Atom a -> Atom (f a)
      List xs -> List (map (fmap f) xs)
      HsList xs -> HsList (map (fmap f) xs)
      TEnd -> TEnd

instance Functor LForm where
  fmap f (LForm (L l a)) = LForm (L l (fmap f a))

instance Foldable Form where
  foldr f z form =
    case form of
      TEnd    -> z
      Atom x  -> f x z
      List xs ->
        case xs of
          []   -> z
          y:ys -> foldr f (foldr f z (unLocLForm y)) (List ys)
      HsList xs ->
        case xs of
          []   -> z
          y:ys -> foldr f (foldr f z (unLocLForm y)) (HsList ys)

instance Foldable LForm where
  foldr f z (LForm (L _ form)) = foldr f z form

aSymbol :: String -> Atom
aSymbol = ASymbol . fsLit

-- | Auxiliary function to construct an 'Atom' containing
-- 'FractionalLit' value from literal fractional numbers.
aFractional :: (Real a, Show a) => a -> Atom
aFractional x = AFractional $! FL (show x) (toRational x)

-- | String representation of located data.
showLoc :: LForm a -> String
showLoc (LForm (L l _)) = case l of
      RealSrcSpan r ->
        unpackFS (srcSpanFile r) ++ ":" ++
        show (srcSpanStartLine r) ++ ":" ++
        show (srcSpanStartCol r) ++ ": "
      UnhelpfulSpan fs -> unpackFS fs ++ ": "

-- | Extract string from given atom when the atom was 'ASymbol',
-- otherwise error.
symbolName :: Code -> String
symbolName = unpackFS . symbolNameFS

symbolNameFS :: Code -> FastString
symbolNameFS (LForm (L _ (Atom (ASymbol name)))) = name
symbolNameFS x = error ("symbolName: got " ++ show x)

toListL :: Code -> Code
toListL orig@(LForm (L l form)) =
  case form of
    List _ -> orig
    HsList xs -> LForm (L l (List xs))
    _ -> LForm (L l (List [orig]))

unLocLForm :: LForm a -> Form a
unLocLForm (LForm (L _ a)) = a
{-# INLINE unLocLForm #-}

mkSkSrcSpan :: String -> SrcSpan
mkSkSrcSpan = UnhelpfulSpan . fsLit

skSrcSpan :: SrcSpan
skSrcSpan = mkSkSrcSpan "<sk generated code>"

genSrc :: a -> Located a
genSrc = L skSrcSpan

quoted :: Form Atom -> Code
quoted = LForm . L (UnhelpfulSpan (fsLit "<quoted code>"))

mkLocatedForm :: [LForm a] -> Located [LForm a]
mkLocatedForm [] = genSrc []
mkLocatedForm ms = L (combineLocs (unLForm (head ms))
                                  (unLForm (last ms)))
                     ms
