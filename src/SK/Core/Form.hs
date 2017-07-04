{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- | Data type for form.

module SK.Core.Form
  ( Form(..)
  , Atom(..)
  , TForm(..)
  , LTForm
  , Code(..)
  , aFractional
  , splice
  , unLocForm

  , symbolNameL
  , toListL

  , nlForm
  , car
  , cdr
  , pprForm
  , pprForms
  , pprTForm
  , pprTForms
  , pForm
  , pForms
  , pAtom
  ) where

-- From base
import Data.Data
import Data.List (intersperse)

-- Pretty module from ghc.
import qualified Pretty as P

-- Internal
import SK.Core.GHC

---
--- Form data type
---

-- | Simple form type.
data Form a
  = Atom a
  | List [Form a]
  | HsList [Form a]
  deriving (Eq, Data, Typeable)

instance Show a => Show (Form a) where
  show form =
    case form of
      Atom a -> show a
      List xs -> mkList "(" xs ")"
      HsList xs -> mkList "[" xs "]"
    where
      mkList open xs close =
        open ++ concat (intersperse " " (map show xs)) ++ close

-- | Atom in tokens.
data Atom
  = AUnit
  | ASymbol String
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
      ASymbol s -> s
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

-- | Token form. Contains location information.
data TForm a
  = TAtom a            -- ^ S-expression atom.
  | TList [LTForm a]   -- ^ S-expression list.
  | THsList [LTForm a] -- ^ Haskell list.
  | TEnd               -- ^ End of token.
  deriving (Eq, Data, Typeable)

instance Show a => Show (TForm a) where
  show (TAtom a) = "TAtom " ++ show a
  show (TList as) = "TList " ++ show (map unLoc as)
  show (THsList as) = "THsList " ++ show (map unLoc as)
  show TEnd = "TEnd"

type LTForm a = Located (TForm a)

-- | Auxiliary function to construct an 'Atom' containing
-- 'FractionalLit' value from literal fractional numbers.
aFractional :: (Real a, Show a) => a -> Atom
aFractional x = AFractional $! FL (show x) (toRational x)

-- | Converts located token form to bare 'Form'. Location information,
-- token end constructor, and Haskell list constructor disappears.
unLocForm :: LTForm a -> Form a
unLocForm form =
   case unLoc form of
     TAtom a    -> Atom a
     TList xs   -> List (map unLocForm xs)
     THsList xs -> HsList (map unLocForm xs)
     TEnd       -> List []

-- | Make a token form with no location information.
nlForm :: Form a -> LTForm a
nlForm form =
  case form of
    Atom x -> noLoc (TAtom x)
    List xs -> noLoc (TList (map nlForm xs))
    HsList xs -> noLoc (THsList (map nlForm xs))

car :: Form a -> Form a
car (List (x:_)) = x
car _ = List []

cdr :: Form a -> Form a
cdr (List (_:xs)) = List xs
cdr _ = List []

-- | Extract string from given atom when the atom was 'ASymbol',
-- otherwise error.
symbolNameL :: LTForm Atom -> String
symbolNameL (L _ (TAtom (ASymbol name))) = name
symbolNameL x = error ("symbolNameL: got " ++ show (pprTForm x))

toListL :: LTForm Atom -> LTForm Atom
toListL orig@(L l form) =
  case form of
    TList _ -> orig
    THsList xs -> L l (TList xs)
    _ -> L l (TList [orig])

pprForm :: Form Atom -> P.Doc
pprForm form =
  case form of
    Atom x -> P.text "Atom" P.<+> P.parens (pprAtom x)
    List xs -> P.text "List" P.<+> P.nest 2 (pprForms xs)
    HsList xs -> P.text "HsList" P.<+> P.nest 2 (pprForms xs)

pprForms :: [Form Atom] -> P.Doc
pprForms forms =
  P.brackets (P.sep (P.punctuate P.comma (map pprForm forms)))

pprAtom :: Atom -> P.Doc
pprAtom atom =
  case atom of
    AUnit     -> P.text "AUnit"
    ASymbol x -> P.text "ASymbol" P.<+> P.text x
    AChar x -> P.text "AChar" P.<+> P.char x
    AString x -> P.text "AString" P.<+> P.doubleQuotes (P.text x)
    AInteger x -> P.text "AInteger" P.<+> (P.text (show x))
    AFractional x -> P.text "AFractional" P.<+> (P.text (fl_text x))
    AComment x -> P.text "AComment" P.<+> (P.doubleQuotes (P.text x))

pprTForm :: LTForm Atom -> P.Doc
pprTForm (L _ form) =
  case form of
    TAtom x -> P.text "TAtom" P.<+> P.parens (pprAtom x)
    TList xs -> P.text "TList" P.<+> P.nest 2 (pprTForms xs)
    THsList xs -> P.text "THsList" P.<+> P.nest 2 (pprTForms xs)
    TEnd -> P.text "TEnd"

pprTForms :: [LTForm Atom] -> P.Doc
pprTForms forms =
  P.brackets (P.sep (P.punctuate P.comma (map pprTForm forms)))

pForm :: Form Atom -> P.Doc
pForm form =
  case form of
    Atom a -> pAtom a
    List forms -> pForms P.parens forms
    HsList forms -> pForms P.brackets forms

pForms :: (P.Doc -> P.Doc) -> [Form Atom] -> P.Doc
pForms f forms =
  case forms of
    [] -> P.empty
    _  -> f (P.sep (map pForm forms))

pAtom :: Atom -> P.Doc
pAtom atom =
  case atom of
    ASymbol x -> P.text x
    AChar x -> P.char '\\' P.<+> P.char x
    AString x -> P.doubleQuotes (P.text x)
    AInteger x -> P.text (show x)
    AFractional x -> P.text (fl_text x)
    AUnit -> P.text "()"
    AComment _ -> P.empty

--- -------------------
--- Code type class

--- Instance data types of Formable class could be inserted to
--- S-expression form with `unquote' and `unquote-splice'.

class Code a where
  toForm :: a -> Form Atom
  fromForm :: Form Atom -> Maybe a
  fromForm _ = Nothing

instance Code Atom where
  toForm a = Atom a
  fromForm a =
    case a of
      Atom x -> Just x
      _      -> Nothing

instance Code () where
  toForm _ = Atom AUnit
  fromForm a =
    case a of
      Atom AUnit -> Just ()
      _          -> Nothing

instance Code Char where
  toForm = Atom . AChar
  fromForm a =
    case a of
      Atom (AChar x) -> Just x
      _              -> Nothing

instance Code String where
  toForm a = Atom (AString a)
  fromForm a =
    case a of
      Atom (AString s) -> Just s
      _                -> Nothing

instance Code Int where
  toForm a = Atom (AInteger (fromIntegral a))
  fromForm a =
    case a of
      Atom (AInteger n) -> Just (fromIntegral n)
      _                 -> Nothing

instance Code Integer where
  toForm a = Atom (AInteger a)
  fromForm a =
    case a of
      Atom (AInteger n) -> Just n
      _                 -> Nothing

instance Code Double where
  toForm a = let r = toRational a in Atom (AFractional (FL (show a) r))
  fromForm a =
    case a of
      Atom (AFractional x) -> Just (fromRational (fl_value x))
      _                    -> Nothing

instance Code a => Code (Form a) where
  toForm form =
    case form of
      Atom a  -> toForm a
      List as -> List (map toForm as)
      HsList as -> HsList (map toForm as)
  fromForm a =
    case a of
      Atom _  -> fromForm a
      List as -> List <$> mapM fromForm as
      HsList as -> HsList <$> mapM fromForm as

splice :: Code a => a -> [Form Atom]
splice form =
  case toForm form of
    List xs -> xs
    _       -> []
