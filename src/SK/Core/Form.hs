{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Form and Atom data.
module SK.Core.Form
  ( -- * The S-expression form
    Form(..)
  , Atom(..)
  , TForm(..)
  , LTForm
  , Code
  , LCode

  , aFractional
  , unLoc
  , unLocForm
  , getLoc
  , showLoc
  , nlForm
  , locateForm
  , symbolNameL
  , toListL

  , pprForm
  , pprForms
  , pprTForm
  , pprTForms
  , pForm
  , pForms
  , pAtom

  , Codish(..)
  , unquoteSplice

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

-- Pretty module from ghc.
import qualified Pretty as P

-- Internal
import SK.Core.GHC


-- -------------------------------------------------------------------
--
-- Form data type
--
-- -------------------------------------------------------------------

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
        open ++ unwords (map show xs) ++ close

instance Functor Form where
  fmap f form =
    case form of
      Atom a -> Atom (f a)
      List xs -> List (map (fmap f) xs)
      HsList xs -> HsList (map (fmap f) xs)

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

-- | Located form type, used as token.
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

type Code = Form Atom

type LCode = LTForm Atom

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

-- | Attach given location to form, including all sub forms.
locateForm :: SrcSpan -> Form a -> LTForm a
locateForm l form =
  case form of
    Atom x -> L l (TAtom x)
    List xs -> L l (TList (map (locateForm l) xs))
    HsList xs -> L l (THsList (map (locateForm l) xs))

-- | String representation of located data.
showLoc :: Located a -> String
showLoc x = case getLoc x of
      RealSrcSpan r ->
        unpackFS (srcSpanFile r) ++ ":" ++
        show (srcSpanStartLine r) ++ ":" ++
        show (srcSpanStartCol r) ++ ": "
      UnhelpfulSpan _ -> "unknown location: "

-- | Extract string from given atom when the atom was 'ASymbol',
-- otherwise error.
symbolNameL :: LCode -> String
symbolNameL (L _ (TAtom (ASymbol name))) = name
symbolNameL x = error ("symbolNameL: got " ++ show (pprTForm x))

toListL :: LCode -> LCode
toListL orig@(L l form) =
  case form of
    TList _ -> orig
    THsList xs -> L l (TList xs)
    _ -> L l (TList [orig])

pprForm :: Code -> P.Doc
pprForm form =
  case form of
    Atom x -> P.text "Atom" P.<+> P.parens (pprAtom x)
    List xs -> P.text "List" P.<+> P.nest 2 (pprForms xs)
    HsList xs -> P.text "HsList" P.<+> P.nest 2 (pprForms xs)

pprForms :: [Code] -> P.Doc
pprForms forms =
  P.brackets (P.sep (P.punctuate P.comma (map pprForm forms)))

pprAtom :: Atom -> P.Doc
pprAtom atom =
  case atom of
    AUnit     -> P.text "AUnit"
    ASymbol x -> P.text "ASymbol" P.<+> P.text x
    AChar x -> P.text "AChar" P.<+> P.char x
    AString x -> P.text "AString" P.<+> P.doubleQuotes (P.text x)
    AInteger x -> P.text "AInteger" P.<+> P.text (show x)
    AFractional x -> P.text "AFractional" P.<+> P.text (fl_text x)
    AComment x -> P.text "AComment" P.<+> P.doubleQuotes (P.text x)

pprTForm :: LCode -> P.Doc
pprTForm (L _ form) =
  case form of
    TAtom x -> P.text "TAtom" P.<+> P.parens (pprAtom x)
    TList xs -> P.text "TList" P.<+> P.nest 2 (pprTForms xs)
    THsList xs -> P.text "THsList" P.<+> P.nest 2 (pprTForms xs)
    TEnd -> P.text "TEnd"

pprTForms :: [LCode] -> P.Doc
pprTForms forms =
  P.brackets (P.sep (P.punctuate P.comma (map pprTForm forms)))

pForm :: Code -> P.Doc
pForm form =
  case form of
    Atom a -> pAtom a
    List forms -> pForms P.parens forms
    HsList forms -> pForms P.brackets forms

pForms :: (P.Doc -> P.Doc) -> [Code] -> P.Doc
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


-- -------------------------------------------------------------------
--
-- Codish type class
--
-- -------------------------------------------------------------------

--- Instance data types of Formable class could be inserted to
--- S-expression form with `unquote' and `unquote-splice'.

class Codish a where
  toCode :: a -> Code

  fromCode :: Code -> Maybe a
  fromCode _ = Nothing

  listToCode :: [a] -> Code
  listToCode = HsList . map toCode

  listFromCode :: Code -> Maybe [a]
  listFromCode xs = case xs of
                      HsList as -> mapM fromCode as
                      _         -> Nothing

instance Codish Atom where
  toCode = Atom
  fromCode a =
    case a of
      Atom x -> Just x
      _      -> Nothing

instance Codish () where
  toCode _ = Atom AUnit
  fromCode a =
    case a of
      Atom AUnit -> Just ()
      _          -> Nothing

instance Codish Char where
  toCode = Atom . AChar
  fromCode a =
    case a of
      Atom (AChar x) -> Just x
      _              -> Nothing
  listToCode = Atom . AString
  listFromCode a = case a of
                     Atom (AString s) -> Just s
                     _ -> Nothing

instance Codish Int where
  toCode a = Atom (AInteger (fromIntegral a))
  fromCode a =
    case a of
      Atom (AInteger n) -> Just (fromIntegral n)
      _                 -> Nothing

instance Codish Integer where
  toCode a = Atom (AInteger a)
  fromCode a =
    case a of
      Atom (AInteger n) -> Just n
      _                 -> Nothing

instance Codish Double where
  toCode a = let r = toRational a in Atom (AFractional (FL (show a) r))
  fromCode a =
    case a of
      Atom (AFractional x) -> Just (fromRational (fl_value x))
      _                    -> Nothing

instance Codish a => Codish [a] where
  toCode = listToCode
  fromCode = listFromCode

instance Codish a => Codish (Form a) where
  toCode form =
    case form of
      Atom a  -> toCode a
      List as -> List (map toCode as)
      HsList as -> HsList (map toCode as)
  fromCode a =
    case a of
      Atom _  -> fromCode a
      List as -> List <$> mapM fromCode as
      HsList as -> HsList <$> mapM fromCode as

-- `FlexibleInstance' language pragma required for below.
instance Codish a => Codish (LTForm a) where
  toCode = toCode . unLocForm
  fromCode = fmap nlForm . fromCode

-- instance Codish SrcSpan where
--   toCode sp =
--     case sp of
--       UnhelpfulSpan txt ->
--         List [Atom (ASymbol "UnhelpfulSpan")
--              ,Atom (AString (unpackFS txt))]
--       RealSrcSpan rs ->
--         List [Atom (ASymbol "mkSrcSpan")
--              ,List [Atom (ASymbol "mkSrcLoc")
--                    ,List [Atom (ASymbol "fsLit")
--                          ,Atom (AString fn)]
--                    ,aint srcSpanStartLine
--                    ,aint srcSpanStartCol]
--              ,List [Atom (ASymbol "mkSrcLoc")
--                    ,List [Atom (ASymbol "fsLit")
--                          ,Atom (AString fn)]
--                    ,aint srcSpanEndLine
--                    ,aint srcSpanEndCol]]
--           where
--             fn = case srcSpanFileName_maybe sp of
--                Just fs -> unpackFS fs
--                Nothing -> "unknown file"
--             aint f = Atom (AInteger (fromIntegral (f rs)))
--   fromCode form =
--     case form of
--       List [Atom (ASymbol "UnhelpfulSpan"), Atom (AString txt)]
--        -> Just (mkGeneralSrcSpan (fsLit txt))
--       List [Atom (ASymbol "mkSrcSpan")
--            ,List [Atom (ASymbol "mkSrcLoc")
--                  ,List [Atom (ASymbol "fsLit")
--                        ,Atom (AString fn)]
--                  ,Atom (AInteger sl)
--                  ,Atom (AInteger sc)]
--            ,List [Atom (ASymbol "mkSrcLoc")
--                  ,List [Atom (ASymbol "fsLit")
--                        ,Atom (AString _)]
--                  ,Atom (AInteger el)
--                  ,Atom (AInteger ec)]]
--        -> Just (mkSrcSpan loc1 loc2)
--          where
--            loc1 = mkSrcLoc fn' (fromIntegral sl) (fromIntegral sc)
--            loc2 = mkSrcLoc fn' (fromIntegral el) (fromIntegral ec)
--            fn' = fsLit fn
--       _ -> Nothing

unquoteSplice :: Codish a => a -> [Code]
unquoteSplice form =
  case toCode form of
    List xs -> xs
    HsList xs -> xs
    _       -> []
