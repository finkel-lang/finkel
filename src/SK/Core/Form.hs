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
  , mkSkSrcSpan
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

instance Show LCode where
  show = show . unLoc

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
      UnhelpfulSpan fs -> unpackFS fs ++ ": "

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
  toCode :: a -> LCode

  fromCode :: LCode -> Maybe a
  fromCode _ = Nothing

  listToCode :: [a] -> LCode
  listToCode xs =
     let xs' = map toCode xs
         l = getLoc (mkLocatedList xs')
     in  L l (THsList xs')

  listFromCode :: LCode -> Maybe [a]
  listFromCode xs = case unLoc xs of
                      THsList as -> mapM fromCode as
                      _          -> Nothing

instance Codish Atom where
  toCode = genSrc . TAtom
  fromCode a =
    case unLoc a of
      TAtom x -> Just x
      _       -> Nothing

instance Codish () where
  toCode _ = genSrc (TAtom AUnit)
  fromCode a =
    case unLoc a of
      TAtom AUnit -> Just ()
      _           -> Nothing

instance Codish Char where
  toCode = genSrc . TAtom . AChar
  fromCode a =
    case unLoc a of
      TAtom (AChar x) -> Just x
      _               -> Nothing
  listToCode = genSrc . TAtom . AString
  listFromCode a = case unLoc a of
                     TAtom (AString s) -> Just s
                     _ -> Nothing

instance Codish Int where
  toCode = genSrc . TAtom . AInteger . fromIntegral
  fromCode a =
    case unLoc a of
      TAtom (AInteger n) -> Just (fromIntegral n)
      _                  -> Nothing

instance Codish Integer where
  toCode = genSrc . TAtom . AInteger
  fromCode a =
    case unLoc a of
      TAtom (AInteger n) -> Just n
      _                 -> Nothing

instance Codish Double where
  toCode a =
    let r = toRational a
    in  genSrc (TAtom (AFractional (FL (show a) r)))
  fromCode a =
    case unLoc a of
      TAtom (AFractional x) -> Just (fromRational (fl_value x))
      _                     -> Nothing

instance Codish a => Codish [a] where
  toCode = listToCode
  fromCode = listFromCode

instance Codish a => Codish (Form a) where
  toCode form =
    case form of
      Atom a  -> toCode a
      List as ->
        let as' = map toCode as
            l = getLoc (mkLocatedList as')
        in  L l (TList as')
      HsList as ->
        let as' = map toCode as
            l = getLoc (mkLocatedList as')
        in  L l (THsList (map toCode as))
  fromCode form@(L _ a) =
    case a of
      TAtom _    -> fromCode form
      TList as   -> List <$> mapM fromCode as
      THsList as -> HsList <$> mapM fromCode as
      TEnd       -> Just (List [])

-- `FlexibleInstance' language pragma required for below.
instance Codish a => Codish (LTForm a) where
  toCode (L l form) =
    case form of
      TAtom a    -> let (L _ b) = toCode a in L l b
      TList xs   -> L l (TList (map toCode xs))
      THsList xs -> L l (THsList (map toCode xs))
      TEnd       -> L l TEnd
  fromCode form@(L _ x) =
    case x of
      TAtom _  -> fromCode form
      _        -> error ("fromCode: LTForm")

instance Codish SrcSpan where
  toCode sp =
    case sp of
      UnhelpfulSpan txt ->
        (list [(atom (ASymbol "mkSkSrcSpan"))
              ,(atom (AString (unpackFS txt)))])
      RealSrcSpan rs ->
        list [atom (ASymbol "mkSrcSpan")
             ,list [atom (ASymbol "mkSrcLoc")
                   ,list [atom (ASymbol "fsLit")
                         ,atom (AString fn)]
                   ,aint srcSpanStartLine
                   ,aint srcSpanStartCol]
             ,list [atom (ASymbol "mkSrcLoc")
                   ,list [atom (ASymbol "fsLit")
                         ,atom (AString fn)]
                   ,aint srcSpanEndLine
                   ,aint srcSpanEndCol]]
        where
          fn = case srcSpanFileName_maybe sp of
             Just fs -> unpackFS fs
             Nothing -> "unknown file"
          aint f = atom (AInteger (fromIntegral (f rs)))
    where
      list = genSrc . TList
      atom = genSrc . TAtom
  fromCode form =
    case unLoc form of
      TList [L _ (TAtom (ASymbol "mkSkSrcSpan"))
            ,L _ (TAtom (AString txt))]
       -> Just (mkSkSrcSpan txt)
      TList [L _ (TAtom (ASymbol "mkSrcSpan"))
            ,L _ (TList [L _ (TAtom (ASymbol "mkSrcLoc"))
                        ,L _ (TList [L _ (TAtom (ASymbol "fsLit"))
                                    ,L _ (TAtom (AString fn))])
                        ,L _ (TAtom (AInteger sl))
                        ,L _ (TAtom (AInteger sc))])
            ,L _ (TList [L _ (TAtom (ASymbol "mkSrcLoc"))
                        ,L _ (TList [L _ (TAtom (ASymbol "fsLit"))
                                    ,L _ (TAtom (AString _))])
                        ,L _ (TAtom (AInteger el))
                        ,L _ (TAtom (AInteger ec))])]
       -> Just (mkSrcSpan loc1 loc2)
         where
           loc1 = mkSrcLoc fn' (fromIntegral sl) (fromIntegral sc)
           loc2 = mkSrcLoc fn' (fromIntegral el) (fromIntegral ec)
           fn' = fsLit fn
      _ -> Nothing

unquoteSplice :: Codish a => a -> [LCode]
unquoteSplice form =
  case unLoc (toCode form) of
    TList xs   -> xs
    THsList xs -> xs
    _          -> []

mkSkSrcSpan :: String -> SrcSpan
mkSkSrcSpan = UnhelpfulSpan . fsLit

skSrcSpan :: SrcSpan
skSrcSpan = mkSkSrcSpan "<sk generated code>"

genSrc :: a -> Located a
genSrc = L skSrcSpan

mkLocatedList ::  [Located a] -> Located [Located a]
mkLocatedList [] = genSrc []
mkLocatedList ms = L (combineLocs (head ms) (last ms)) ms
