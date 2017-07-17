{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Form and Atom data.
module SK.Core.Form
  ( -- * The S-expression form
    Atom(..)
  , Form(..)
  , Code

  , aFractional
  , symbolNameL
  , toListL

  , unLoc
  , getLoc
  , showLoc
  , mkSkSrcSpan
  , skSrcSpan
  , quoted

  , pprForm
  , pprForms
  , pForm
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

-- | Form type. Also used as token. Elements of recursive structures
-- contain location information.
data Form a
  = Atom a                    -- ^ S-expression atom.
  | List [Located (Form a)]   -- ^ S-expression list.
  | HsList [Located (Form a)] -- ^ Haskell list.
  | TEnd                      -- ^ End of token.
  deriving (Eq, Data, Typeable)

type Code = Located (Form Atom)

instance Show a => Show (Form a) where
  show form =
    case form of
      Atom a -> show a
      List xs -> mkList "(" xs ")"
      HsList xs -> mkList "[" xs "]"
      TEnd -> "TEnd"
    where
      mkList open xs close =
        open ++ unwords (map (show . unLoc) xs) ++ close

instance Functor Form where
  fmap f form =
    case form of
      Atom a -> Atom (f a)
      List xs -> List (map (fmap (fmap f)) xs)
      HsList xs -> HsList (map (fmap (fmap f)) xs)
      TEnd -> TEnd

instance Foldable Form where
  -- XXX: Quite inefficient.
  foldr f z form =
    case form of
      TEnd -> z
      Atom x -> f x z
      List xs ->
        case xs of
          [] -> z
          y:ys -> foldr f (foldr f z (unLoc y)) (List ys)
      HsList xs ->
        case xs of
          [] -> z
          y:ys -> foldr f (foldr f z (unLoc y)) (HsList ys)

instance Show Code where
  show = show . unLoc

-- | Auxiliary function to construct an 'Atom' containing
-- 'FractionalLit' value from literal fractional numbers.
aFractional :: (Real a, Show a) => a -> Atom
aFractional x = AFractional $! FL (show x) (toRational x)

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
symbolNameL :: Code -> String
symbolNameL (L _ (Atom (ASymbol name))) = name
symbolNameL x = error ("symbolNameL: got " ++ show (pprForm x))

toListL :: Code -> Code
toListL orig@(L l form) =
  case form of
    List _ -> orig
    HsList xs -> L l (List xs)
    _ -> L l (List [orig])

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

pprForm :: Code -> P.Doc
pprForm (L _ form) =
  case form of
    Atom x -> P.text "Atom" P.<+> P.parens (pprAtom x)
    List xs -> P.text "List" P.<+> P.nest 2 (pprForms xs)
    HsList xs -> P.text "HsList" P.<+> P.nest 2 (pprForms xs)
    TEnd -> P.text "TEnd"

pprForms :: [Code] -> P.Doc
pprForms forms =
  P.brackets (P.sep (P.punctuate P.comma (map pprForm forms)))

pForm :: Code -> P.Doc
pForm (L _ form) =
  case form of
    Atom a -> pAtom a
    List forms -> pForms P.parens forms
    HsList forms -> pForms P.brackets forms
    TEnd -> P.text "TEnd"

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
  listToCode xs =
     let xs' = map toCode xs
         l = getLoc (mkLocatedList xs')
     in  L l (HsList xs')

  listFromCode :: Code -> Maybe [a]
  listFromCode xs = case unLoc xs of
                      HsList as -> mapM fromCode as
                      _          -> Nothing

instance Codish Atom where
  toCode = genSrc . Atom
  fromCode a =
    case unLoc a of
      Atom x -> Just x
      _       -> Nothing

instance Codish () where
  toCode _ = genSrc (Atom AUnit)
  fromCode a =
    case unLoc a of
      Atom AUnit -> Just ()
      _           -> Nothing

instance Codish Char where
  toCode = genSrc . Atom . AChar
  fromCode a =
    case unLoc a of
      Atom (AChar x) -> Just x
      _               -> Nothing
  listToCode = genSrc . Atom . AString
  listFromCode a = case unLoc a of
                     Atom (AString s) -> Just s
                     _ -> Nothing

instance Codish Int where
  toCode = genSrc . Atom . AInteger . fromIntegral
  fromCode a =
    case unLoc a of
      Atom (AInteger n) -> Just (fromIntegral n)
      _                  -> Nothing

instance Codish Integer where
  toCode = genSrc . Atom . AInteger
  fromCode a =
    case unLoc a of
      Atom (AInteger n) -> Just n
      _                 -> Nothing

instance Codish Double where
  toCode a =
    let r = toRational a
    in  genSrc (Atom (AFractional (FL (show a) r)))
  fromCode a =
    case unLoc a of
      Atom (AFractional x) -> Just (fromRational (fl_value x))
      _                     -> Nothing

instance Codish a => Codish [a] where
  toCode = listToCode
  fromCode = listFromCode

-- `FlexibleInstance' language pragma required for below.
-- instance Codish a => Codish (LForm a) where
--   toCode (L l form) =
--     case form of
--       Atom a    -> let (L _ b) = toCode a in L l b
--       List xs   -> L l (List (map toCode xs))
--       HsList xs -> L l (HsList (map toCode xs))
--       TEnd       -> L l TEnd
--   fromCode form@(L _ x) =
--     case x of
--       Atom _  -> fromCode form
--       _        -> error "fromCode: LForm"

instance Codish (Form Atom) where
  toCode = genSrc
  fromCode = Just . unLoc

-- Requires "FlexibleInstances".
instance Codish Code where
  toCode = id
  fromCode = Just

instance Codish SrcSpan where
  toCode sp =
    case sp of
      UnhelpfulSpan txt ->
        list [atom (ASymbol "mkSkSrcSpan")
             ,atom (AString (unpackFS txt))]
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
      list = genSrc . List
      atom = genSrc . Atom
  fromCode form =
    case unLoc form of
      List [L _ (Atom (ASymbol "mkSkSrcSpan"))
            ,L _ (Atom (AString txt))]
       -> Just (mkSkSrcSpan txt)
      List [L _ (Atom (ASymbol "mkSrcSpan"))
            ,L _ (List [L _ (Atom (ASymbol "mkSrcLoc"))
                        ,L _ (List [L _ (Atom (ASymbol "fsLit"))
                                    ,L _ (Atom (AString fn))])
                        ,L _ (Atom (AInteger sl))
                        ,L _ (Atom (AInteger sc))])
            ,L _ (List [L _ (Atom (ASymbol "mkSrcLoc"))
                        ,L _ (List [L _ (Atom (ASymbol "fsLit"))
                                    ,L _ (Atom (AString _))])
                        ,L _ (Atom (AInteger el))
                        ,L _ (Atom (AInteger ec))])]
       -> Just (mkSrcSpan loc1 loc2)
         where
           loc1 = mkSrcLoc fn' (fromIntegral sl) (fromIntegral sc)
           loc2 = mkSrcLoc fn' (fromIntegral el) (fromIntegral ec)
           fn' = fsLit fn
      _ -> Nothing

unquoteSplice :: Codish a => a -> [Code]
unquoteSplice form =
  case unLoc (toCode form) of
    List xs   -> xs
    HsList xs -> xs
    _          -> []

mkSkSrcSpan :: String -> SrcSpan
mkSkSrcSpan = UnhelpfulSpan . fsLit

skSrcSpan :: SrcSpan
skSrcSpan = mkSkSrcSpan "<sk generated code>"

genSrc :: a -> Located a
genSrc = L skSrcSpan

quoted :: Form Atom -> Code
quoted = L (UnhelpfulSpan (fsLit "<quoted code>"))

mkLocatedList ::  [Located a] -> Located [Located a]
mkLocatedList [] = genSrc []
mkLocatedList ms = L (combineLocs (head ms) (last ms)) ms
