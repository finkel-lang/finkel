{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
-- | Syntax for patterns.
module Language.SK.Syntax.HPat where

-- base
import Data.Char (isUpper)

-- ghc
import BasicTypes (Boxity(..), SourceText(..))
import FastString (fsLit, headFS, tailFS)
import HsLit (HsLit(..))
import HsPat (HsRecFields(..), Pat(..))
import HsTypes (HsConDetails(..))
import HsUtils (mkHsIsString, mkNPat, nlWildPat)
import SrcLoc (GenLocated(..), getLoc)

#if MIN_VERSION_ghc(8,6,0)
import HsExtension (noExt)
#else
import PlaceHolder (placeHolderType)
#endif

-- Internal
import Language.SK.Builder
import Language.SK.Form
import Language.SK.Syntax.SynUtils

#include "Syntax.h"

-- ---------------------------------------------------------------------
--
-- Pattern
--
-- ---------------------------------------------------------------------

b_intP :: Code -> Builder HPat
b_intP (LForm (L l form))
  | (Atom (AInteger n)) <- form
  = return (L l (mkNPat (L l (mkHsIntegral_compat n)) Nothing))
  | otherwise
  = builderError
{-# INLINE b_intP #-}

b_stringP :: Code -> Builder HPat
b_stringP (LForm (L l form))
  | (Atom (AString str)) <- form
  = return (L l (mkNPat (L l (lit str)) Nothing))
  | otherwise
  = builderError
  where
    lit str = hsIsString (SourceText (show str)) (fsLit str)
    hsIsString s t =
#if MIN_VERSION_ghc(8,6,0)
      mkHsIsString s t
#else
      mkHsIsString s t placeHolderType
#endif
{-# INLINE b_stringP #-}

b_charP :: Code -> Builder HPat
b_charP (LForm (L l form))
  | (Atom (AChar c)) <- form = return (L l (litPat (lit c)))
  | otherwise                = builderError
  where
    lit c = HsChar (SourceText (show c)) c
    litPat = LitPat NOEXT
{-# INLINE b_charP #-}

b_unitP :: Code -> Builder HPat
b_unitP (LForm (L l form))
  | Atom AUnit <- form = return (L l (mkTuplePat_compat []))
  | otherwise          = builderError
{-# INLINE b_unitP #-}

b_symP :: Code -> Builder HPat
b_symP (LForm (L l form))
  | (Atom (ASymbol name)) <- form
  , let hdchr = headFS name
  = case () of
      _ | name == fsLit "_"
        -> return (L l wildPat)
        | isUpper hdchr || hdchr == ':'
        -> return (L l (ConPatIn (L l (mkVarRdrName name))
                                 (PrefixCon [])))
        | hdchr == '~'
        -> let name' = tailFS name
               pat = L l (varPat (L l (mkRdrName name')))
           in  return (L l (lazyPat pat))
        | otherwise
        -> return (L l (varPat (L l (mkRdrName name))))
  | otherwise = builderError
  where
    varPat = VarPat NOEXT
    lazyPat = LazyPat NOEXT
{-# INLINE b_symP #-}

b_hsListP :: [HPat] -> HPat
b_hsListP pats = L l (listPat pats)
  where
    l = getLoc (mkLocatedList pats)
    listPat ps =
#if MIN_VERSION_ghc(8,6,0)
      ListPat noExt ps
#else
      ListPat ps placeHolderType Nothing
#endif
{-# INLINE b_hsListP #-}

b_labeledP :: Code -> [(Code, HPat)] -> Builder HPat
b_labeledP (LForm (L l form)) ps
  | Atom (ASymbol name) <- form
  , let x = headFS name
  , isUpper x || x == ':' = do
    let mkcfld' (LForm (L nl sym), p)
          | Atom (ASymbol n) <- sym = return (mkcfld (L nl n, p))
          | otherwise               = builderError
    flds <- mapM mkcfld' ps
    let rc = HsRecFields { rec_flds = flds
                         , rec_dotdot = Nothing }
    return (L l (ConPatIn (L l (mkVarRdrName name)) (RecCon rc)))
  | otherwise = builderError

{-# INLINE b_labeledP #-}

b_tupP :: Code -> [HPat] -> HPat
b_tupP (LForm (L l _)) ps = L l (mkTuplePat_compat ps)
{-# INLINE b_tupP #-}

b_asP :: Code -> HPat -> Builder HPat
b_asP (LForm (L l form)) pat
  | (Atom (ASymbol name)) <- form
  = return (L l (asPat (L l (mkRdrName name)) (mkParPat_compat pat)))
  | otherwise
  = builderError
  where
    asPat = AsPat NOEXT
{-# INLINE b_asP #-}

b_lazyP :: HPat -> HPat
b_lazyP pat@ (L l _) = L l (lazyPat pat)
  where
    lazyPat = LazyPat NOEXT
{-# INLINE b_lazyP #-}

b_conP :: Code -> [HPat] -> Builder HPat
b_conP (LForm (L l form)) rest
  | Atom (ASymbol name) <- form
  , let x = headFS name
  , isUpper x || x == ':'
  = return (mkParPat_compat
              (L l (ConPatIn (L l (mkVarRdrName name))
                             (PrefixCon rest))))
  | otherwise = builderError
{-# INLINE b_conP #-}

wildPat :: Pat PARSED
wildPat = case nlWildPat of L _ pat -> pat
{-# INLINE wildPat #-}

mkTuplePat_compat :: [HPat] -> Pat PARSED
mkTuplePat_compat ps = tuplePat ps Boxed
  where
    tuplePat pats boxity =
#if MIN_VERSION_ghc(8,6,0)
      TuplePat noExt pats boxity
#else
      TuplePat pats boxity []
#endif
{-# INLINE mkTuplePat_compat #-}

mkParPat_compat :: HPat -> HPat
mkParPat_compat (p @ (L l _)) = L l (parPat p)
  where
    parPat = ParPat NOEXT
{-# INLINE mkParPat_compat #-}
