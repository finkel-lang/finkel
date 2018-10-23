{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
-- | Syntax for patterns.
module Language.SK.Syntax.HPat where

-- base
import Data.Char (isUpper)

-- ghc
import BasicTypes (Boxity(..), SourceText(..))
import FastString (headFS)
import HsLit (HsLit(..))
import HsPat (HsRecFields(..), Pat(..))
import HsTypes (HsConDetails(..))
import HsUtils (mkHsIsString, mkNPat, nlWildPat)

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

b_intP :: Code -> HPat
b_intP (LForm (L l (Atom (AInteger n)))) =
  L l (mkNPat (L l (mkHsIntegral_compat n)) Nothing)
b_intP _ = error "b_intP"
{-# INLINE b_intP #-}

b_stringP :: Code -> HPat
b_stringP (LForm (L l (Atom (AString str)))) =
  L l (mkNPat (L l lit) Nothing)
  where
    lit = hsIsString (SourceText (show str)) (fsLit str)
    hsIsString s t =
#if MIN_VERSION_ghc(8,6,0)
      mkHsIsString s t
#else
      mkHsIsString s t placeHolderType
#endif
b_stringP _ = error "b_stringP"
{-# INLINE b_stringP #-}

b_charP :: Code -> HPat
b_charP (LForm (L l (Atom (AChar c)))) = L l (litPat lit)
  where
    lit = HsChar (SourceText (show c)) c
    litPat = LitPat NOEXT
b_charP _ = error "b_charP"
{-# INLINE b_charP #-}

b_unitP :: Code -> HPat
b_unitP (LForm (L l (Atom AUnit))) = L l (mkTuplePat_compat [])
b_unitP _ = error "b_unitP"
{-# INLINE b_unitP #-}

b_symP :: Code -> HPat
b_symP (LForm (L l (Atom (ASymbol name))))
   | name == fsLit "_"
    = L l mkWildPat_compat
   | isUpper x || x == ':'
    = L l (ConPatIn (L l (mkVarRdrName name)) (PrefixCon []))
   | otherwise
    = L l (varPat (L l (mkRdrName name)))
   where
     x = headFS name
     varPat = VarPat NOEXT
b_symP _ = error "b_symP: not a symbol"
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
b_labeledP (LForm (L l (Atom (ASymbol name)))) ps
  | isUpper x || x == ':' =  do
    let rc = HsRecFields { rec_flds = map mkcfld' ps
                         , rec_dotdot = Nothing }
        mkcfld' (LForm (L _ (Atom (ASymbol n))), p) = mkcfld (n, p)
        mkcfld' _ = error "b_labeledP.mkfld'"
    return (L l (ConPatIn (L l (mkVarRdrName name)) (RecCon rc)))
  | otherwise = builderError
  where
     x = headFS name
b_labeledP _ _ = error "b_labeledP"
{-# INLINE b_labeledP #-}

b_tupP :: Code -> [HPat] -> HPat
b_tupP (LForm (L l _)) ps = L l (mkTuplePat_compat ps)
{-# INLINE b_tupP #-}

b_asP :: Code -> HPat -> HPat
b_asP (LForm (L l (Atom (ASymbol name)))) pat =
  L l (asPat (L l (mkRdrName name)) (mkParPat_compat pat))
  where
    asPat = AsPat NOEXT
b_asP _ _ = error "b_asP: not a symbol"
{-# INLINE b_asP #-}

b_lazyP :: HPat -> HPat
b_lazyP pat@ (L l _) = mkParPat_compat (L l (lazyPat pat))
  where
    lazyPat = LazyPat NOEXT
{-# INLINE b_lazyP #-}

b_conP :: Code -> [HPat] -> Builder HPat
b_conP (LForm (L l (Atom (ASymbol name)))) rest
  | isUpper x || x == ':'
    = return (mkParPat_compat
               (L l (ConPatIn (L l (mkVarRdrName name))
                              (PrefixCon rest))))
  | otherwise = builderError
  where
    x = headFS name
b_conP _ _ = error "b_conP"
{-# INLINE b_conP #-}

mkWildPat_compat :: Pat PARSED
mkWildPat_compat = case nlWildPat of L _ pat -> pat
{-# INLINE mkWildPat_compat #-}

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
