{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
-- | Syntax for patterns.
module Language.SK.Syntax.HPat where

-- base
import Data.List (foldl1')

-- ghc
import BasicTypes (Boxity(..), SourceText(..))
import FastString (fsLit, headFS, nullFS, tailFS)
import HsLit (HsLit(..))
import HsPat (HsRecFields(..), Pat(..))
import HsTypes (HsConDetails(..))
import HsUtils (mkHsIsString, mkLHsSigWcType, mkNPat, nlWildPat)
import Lexeme (isLexCon, isLexConId, isLexConSym, isLexSym)
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

b_wildP :: Code -> HPat
b_wildP (LForm (L l _)) = L l wildPat
{-# INLINE b_wildP #-}

b_symP :: Code -> Builder HPat
b_symP orig@(LForm (L l form))
  | (Atom (ASymbol name)) <- form
  , let hdchr = headFS name
  , let tlchrs = tailFS name
  = case () of
      _ | isLexCon name
        -- Constructor.
        -> return (L l (ConPatIn (L l (mkVarRdrName name))
                                 (PrefixCon [])))
        | hdchr == '~'
        -- Lazy pattern or operator function.
        -> if nullFS tlchrs
              then failB "invalid use of `~'"
              else if isLexSym tlchrs
                      -- Operator function.
                      then do checkVarId orig name
                              let name' = L l (mkRdrName name)
                              return (L l (varPat name'))
                      -- Lazy pattern.
                      else do checkVarId orig tlchrs
                              let name' = L l (mkRdrName tlchrs)
                                  pat = L l (varPat name')
                              return (L l (lazyPat pat))
        | hdchr == '!'
        , not (nullFS tlchrs)
        , not (isLexSym tlchrs)
        -- Bang pattern.
        -> do let pat = L l (varPat (L l (mkRdrName tlchrs)))
              checkVarId orig tlchrs
              return (L l (bangPat pat))
        | otherwise
        -- Varid.
        -> do checkVarId orig name
              return (L l (varPat (L l (mkRdrName name))))
  | otherwise = builderError
  where
    varPat = VarPat NOEXT
    lazyPat = LazyPat NOEXT
    bangPat = BangPat NOEXT
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
  , isLexCon name = do
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
  = return (L l (asPat (L l (mkRdrName name)) (mkParPat' pat)))
  | otherwise
  = builderError
  where
    asPat = AsPat NOEXT
{-# INLINE b_asP #-}

b_lazyP :: HPat -> HPat
b_lazyP pat@ (L l _) = L l (LazyPat NOEXT pat)
{-# INLINE b_lazyP #-}

b_bangP :: HPat -> HPat
b_bangP pat@(L l _) = L l (BangPat NOEXT pat)
{-# INLINE b_bangP #-}

b_conP :: [Code] -> Bool -> [HPat] -> Builder HPat
b_conP forms is_paren rest =
  case forms of
    [LForm (L l (Atom (ASymbol name)))]
      | is_paren, isLexConSym name -> prefixPat
      | isLexConId name -> prefixPat
      | isLexConSym name -> infixPat
      where
       lrname = L l (mkVarRdrName name)
       prefixPat =
         return (mkParPat' (L l (ConPatIn lrname (PrefixCon rest))))
       infixPat =
         let f lhp rhp = L l (ConPatIn lrname (InfixCon lhp rhp))
         in  return (mkParPat' (foldl1' f rest))
    _ -> builderError
{-# INLINE b_conP #-}

b_sigP :: Code -> HPat -> HType -> HPat
b_sigP (LForm (L l _)) pat ty =
#if MIN_VERSION_ghc(8,6,0)
  L l (SigPat (mkLHsSigWcType ty) pat)
#else
  L l (SigPatIn pat (mkLHsSigWcType ty))
#endif
{-# INLINE b_sigP #-}

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

mkParPat' :: HPat -> HPat
mkParPat' (p @ (L l _)) = L l (parPat p)
  where
    parPat = ParPat NOEXT
{-# INLINE mkParPat' #-}
