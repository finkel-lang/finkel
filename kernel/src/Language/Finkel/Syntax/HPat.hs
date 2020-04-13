{-# LANGUAGE CPP          #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | Syntax for patterns.
module Language.Finkel.Syntax.HPat where

#include "Syntax.h"

-- base
import Data.List                       (foldl')

-- ghc
import BasicTypes                      (Boxity (..), SourceText (..))
import FastString                      (headFS, nullFS, tailFS)
import GHC_Hs_Lit                      (HsLit (..))
import GHC_Hs_Pat                      (HsRecFields (..), Pat (..))
import GHC_Hs_Types                    (HsConDetails (..))
import GHC_Hs_Utils                    (mkHsIsString, mkLHsSigWcType, mkNPat,
                                        nlWildPat)
import Lexeme                          (isLexCon, isLexConId, isLexConSym,
                                        isLexSym)
import SrcLoc                          (GenLocated (..))

#if MIN_VERSION_ghc(8,10,0)
import GHC_Hs_Extension                (noExtField)
#elif MIN_VERSION_ghc(8,6,0)
import GHC_Hs_Extension                (noExt)
#else
import PlaceHolder                     (placeHolderType)
#endif

#if MIN_VERSION_ghc(8,6,0)
import GHC_Hs_Pat                      (parenthesizePat)
#else
import SrcLoc                          (unLoc)
#endif

-- Internal
import Language.Finkel.Builder
import Language.Finkel.Form
import Language.Finkel.Syntax.SynUtils

#if !MIN_VERSION_ghc(8,6,0)
import Language.Finkel.Syntax.HExpr
#endif

-- ------------------------------------------------------------------------
--
-- Pattern
--
-- ------------------------------------------------------------------------

-- Note: [Pattern from expression]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Until ghc-8.8.x, parser in GHC had intermediate constructors in HsExpr data
-- type, to make HsPat values from HsExpr. In ghc-8.10.1, the intermediate
-- constructors were removed and RdrHsSyn.{PatBuilder,ECP,DisambECP} and
-- related functions were introduced.
--
-- At the moment, Finkel parser does not use the ECP data type, since the prefix
-- notation does not introduce much disambiguate syntax. Also, due to the
-- ubiquitous use of the parentheses, parsing without adding redundant
-- parentheses in the AST seemed difficult with the ECP approach.

b_intP :: Code -> Builder HPat
b_intP (LForm (L l form))
  | (Atom (AInteger n)) <- form
  = return (cL l (mkNPat (L l (mkHsIntegral_compat n)) Nothing))
  | otherwise
  = builderError
{-# INLINE b_intP #-}

b_stringP :: Code -> Builder HPat
b_stringP (LForm (L l form))
  | (Atom (AString _ str)) <- form
  = return (cL l (mkNPat (L l (lit str)) Nothing))
  | otherwise
  = builderError
  where
    lit str = hsIsString (SourceText (show str)) str
    hsIsString s t =
#if MIN_VERSION_ghc(8,6,0)
      mkHsIsString s t
#else
      mkHsIsString s t placeHolderType
#endif
{-# INLINE b_stringP #-}

b_charP :: Code -> Builder HPat
b_charP (LForm (dL->L l form))
  | (Atom (AChar _ c)) <- form = return (cL l (LitPat NOEXT (lit c)))
  | otherwise                  = builderError
  where
    lit c = HsChar (SourceText (show c)) c
{-# INLINE b_charP #-}

b_unitP :: Code -> Builder HPat
b_unitP (LForm (dL->L l form))
  | Atom AUnit <- form = return (cL l (mkTuplePat' []))
  | otherwise          = builderError
{-# INLINE b_unitP #-}

b_wildP :: Code -> HPat
b_wildP (LForm (dL->L l _)) = cL l wildPat
  where
    wildPat | L _ pat <- dL nlWildPat = pat

{-# INLINE b_wildP #-}

b_symP :: Code -> Builder HPat
b_symP orig@(LForm (dL->L l form))
  | (Atom (ASymbol name)) <- form
  , let hdchr = headFS name
  , let tlchrs = tailFS name
  = case () of
      _ | isLexCon name
        -- Constructor.
        -> return (cL l (ConPatIn (L l (mkVarRdrName name))
                                  (PrefixCon [])))
        | hdchr == '~'
        -- Lazy pattern or operator function.
        -> if nullFS tlchrs
              then failB "invalid use of `~'"
              else if isLexSym tlchrs
                      -- Operator function.
                      then do checkVarId orig name
                              let name' = L l (mkRdrName name)
                              return (cL l (VarPat NOEXT name'))
                      -- Lazy pattern.
                      else do checkVarId orig tlchrs
                              let name' = L l (mkRdrName tlchrs)
                                  pat = cL l (VarPat NOEXT name')
                              return (cL l (LazyPat NOEXT pat))
        | hdchr == '!'
        , not (nullFS tlchrs)
        , not (isLexSym tlchrs)
        -- Bang pattern.
        -> do let pat = cL l (VarPat NOEXT (L l (mkRdrName tlchrs)))
              checkVarId orig tlchrs
              return (cL l (BangPat NOEXT pat))
        | otherwise
        -- Varid.
        -> do checkVarId orig name
              return (cL l (VarPat NOEXT (L l (mkRdrName name))))
  | otherwise = builderError
{-# INLINE b_symP #-}

b_hsListP :: [HPat] -> HPat
b_hsListP pats = p
  where
#if MIN_VERSION_ghc(8,10,0)
     -- p = L (getLoc (mkLocatedList pats)) (listPat pats)
     p = case dL (mkLocatedList pats) of L l _ -> L l (listPat pats)
#elif MIN_VERSION_ghc(8,8,0)
     p = listPat pats
#else
     -- p = L (getLoc (mkLocatedList pats)) (listPat pats)
     p = case dL (mkLocatedList pats) of L l _ -> L l (listPat pats)
#endif
     listPat ps =
#if MIN_VERSION_ghc(8,6,0)
       ListPat NOEXT ps
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
        cid = L l (mkVarRdrName name)
        cpd = RecCon rc
    return (cL l (ConPatIn cid cpd))
  | otherwise = builderError
{-# INLINE b_labeledP #-}

b_tupP :: Code -> [HPat] -> HPat
b_tupP (LForm (L l _)) ps = cL l (mkTuplePat' ps)
{-# INLINE b_tupP #-}

b_asP :: Code -> HPat -> Builder HPat
b_asP (LForm (dL->L l form)) pat
  | (Atom (ASymbol name)) <- form
  = return (cL l (asPat (L l (mkRdrName name)) (mkParPat' pat)))
  | otherwise
  = builderError
  where
    asPat = AsPat NOEXT
{-# INLINE b_asP #-}

b_lazyP :: HPat -> HPat
b_lazyP (dL-> L l pat0) = cL l (LazyPat NOEXT pat1)
  where
    pat1 = parenthesizePat' appPrec (cL l pat0)
{-# INLINE b_lazyP #-}

b_bangP :: HPat -> HPat
b_bangP (dL->L l pat) = cL l (BangPat NOEXT (cL l pat))
{-# INLINE b_bangP #-}

b_conP :: [Code] -> Bool -> [HPat] -> Builder HPat
b_conP forms is_paren rest =
  case forms of
    [LForm (L l (Atom (ASymbol name)))]
      | is_paren, isLexConSym name -> prefixPat
      | isLexConId name -> prefixPat
      | isLexConSym name -> infixPat
      where
        rname = mkVarRdrName name
        lrname = L l rname
        prefixPat = return (cL l (ConPatIn lrname (PrefixCon prest)))
        prest = map (parenthesizePat' appPrec) rest
        infixPat =
          case rest of
            (hd:rest') ->
              let f lh rh = cL l (ConPatIn lrname (InfixCon lh (paren rh)))
                  paren = parenthesizePat' opPrec
              in  return (foldl' f (parenthesizePat' opPrec hd) rest')
            _ -> builderError
    _ -> builderError
{-# INLINE b_conP #-}

b_sigP :: Code -> HPat -> HType -> HPat
b_sigP (LForm (L l _)) pat ty =
#if MIN_VERSION_ghc(8,8,0)
  cL l (SigPat NOEXT pat (mkLHsSigWcType ty))
#elif MIN_VERSION_ghc(8,6,0)
  cL l (SigPat (mkLHsSigWcType ty) pat)
#else
  cL l (SigPatIn pat (mkLHsSigWcType ty))
#endif
{-# INLINE b_sigP #-}

mkTuplePat' :: [HPat] -> Pat PARSED
mkTuplePat' ps =
#if MIN_VERSION_ghc(8,6,0)
  TuplePat NOEXT ps Boxed
#else
  TuplePat ps Boxed []
#endif
{-# INLINE mkTuplePat' #-}

mkParPat' :: HPat -> HPat
mkParPat' (dL->L l p) =
  cL l (ParPat NOEXT (cL l p))
{-# INLINE mkParPat' #-}


-- ------------------------------------------------------------------------
--
-- Parenthesizing
--
-- ------------------------------------------------------------------------

-- | Parenthesize patterns.
parenthesizePat' :: PprPrec -> HPat -> HPat
#if MIN_VERSION_ghc(8,6,0)
parenthesizePat' = parenthesizePat
#else
-- Brought from "compiler/hsSyn/HsPa.hs" in tghc 8.8.3 source code. Modified to
-- work with ghc 8.2.x and ghc 8.4.x.

-- | @'parenthesizePat' p pat@ checks if @'patNeedsParens' p pat@ is true, and
-- if so, surrounds @pat@ with a 'ParPat'. Otherwise, it simply returns @pat@.
parenthesizePat' p lpat@(dL->L loc pat)
  | patNeedsParens p pat = cL loc (ParPat NOEXT lpat)
  | otherwise            = lpat

-- | @'patNeedsParens' p pat@ returns 'True' if the pattern @pat@ needs
-- parentheses under precedence @p@.
patNeedsParens :: PprPrec -> Pat p -> Bool
patNeedsParens p = go
  where
    go (NPlusKPat {})    = p > opPrec
    go (SplicePat {})    = False
    go (ConPatIn _ ds)   = conPatNeedsParens p ds
    go cp@(ConPatOut {}) = conPatNeedsParens p (pat_args cp)
    -- go (SigPat {})       = p >= sigPrec
    go (ViewPat {})      = True
    go (CoPat _ q _)     = go q
    go (WildPat {})      = False
    go (VarPat {})       = False
    go (LazyPat {})      = False
    go (BangPat {})      = False
    go (ParPat {})       = False
    go (AsPat {})        = False
    go (TuplePat {})     = False
    go (SumPat {})       = False
    go (ListPat {})      = False
    go (LitPat l)        = hsLitNeedsParens p l
    go (NPat lol _ _ _)  = hsOverLitNeedsParens p (unLoc lol)
    -- go (XPat {})         = True -- conservative default
    go _                 = True

-- | @'conPatNeedsParens' p cp@ returns 'True' if the constructor patterns @cp@
-- needs parentheses under precedence @p@.
conPatNeedsParens :: PprPrec -> HsConDetails a b -> Bool
conPatNeedsParens p = go
  where
    go (PrefixCon args) = p >= appPrec && not (null args)
    go (InfixCon {})    = p >= opPrec
    go (RecCon {})      = False
#endif
