{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE ViewPatterns     #-}

-- | Syntax for patterns.
module Language.Finkel.Syntax.HPat where

#include "Syntax.h"
#include "ghc_modules.h"

-- base
import Data.Either                     (partitionEithers)
import Data.List                       (foldl')

-- ghc
import GHC_Data_FastString             (headFS, nullFS)
import GHC_Hs_Lit                      (HsLit (..), HsOverLit)
import GHC_Hs_Pat                      (HsConPatDetails, HsRecFields (..),
                                        Pat (..))
import GHC_Hs_Type                     (HsConDetails (..))
import GHC_Hs_Utils                    (mkHsIsString, mkNPat, nlWildPat)
import GHC_Types_Basic                 (Boxity (..))
import GHC_Types_SrcLoc                (GenLocated (..))
import GHC_Utils_Lexeme                (isLexCon, isLexConId, isLexConSym,
                                        isLexSym)

import GHC_Types_SrcLoc                (Located)

#if MIN_VERSION_ghc(9,4,0)
import GHC.Hs.Pat                      (gParPat)
#endif

#if MIN_VERSION_ghc(9,0,0)
import GHC_Hs_Pat                      (ConLikeP)
#elif MIN_VERSION_ghc(8,4,0)
import GHC_Hs_Extension                (IdP)
#endif

#if MIN_VERSION_ghc(9,0,0)
import GHC_Hs_Type                     (mkHsPatSigType)
#else
import GHC_Hs_Utils                    (mkLHsSigWcType)
#endif

#if !MIN_VERSION_ghc(8,6,0)
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
import Language.Finkel.Syntax.HExpr    hiding (mkcfld')
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
-- constructors were removed and RdrHsSyn.{PatBuilder,ECP,DisambECP} and related
-- functions were introduced. Those modules were renamed to
-- GHC.Parser.PostProcess in ghc 9.0.1.
--
-- At the moment, Finkel parser does not use the ECP, since the prefix notation
-- does not introduce ambiguous syntax so much. Also, due to the ubiquitous use
-- of the parentheses, parsing without adding redundant parentheses in the AST
-- seemed difficult with the ECP approach.

b_intP :: Code -> Builder HPat
b_intP (LForm (L l form)) =
  case form of
    Atom (AInteger n) -> return $! lA l (npat n)
    _                 -> builderError
  where
    npat n = mkNPat' (L l (mkHsIntegral_compat n))
{-# INLINABLE b_intP #-}

b_stringP :: Code -> Builder HPat
b_stringP (LForm (L l form)) =
  case form of
    Atom (AString _ str) -> return $! lA l (npat str)
    _                    -> builderError
  where
    npat str = mkNPat' (L l (lit str))
    lit str = hsIsString (SourceText (show str)) str
    hsIsString s t =
#if MIN_VERSION_ghc(8,6,0)
      mkHsIsString s t
#else
      mkHsIsString s t placeHolderType
#endif
{-# INLINABLE b_stringP #-}

b_charP :: Code -> Builder HPat
b_charP (LForm (L l form)) =
  case form of
    Atom (AChar _ c) -> return $! lA l (LitPat NOEXT (lit c))
    _                -> builderError
  where
    lit c = HsChar (SourceText (show c)) c
{-# INLINABLE b_charP #-}

b_unitP :: Code -> Builder HPat
b_unitP (LForm (L l form)) =
  case form of
    Atom AUnit -> return $! lA l (mkTuplePat' [])
    _          -> builderError
{-# INLINABLE b_unitP #-}

b_wildP :: Code -> HPat
b_wildP (LForm (L l _)) = lA l wildPat
  where
    wildPat | L _ pat <- dL nlWildPat = pat
{-# INLINABLE b_wildP #-}

b_symP :: Code -> Builder HPat
b_symP orig@(LForm (L l form))
  | (Atom (ASymbol name)) <- form
  , let hdchr = headFS name
  , let tlchrs = tailFS name
  = case () of
      _ | isLexCon name
        -- Constructor.
        -> return (lA l (mkConPat (lN l (mkVarRdrName name)) (mkPrefixCon [])))
        | hdchr == '~'
        -- Lazy pattern or operator function.
        -> if nullFS tlchrs
              then failB "Invalid use of `~'"
              else if isLexSym tlchrs
                      -- Operator function.
                      then do checkVarId orig name
                              let name' = lN l (mkRdrName name)
                              return (lA l (VarPat NOEXT name'))
                      -- Lazy pattern.
                      else do checkVarId orig tlchrs
                              let name' = lN l (mkRdrName tlchrs)
                                  pat = lA l (VarPat NOEXT name')
                              return (lA l (LazyPat NOEXT pat))
        | hdchr == '!'
        , not (nullFS tlchrs)
        , not (isLexSym tlchrs)
        -- Bang pattern.
        -> do let pat = lA l (VarPat NOEXT (lN l (mkRdrName tlchrs)))
              checkVarId orig tlchrs
              return (lA l (BangPat NOEXT pat))
        | otherwise
        -- Varid.
        -> do checkVarId orig name
              return (lA l (VarPat NOEXT (lN l (mkRdrName name))))
  | otherwise = builderError
{-# INLINABLE b_symP #-}

b_hsListP :: [HPat] -> HPat
b_hsListP pats = p
  where
#if MIN_VERSION_ghc(8,10,0)
     p = case dL (mkLocatedListA pats) of L l _ -> L l (listPat pats)
#elif MIN_VERSION_ghc(8,8,0)
     p = listPat pats
#else
     p = case dL (mkLocatedList pats) of L l _ -> L l (listPat pats)
#endif
     listPat ps =
#if MIN_VERSION_ghc(8,6,0)
       ListPat NOEXT ps
#else
       ListPat ps placeHolderType Nothing
#endif
{-# INLINABLE b_hsListP #-}

b_labeledP :: Code -> [PreRecField HPat] -> Builder HPat
b_labeledP (LForm (L l form)) ps
  | Atom (ASymbol name) <- form
  , isLexCon name = do
    let mkcfld' (lab, mb_p) =
          case mb_p of
            Just p  -> mkcfld False (lab, p)
            Nothing -> mkcfld True (lab, punned)
        punned = lA l (VarPat NOEXT (lN l pun_RDR))
        (wilds, non_wilds) = partitionEithers ps
        mb_dotdot = case wilds of
          []                  -> Nothing
#if MIN_VERSION_ghc(8,10,0)
          (LForm (L wl _): _) -> Just (L wl (length non_wilds))
#else
          _                   -> Just (length non_wilds)
#endif
        flds = map mkcfld' non_wilds
        rc = HsRecFields { rec_flds = flds
                         , rec_dotdot = mb_dotdot }
        cid = lN l (mkVarRdrName name)
        cpd = RecCon rc
    return (lA l (mkConPat cid cpd))
  | otherwise = builderError
{-# INLINABLE b_labeledP #-}

b_tupP :: Code -> [HPat] -> HPat
b_tupP (LForm (L l _)) ps = lA l (mkTuplePat' ps)
{-# INLINABLE b_tupP #-}

b_asP :: Code -> HPat -> Builder HPat
b_asP (LForm (dL->L l form)) pat =
  case form of
    Atom (ASymbol name) ->
      return $! lA l (asPat (lN l (mkRdrName name)) (mkParPat' pat))
    _ -> builderError
  where
    asPat = AsPat NOEXT
{-# INLINABLE b_asP #-}

b_lazyP :: HPat -> HPat
b_lazyP (dL-> L l pat0) = cL l (LazyPat NOEXT pat1)
  where
    pat1 = parenthesizePat' appPrec (cL l pat0)
{-# INLINABLE b_lazyP #-}

b_bangP :: HPat -> HPat
b_bangP (dL->L l pat) = cL l (BangPat NOEXT (cL l pat))
{-# INLINABLE b_bangP #-}

b_conP :: [Code] -> Bool -> [HPat] -> Builder HPat
b_conP forms is_paren rest =
  case forms of
    [LForm (L l (Atom (ASymbol name)))]
      | is_paren, isLexConSym name -> prefixPat
      | isLexConId name -> prefixPat
      | isLexConSym name -> infixPat
      where
        rname = mkVarRdrName name
        lrname = lN l rname
        prefixPat = return (lA l (mkConPat lrname (mkPrefixCon prest)))
        prest = map (parenthesizePat' appPrec) rest
        infixPat =
          case rest of
            (hd:rest') ->
              let f lh rh = lA l (mkConPat lrname (InfixCon lh (paren rh)))
                  paren = parenthesizePat' opPrec
              in  return (foldl' f (parenthesizePat' opPrec hd) rest')
            _ -> builderError
    _ -> builderError
{-# INLINABLE b_conP #-}

b_sigP :: Code -> HPat -> HType -> HPat
b_sigP (LForm (L l _)) pat ty =
#if MIN_VERSION_ghc(9,2,0)
  lA l (SigPat NOEXT pat (mkHsPatSigType NOEXT ty))
#elif MIN_VERSION_ghc(9,0,0)
  lA l (SigPat NOEXT pat (mkHsPatSigType ty))
#elif MIN_VERSION_ghc(8,8,0)
  cL l (SigPat NOEXT pat (mkLHsSigWcType ty))
#elif MIN_VERSION_ghc(8,6,0)
  cL l (SigPat (mkLHsSigWcType ty) pat)
#else
  cL l (SigPatIn pat (mkLHsSigWcType ty))
#endif
{-# INLINABLE b_sigP #-}

mkTuplePat' :: [HPat] -> Pat PARSED
mkTuplePat' ps =
#if MIN_VERSION_ghc(8,6,0)
  TuplePat NOEXT ps Boxed
#else
  TuplePat ps Boxed []
#endif
{-# INLINABLE mkTuplePat' #-}

mkParPat' :: HPat -> HPat
#if MIN_VERSION_ghc(9,4,0)
mkParPat' pat@(L l _) = cL l (gParPat pat)
#else
mkParPat' (dL->L l p) =
  -- This newline is mandatory to support 'NOEXT' CPP macro. Seems like, the C
  -- preprocessor is not working well with view pattern.
  cL l (ParPat NOEXT (cL l p))
#endif
{-# INLINABLE mkParPat' #-}

#if MIN_VERSION_ghc(9,0,0)
mkConPat :: LocatedN (ConLikeP PARSED) -> HsConPatDetails PARSED -> Pat PARSED
mkConPat = ConPat NOEXT
#elif MIN_VERSION_ghc(8,4,0)
mkConPat :: Located (IdP PARSED) -> HsConPatDetails PARSED -> Pat PARSED
mkConPat = ConPatIn
#else
mkConPat :: Located PARSED -> HsConPatDetails PARSED -> Pat PARSED
mkConPat = ConPatIn
#endif

mkNPat' :: Located (HsOverLit PARSED) -> Pat PARSED
#if MIN_VERSION_ghc(9,4,0)
mkNPat' li = mkNPat (reLocA li) Nothing unused
#elif MIN_VERSION_ghc(9,2,0)
mkNPat' li = mkNPat li Nothing unused
#else
mkNPat' li = mkNPat li Nothing
#endif
{-# INLINABLE mkNPat' #-}

#if MIN_VERSION_ghc(9,2,0)
mkPrefixCon :: [a] -> HsConDetails ta a r
mkPrefixCon = PrefixCon []
#else
mkPrefixCon :: [a] -> HsConDetails a r
mkPrefixCon = PrefixCon
#endif
{-# INLINABLE mkPrefixCon #-}

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
