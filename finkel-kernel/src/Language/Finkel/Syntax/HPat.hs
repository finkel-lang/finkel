{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE ViewPatterns     #-}

-- | Syntax for patterns.
module Language.Finkel.Syntax.HPat where

#include "ghc_modules.h"

-- base
import Data.Either                     (partitionEithers)
#if !MIN_VERSION_base(4,20,0)
import Data.List                       (foldl')
#endif

-- ghc
import GHC_Hs_Lit                      (HsLit (..), HsOverLit)
import GHC_Hs_Pat                      (HsConPatDetails, HsRecFields (..),
                                        Pat (..), parenthesizePat)
import GHC_Hs_Type                     (HsConDetails (..))
import GHC_Hs_Utils                    (mkHsIntegral, mkHsIsString, mkNPat,
                                        nlWildPat)
import GHC_Types_Basic                 (Boxity (..), appPrec, opPrec)
import GHC_Types_SrcLoc                (GenLocated (..), Located)
import GHC_Utils_Lexeme                (isLexCon, isLexConId, isLexConSym,
                                        isLexSym)

#if !MIN_VERSION_ghc(9,10,0) && MIN_VERSION_ghc(9,6,0)
import GHC.Hs.Extension                (noHsTok)
#endif

#if MIN_VERSION_ghc(9,6,0)
import GHC.Hs.Pat                      (RecFieldsDotDot (..))
#endif

#if MIN_VERSION_ghc(9,4,0)
import GHC.Hs.Pat                      (gParPat)
#endif

#if MIN_VERSION_ghc(9,0,0)
import GHC_Hs_Pat                      (ConLikeP)
#else
import GHC_Hs_Extension                (IdP)
#endif

#if MIN_VERSION_ghc(9,0,0)
import GHC_Hs_Type                     (mkHsPatSigType)
#else
import GHC_Hs_Utils                    (mkLHsSigWcType)
#endif

-- Internal
import Language.Finkel.Builder
import Language.Finkel.Data.FastString (nullFS, unconsFS)
import Language.Finkel.Form
import Language.Finkel.Syntax.Utils


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
    npat n = mkNPat' (L l (mkHsIntegral n))
{-# INLINABLE b_intP #-}

b_stringP :: Code -> Builder HPat
b_stringP (LForm (L l form)) =
  case form of
    Atom (AString stxt str) -> return $! lA l (npat stxt str)
    _                       -> builderError
  where
    npat stxt str = mkNPat' (L l (mkHsIsString stxt str))
{-# INLINABLE b_stringP #-}

b_charP :: Code -> Builder HPat
b_charP (LForm (L l form)) =
  case form of
    Atom (AChar stxt c) -> return $! lA l (LitPat unused (HsChar stxt c))
    _                   -> builderError
{-# INLINABLE b_charP #-}

b_unitP :: Code -> Builder HPat
b_unitP (LForm (L l form)) =
  case form of
    Atom AUnit -> return $! lA l (mkTuplePat [])
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
  , Just (hdchr,tlchrs) <- unconsFS name
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
                              return (lA l (VarPat unused name'))
                      -- Lazy pattern.
                      else do checkVarId orig tlchrs
                              let name' = lN l (mkRdrName tlchrs)
                                  pat = lA l (VarPat unused name')
                              return (lA l (LazyPat unused pat))
        | hdchr == '!'
        , not (nullFS tlchrs)
        , not (isLexSym tlchrs)
        -- Bang pattern.
        -> do let pat = lA l (VarPat unused (lN l (mkRdrName tlchrs)))
              checkVarId orig tlchrs
              return (lA l (BangPat unused pat))
        | otherwise
        -- Varid.
        -> do checkVarId orig name
              return (lA l (VarPat unused (lN l (mkRdrName name))))
  | otherwise = builderError
{-# INLINABLE b_symP #-}

b_hsListP :: [HPat] -> HPat
b_hsListP pats = p
  where
     p = case dL (mkLocatedListA pats) of L l _ -> L l (listPat pats)
     listPat = ListPat unused
{-# INLINABLE b_hsListP #-}

b_labeledP :: Code -> [PreRecField HPat] -> Builder HPat
b_labeledP (LForm (L l form)) ps
  | Atom (ASymbol name) <- form
  , isLexCon name = do
    let mkcfld' (lab, mb_p) =
          case mb_p of
            Just p  -> mkcfld False (lab, p)
            Nothing -> mkcfld True (lab, punned)
        punned = lA l (VarPat unused (lN l punRDR))
        (wilds, non_wilds) = partitionEithers ps
        mb_dotdot = case wilds of
          []                  -> Nothing
#if MIN_VERSION_ghc(9,10,0)
          (LForm (L wl _): _) -> Just (la2la
                                     (L wl (RecFieldsDotDot (length non_wilds))))
#elif MIN_VERSION_ghc(9,6,0)
          (LForm (L wl _): _) -> Just (L wl (RecFieldsDotDot (length non_wilds)))
#else
          (LForm (L wl _): _) -> Just (L wl (length non_wilds))
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
b_tupP (LForm (L l _)) ps = lA l (mkTuplePat ps)
{-# INLINABLE b_tupP #-}

b_asP :: Code -> HPat -> Builder HPat
b_asP (LForm (dL->L l form)) pat =
  case form of
    Atom (ASymbol name) ->
      return $! lA l (asPat (lN l (mkRdrName name)) (mkParPat' pat))
    _ -> builderError
  where
#if MIN_VERSION_ghc(9,10,0)
    asPat lid p = AsPat unused lid p
#elif MIN_VERSION_ghc(9,6,0)
    asPat lid p = AsPat unused lid noHsTok p
#else
    asPat = AsPat unused
#endif
{-# INLINABLE b_asP #-}

b_lazyP :: HPat -> HPat
b_lazyP (dL-> L l pat0) = cL l (LazyPat unused pat1)
  where
    pat1 = parenthesizePat appPrec (cL l pat0)
{-# INLINABLE b_lazyP #-}

b_bangP :: HPat -> HPat
b_bangP (dL->L l pat) = cL l (BangPat unused (cL l pat))
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
        prest = map (parenthesizePat appPrec) rest
        infixPat =
          case rest of
            (hd:rest') ->
              let f lh rh = lA l (mkConPat lrname (InfixCon lh (paren rh)))
                  paren = parenthesizePat opPrec
              in  return (foldl' f (parenthesizePat opPrec hd) rest')
            _ -> builderError
    _ -> builderError
{-# INLINABLE b_conP #-}

b_sigP :: Code -> HPat -> HType -> HPat
b_sigP (LForm (L l _)) pat ty =
#if MIN_VERSION_ghc(9,2,0)
  lA l (SigPat unused pat (mkHsPatSigType unused ty))
#elif MIN_VERSION_ghc(9,0,0)
  lA l (SigPat unused pat (mkHsPatSigType ty))
#else
  cL l (SigPat unused pat (mkLHsSigWcType ty))
#endif
{-# INLINABLE b_sigP #-}

mkTuplePat :: [HPat] -> Pat PARSED
mkTuplePat ps = TuplePat unused ps Boxed
{-# INLINABLE mkTuplePat #-}

-- XXX: Consider using GHC.Hs.Utils.mkParPat
mkParPat' :: HPat -> HPat
#if MIN_VERSION_ghc(9,4,0)
mkParPat' pat@(L l _) = cL l (gParPat pat)
#else
mkParPat' (dL->L l p) =
  -- This newline is mandatory to support 'unused' CPP macro. Seems like, the C
  -- preprocessor is not working well with view pattern.
  cL l (ParPat unused (cL l p))
#endif
{-# INLINABLE mkParPat' #-}

#if MIN_VERSION_ghc(9,0,0)
mkConPat :: LocatedN (ConLikeP PARSED) -> HsConPatDetails PARSED -> Pat PARSED
mkConPat = ConPat unused
#else
mkConPat :: Located (IdP PARSED) -> HsConPatDetails PARSED -> Pat PARSED
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
