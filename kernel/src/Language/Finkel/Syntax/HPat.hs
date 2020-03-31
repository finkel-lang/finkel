{-# LANGUAGE CPP          #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
-- | Syntax for patterns.
module Language.Finkel.Syntax.HPat where

#include "Syntax.h"

-- ghc
import GHC_Hs_Pat                      (Pat (..))
import Lexer                           (P (..), PState, ParseResult (..))
import RdrHsSyn                        (checkPattern)
import SrcLoc                          (GenLocated (..))

#if MIN_VERSION_ghc(8,10,0)
import RdrHsSyn                        (ecpFromExp, runECP_P)
#else
import Outputable                      (text)
#endif

#if MIN_VERSION_ghc(8,10,0)
import GHC_Hs_Extension                (noExtField)
#elif MIN_VERSION_ghc(8,6,0)
import GHC_Hs_Extension                (noExt)
#endif

#if MIN_VERSION_ghc(8,6,0)
import GHC_Hs_Pat                      (parenthesizePat)
#else
import GHC_Hs_Types                    (HsConDetails (..))
import SrcLoc                          (unLoc)
#endif

-- Internal
import Language.Finkel.Builder
import Language.Finkel.Syntax.SynUtils

#if !MIN_VERSION_ghc(8,6,0)
import Language.Finkel.Syntax.HExpr
#endif

-- ---------------------------------------------------------------------
--
-- Pattern
--
-- ---------------------------------------------------------------------

b_exprToP :: HExpr -> Builder HPat
b_exprToP expr =
  do pstate <- ghcPState <$> getBState
     case checkPattern' expr pstate of
       POk _ a -> return a
       _       -> builderError
{-# INLINE b_exprToP #-}

checkPattern' :: HExpr -> PState -> ParseResult HPat
#if MIN_VERSION_ghc(8,10,0)
checkPattern' expr = unP (runECP_P (ecpFromExp expr) >>= checkPattern)
#else
checkPattern' expr = unP (checkPattern (text "checkPattern'") expr)
#endif

b_lazyP :: HPat -> HPat
b_lazyP (dL->L l pat) = cL l (LazyPat NOEXT (cL l pat))
{-# INLINE b_lazyP #-}

b_bangP :: HPat -> HPat
b_bangP (dL->L l pat) = cL l (BangPat NOEXT (cL l pat))
{-# INLINE b_bangP #-}


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
