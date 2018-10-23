{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- | Syntax for expression.
module Language.SK.Syntax.HExpr where

-- base
import Data.Char (isUpper)
import Data.List (foldl1')

-- ghc
import BasicTypes ( Boxity(..), FractionalLit(..), Origin(..)
                  , SourceText(..), fl_value)
import FastString (FastString, headFS)
import HsExpr ( ArithSeqInfo(..), GRHS(..), HsExpr(..)
              , HsMatchContext(..), HsStmtContext(..), HsTupArg(..)
              , Match(..), StmtLR(..) )
import HsDoc (HsDocString)
import HsLit (HsLit(..), HsOverLit(..))
import HsPat (HsRecFields(..))
import HsUtils ( mkBindStmt, mkBodyStmt, mkHsApp, mkHsDo, mkHsFractional
               , mkHsIf, mkHsLam, mkLHsPar, mkLHsSigWcType, mkLHsTupleExpr
               , mkMatchGroup )
import OrdList (toOL)
import RdrHsSyn ( mkRdrRecordCon, mkRdrRecordUpd )
import SrcLoc (Located, noLoc)

#if MIN_VERSION_ghc(8,6,0)
import HsDoc (mkHsDocString)
import HsExtension (noExt)
#else
import HsDoc (HsDocString(..))
import HsExpr (noPostTcExpr)
import PlaceHolder (placeHolderType)
#endif

-- Internal
import Language.SK.Builder
import Language.SK.Form
import Language.SK.Syntax.SynUtils

-- ---------------------------------------------------------------------
--
-- C macro for GHC
--
-- ---------------------------------------------------------------------

-- From ghc 8.6.0, many number of data type used by the internal AST in
-- GHC were modified to take extension argument. Following `NOEXT' macro
-- will pass the `noExt' argument to such constructors, and behaves as
-- empty code when compiling with GHC version prior to 8.6.0.

#include "Syntax.h"

-- ---------------------------------------------------------------------
--
-- Expression
--
-- ---------------------------------------------------------------------

b_ifE :: Code -> HExpr -> HExpr -> HExpr -> HExpr
b_ifE (LForm (L l (Atom _))) p t f = L l (mkHsIf p t f)
{-# INLINE b_ifE #-}

b_lamE :: (HExpr,[HPat]) -> HExpr
b_lamE (body,pats) = mkHsLam pats body
{-# INLINE b_lamE #-}

b_tupE :: Code -> [HExpr] -> HExpr
b_tupE (LForm (L l _)) args = L l e
  where
    e = explicitTuple (map mkArg args) Boxed
    mkArg x@(L al _) = L al (present x)
    explicitTuple = ExplicitTuple NOEXT
    present = Present NOEXT
{-# INLINE b_tupE #-}

b_letE :: Code -> [HDecl] -> HExpr -> HExpr
b_letE (LForm (L l _)) decls body =
  let (mbs, sigs) = cvBindsAndSigs (toOL decls)
      valbinds = mkHsValBinds_compat mbs sigs
      hsLet = HsLet NOEXT
  in  L l (hsLet (L l valbinds) body)
{-# INLINE b_letE #-}

b_caseE :: Code -> HExpr -> [HMatch] -> HExpr
b_caseE (LForm (L l _)) expr matches = L l (hsCase expr mg)
  where
    hsCase = HsCase NOEXT
    mg = mkMatchGroup FromSource matches
{-# INCLUDE b_caseE #-}

b_match :: HPat -> ([HGRHS],[HDecl]) -> HMatch
b_match pat@(L l _) (grhss,decls) =
#if MIN_VERSION_ghc(8,6,0)
    L l (Match noExt ctxt [pat] grhss')
#elif MIN_VERSION_ghc(8,4,0)
    L l (Match ctxt [pat] grhss')
#else
    L l (Match ctxt [pat] Nothing grhss')
#endif
  where
    grhss' = mkGRHSs grhss decls l
    ctxt = CaseAlt
{-# INLINE b_match #-}

b_hgrhs :: [HGRHS] -> (HExpr, [HGuardLStmt]) -> [HGRHS]
b_hgrhs rhss (body, gs) =
  let lrhs = case gs of
        [] -> noLoc rhs
        _  -> let l = getLoc (mkLocatedList gs) in L l rhs
      rhs = b_GRHS gs body
  in  (lrhs:rhss)
{-# INLINE b_hgrhs #-}

b_GRHS :: [HGuardLStmt] -> HExpr -> GRHS PARSED HExpr
b_GRHS = GRHS NOEXT
{-# INLINE b_GRHS #-}

b_doE :: Code -> [HStmt] -> HExpr
b_doE (LForm (L l _)) exprs = L l (mkHsDo DoExpr exprs)
{-# INLINE b_doE #-}

b_tsigE :: Code -> HExpr -> ([HType], HType) -> HExpr
b_tsigE (LForm (L l _)) e (ctxt,t) =
  let t' = case ctxt of
             [] -> t
             _  -> L l (mkHsQualTy_compat (mkLocatedList ctxt) t)
#if MIN_VERSION_ghc(8,6,0)
      e' = ExprWithTySig (mkLHsSigWcType t') e
#else
      e' = ExprWithTySig e (mkLHsSigWcType t')
#endif
  in  mkLHsPar (L l e')
{-# INLINE b_tsigE #-}

b_recConOrUpdE :: Code -> [(FastString,HExpr)] -> HExpr
b_recConOrUpdE sym@(LForm (L l _)) flds = L l expr
  where
    expr =
      case () of
        _ | isUpper (headFS name) -> mkRdrRecordCon rName cflds
        _ -> mkRdrRecordUpd (b_varE sym) uflds
    name = symbolNameFS sym
    rName = L l (mkVarRdrName name)
    cflds = HsRecFields { rec_flds = map mkcfld flds
                        , rec_dotdot = Nothing }
    uflds = map mkufld flds
    mkufld  = cfld2ufld . mkcfld
{-# INLINE b_recConOrUpdE #-}

b_recUpdE :: Builder HExpr -> [(FastString,HExpr)] -> Builder HExpr
b_recUpdE expr flds = do
   expr' <- expr
   let uflds = map (cfld2ufld . mkcfld) flds
       l = getLoc expr'
   return (L l (mkRdrRecordUpd (mkLHsPar expr') uflds))
{-# INLINE b_recUpdE #-}

b_appE :: [HExpr] -> HExpr
b_appE = foldl1' f
  where
    f a b = mkHsApp a (mkLHsPar b)
{-# INLINE b_appE #-}

b_charE :: Code -> HExpr
b_charE (LForm (L l (Atom (AChar x)))) =
  L l (mkHsLit_compat (HsChar (SourceText (show x)) x))
{-# INLINE b_charE #-}

b_stringE :: Code -> HExpr
b_stringE (LForm (L l (Atom (AString x)))) =
  L l (mkHsLit_compat (HsString (SourceText (show x)) (fsLit x)))
{-# INLINE b_stringE #-}

b_integerE :: Code -> HExpr
b_integerE (LForm (L l (Atom (AInteger x))))
   | x < 0     = L l (mkHsPar_compat expr)
   | otherwise = expr
  where
    expr =     L l (mkHsOverLit_compat $! mkHsIntegral_compat x)
{-# INLINE b_integerE #-}

b_floatE :: Code -> HExpr
b_floatE (LForm (L l (Atom (AFractional x))))
  | fl_value x < 0 = L l (mkHsPar_compat expr)
  | otherwise      = expr
  where
    expr = L l (mkHsOverLit_compat $! mkHsFractional_compat x)
{-# INLINE b_floatE #-}

b_varE :: Code -> HExpr
b_varE (LForm (L l (Atom (ASymbol x)))) = L l (hsVar (L l rname))
  where
    hsVar = HsVar NOEXT
    rname = mkVarRdrName x
{-# INLINE b_varE #-}

b_unitE :: Code -> HExpr
b_unitE (LForm (L l _)) =
  case mkLHsTupleExpr [] of
    L _ tuple -> L l tuple
{-# INLINE b_unitE #-}

b_commentStringE :: Code -> Located HsDocString
b_commentStringE (LForm (L l (Atom (AComment x)))) =
  L l (mkHsDocString_compat x)
{-# INLINE b_commentStringE #-}

b_hsListE :: Either HExpr [HExpr] -> HExpr
b_hsListE expr =
  case expr of
    Right exprs -> L l (ExplicitList xEXPLICITLIST Nothing exprs)
      where
        l = getLoc (mkLocatedList exprs)
#if MIN_VERSION_ghc(8,6,0)
        xEXPLICITLIST = noExt
#else
        xEXPLICITLIST = placeHolderType
#endif
    Left arithSeqExpr -> arithSeqExpr
{-# INLINE b_hsListE #-}

b_arithSeqE :: HExpr -> Maybe HExpr -> Maybe HExpr -> HExpr
b_arithSeqE fromE thenE toE =
#if MIN_VERSION_ghc(8,6,0)
  L l (ArithSeq noExt Nothing info)
#else
  L l (ArithSeq noPostTcExpr Nothing info)
#endif
  where
    info | Just thenE' <- thenE, Just toE' <- toE =
           FromThenTo fromE thenE' toE'
         | Just thenE' <- thenE =
           FromThen fromE thenE'
         | Just toE' <- toE =
           FromTo fromE toE'
         | otherwise = From fromE
    l = getLoc fromE
{-# INLINE b_arithSeqE #-}

#if MIN_VERSION_ghc(8,4,0)
mkHsLit_compat :: HsLit PARSED -> HsExpr PARSED
#else
mkHsLit_compat :: HsLit -> HsExpr PARSED
#endif
mkHsLit_compat = HsLit NOEXT
{-# INLINE mkHsLit_compat #-}

mkHsDocString_compat :: String -> HsDocString
#if MIN_VERSION_ghc(8,6,0)
mkHsDocString_compat = mkHsDocString
#else
mkHsDocString_compat = HsDocString . fsLit
#endif
{-# INLINE mkHsDocString_compat #-}

mkHsPar_compat :: HExpr -> HsExpr PARSED
mkHsPar_compat = HsPar NOEXT
{-# INLINE mkHsPar_compat #-}

mkHsOverLit_compat :: HsOverLit PARSED -> HsExpr PARSED
mkHsOverLit_compat = HsOverLit NOEXT
{-# INLINE mkHsOverLit_compat #-}

mkHsFractional_compat :: FractionalLit -> HsOverLit PARSED
#if MIN_VERSION_ghc(8,6,0)
mkHsFractional_compat = mkHsFractional
#else
mkHsFractional_compat x = mkHsFractional x placeHolderType
#endif
{-# INLINE mkHsFractional_compat #-}


-- ---------------------------------------------------------------------
--
-- Statement
--
-- ---------------------------------------------------------------------

b_bindS :: Code -> HPat -> HExpr -> HStmt
b_bindS (LForm (L l _)) pat expr = L l (mkBindStmt pat expr)
{-# INLINE b_bindS #-}

b_letS :: Code -> [HDecl] -> HStmt
b_letS (LForm (L l _)) decls =
  let (mbs, sigs) = cvBindsAndSigs (toOL decls)
      valbinds = mkHsValBinds_compat mbs sigs
      letStmt = LetStmt NOEXT
  in  L l (letStmt (L l valbinds))
{-# INLINE b_letS #-}

b_bodyS :: HExpr -> HStmt
b_bodyS expr = L (getLoc expr) (mkBodyStmt expr)
{-# INLINE b_bodyS #-}
