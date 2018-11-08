{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
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
import HsExtension (noExt)
#else
import HsExpr (noPostTcExpr)
import PlaceHolder (placeHolderType)
#endif

-- Internal
import Language.SK.Builder
import Language.SK.Form
import Language.SK.Syntax.SynUtils

#include "Syntax.h"

-- ---------------------------------------------------------------------
--
-- Expression
--
-- ---------------------------------------------------------------------

b_ifE :: Code -> HExpr -> HExpr -> HExpr -> HExpr
b_ifE (LForm (L l _)) p t f = L l (mkHsIf p t f)
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

b_recConOrUpdE :: Code -> [(Located FastString,HExpr)] -> Builder HExpr
b_recConOrUpdE sym@(LForm (L l _)) flds
  | isUpper (headFS name)
  = return (L l (mkRdrRecordCon rName cflds))
  | otherwise
  = do v <- b_varE sym
       return (L l (mkRdrRecordUpd v uflds))
  where
    name = symbolNameFS sym
    rName = L l (mkVarRdrName name)
    cflds = HsRecFields { rec_flds = map mkcfld flds
                        , rec_dotdot = Nothing }
    uflds = map mkufld flds
    mkufld  = cfld2ufld . mkcfld
{-# INLINE b_recConOrUpdE #-}

b_recUpdE :: Builder HExpr -> [(Located FastString,HExpr)]
          -> Builder HExpr
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

b_charE :: Code -> Builder HExpr
b_charE (LForm (L l form))
  | (Atom (AChar x)) <- form
  = return (L l (hsLit (HsChar (SourceText (show x)) x)))
  | otherwise
  = builderError
{-# INLINE b_charE #-}

b_stringE :: Code -> Builder HExpr
b_stringE (LForm (L l form))
  | (Atom (AString x)) <- form
  = return
      (L l (hsLit (HsString (SourceText (show x)) (fsLit x))))
  | otherwise
  = builderError
{-# INLINE b_stringE #-}

b_integerE :: Code -> Builder HExpr
b_integerE (LForm (L l form))
  | (Atom (AInteger x)) <- form
  = if x < 0
       then return (L l (hsPar (expr x)))
       else return (expr x)
  | otherwise = builderError
  where
    expr x = L l (hsOverLit $! mkHsIntegral_compat x)
{-# INLINE b_integerE #-}

b_floatE :: Code -> Builder HExpr
b_floatE (LForm (L l form))
  | (Atom (AFractional x)) <- form
  = if fl_value x < 0
       then return (L l (hsPar (expr x)))
       else return (expr x)
  | otherwise
  = builderError
  where
    expr x = L l (hsOverLit $! hsFractional x)
{-# INLINE b_floatE #-}

b_varE :: Code -> Builder HExpr
b_varE (LForm (L l form))
  | Atom (ASymbol x) <- form =
    let rname = mkVarRdrName x
        hsVar = HsVar NOEXT
    in  return (L l (hsVar (L l rname)))
  | otherwise = builderError
{-# INLINE b_varE #-}

b_unitE :: Code -> HExpr
b_unitE (LForm (L l _)) = case mkLHsTupleExpr [] of L _ t -> L l t
{-# INLINE b_unitE #-}

b_commentStringE :: Code -> Builder (Located HsDocString)
b_commentStringE (LForm (L l form))
  | (Atom (AComment x)) <- form = return (L l (hsDocString x))
  | otherwise                   = builderError
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
hsLit :: HsLit PARSED -> HsExpr PARSED
#else
hsLit :: HsLit -> HsExpr PARSED
#endif
hsLit = HsLit NOEXT
{-# INLINE hsLit #-}

hsPar :: HExpr -> HsExpr PARSED
hsPar = HsPar NOEXT
{-# INLINE hsPar #-}

hsOverLit :: HsOverLit PARSED -> HsExpr PARSED
hsOverLit = HsOverLit NOEXT
{-# INLINE hsOverLit #-}

hsFractional :: FractionalLit -> HsOverLit PARSED
#if MIN_VERSION_ghc(8,6,0)
hsFractional = mkHsFractional
#else
hsFractional x = mkHsFractional x placeHolderType
#endif
{-# INLINE hsFractional #-}


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
