{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
-- | Syntax for expression.
module Language.SK.Syntax.HExpr where

-- base
import Data.List (foldl', foldl1')

-- ghc
import BasicTypes ( Arity, Boxity(..), FractionalLit(..), Origin(..)
                  , SourceText(..), fl_value)
import FastString (FastString, lengthFS, unpackFS)
import HsExpr ( ArithSeqInfo(..), GRHS(..), HsExpr(..)
              , HsMatchContext(..), HsStmtContext(..), HsTupArg(..)
              , Match(..), StmtLR(..) )
import HsDoc (HsDocString)
import HsLit (HsLit(..), HsOverLit(..))
import HsPat (HsRecFields(..))
import HsTypes (mkHsWildCardBndrs)
import HsUtils ( mkBindStmt, mkBodyStmt, mkHsApp, mkHsComp
               , mkHsDo, mkHsFractional, mkHsIf, mkHsLam, mkLHsPar
               , mkLHsSigWcType, mkLHsTupleExpr, mkMatchGroup )
import Lexeme (isLexCon, isLexSym)
import OrdList (toOL)
import RdrHsSyn ( mkRdrRecordCon, mkRdrRecordUpd )
import RdrName ( RdrName, getRdrName )
import SrcLoc (GenLocated(..), Located, getLoc, noLoc)
import TysWiredIn ( tupleDataCon )

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

-- Expression for tuple constructor function (i.e. the (,)
-- function). See also 'b_varE' for tuples with more elements.
b_tupConE :: Code -> HExpr
b_tupConE (LForm (L l _)) = L l (HsVar NOEXT (L l (tupConName Boxed 2)))
{-# INLINE b_tupConE #-}

b_letE :: Code -> [HDecl] -> HExpr -> Builder HExpr
b_letE (LForm (L l _)) decls body = do
  cd <- cvBindsAndSigs (toOL decls)
  let valbinds = mkHsValBinds_compat (cd_binds cd) (cd_sigs cd)
      hsLet = HsLet NOEXT
  return (L l (hsLet (L l valbinds) body))
{-# INLINE b_letE #-}

b_caseE :: Code -> HExpr -> [HMatch] -> HExpr
b_caseE (LForm (L l _)) expr matches = L l (hsCase expr mg)
  where
    hsCase = HsCase NOEXT
    mg = mkMatchGroup FromSource matches
{-# INCLUDE b_caseE #-}

b_match :: HPat -> ([HGRHS],[HDecl]) -> HMatch
b_match pat (grhss,decls) =
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
    l = getLoc (dL pat)
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
#if MIN_VERSION_ghc (8,8,0)
      e' = ExprWithTySig noExt e (mkLHsSigWcType t')
#elif MIN_VERSION_ghc(8,6,0)
      e' = ExprWithTySig (mkLHsSigWcType t') e
#else
      e' = ExprWithTySig e (mkLHsSigWcType t')
#endif
  in  mkLHsPar (L l e')
{-# INLINE b_tsigE #-}

b_recConOrUpdE :: Code -> [(Located FastString,HExpr)] -> Builder HExpr
b_recConOrUpdE whole@(LForm (L l form)) flds
  | Atom (ASymbol name) <- form
  , isLexCon name
  = return (L l (mkRdrRecordCon (L l (mkVarRdrName name)) cflds))
  | otherwise
  = do v <- b_varE whole
       return (L l (mkRdrRecordUpd v uflds))
  where
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

b_opOrAppE :: Code -> ([HExpr], [HType]) -> Builder HExpr
b_opOrAppE code (args, tys) = do
  fn <- b_varE code
  let fn' = mkAppTypes fn tys
      mkOp loc lhs rhs = L loc (mkOpApp fn' lhs rhs)
  case code of
    LForm (L l (Atom (ASymbol name)))
      | isLexSym name
      , _:_:_ <- args
      -> pure (mkLHsPar (foldl1' (mkOp l) args))
    _ -> pure (b_appE (fn':args, tys))
{-# INLINE b_opOrAppE #-}

mkOpApp :: HExpr -> HExpr -> HExpr -> HsExpr PARSED
mkOpApp op l r =
#if MIN_VERSION_ghc(8,6,0)
  OpApp NOEXT l op r
#else
  OpApp l op placeHolderType r
#endif
{-# INLINE mkOpApp #-}

b_appE :: ([HExpr], [HType]) -> HExpr
b_appE (args,_tys) = foldl1' f args
  where
    f a b = mkHsApp a (mkLHsPar b)
{-# INLINE b_appE #-}

mkAppTypes :: HExpr -> [HType] -> HExpr
mkAppTypes = foldl' mkAppType
{-# INLINE mkAppTypes #-}

mkAppType :: HExpr -> HType -> HExpr
mkAppType expr ty =
  let l = getLoc expr
#if MIN_VERSION_ghc (8,8,0)
  in  cL l (HsAppType NOEXT expr (mkHsWildCardBndrs ty))
#elif MIN_VERSION_ghc (8,6,0)
  in  cL l (HsAppType (mkHsWildCardBndrs ty) expr)
#else
  in  cL l (HsAppType expr (mkHsWildCardBndrs ty))
#endif
{-# INLINE mkAppType #-}

b_charE :: Code -> Builder HExpr
b_charE (LForm (L l form))
  | Atom (AChar x) <- form
  = return (L l (hsLit (HsChar (SourceText (show x)) x)))
  | otherwise
  = builderError
{-# INLINE b_charE #-}

b_stringE :: Code -> Builder HExpr
b_stringE (LForm (L l form))
  | Atom (AString x) <- form
  = return
      (L l (hsLit (HsString (SourceText (show x)) x)))
  | otherwise
  = builderError
{-# INLINE b_stringE #-}

b_integerE :: Code -> Builder HExpr
b_integerE (LForm (L l form))
  | Atom (AInteger x) <- form
  = if x < 0
       then return (L l (hsPar (expr x)))
       else return (expr x)
  | otherwise = builderError
  where
    expr x = L l (hsOverLit $! mkHsIntegral_compat x)
{-# INLINE b_integerE #-}

b_floatE :: Code -> Builder HExpr
b_floatE (LForm (L l form))
  | Atom (AFractional x) <- form
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
    -- Tuple constructor function with more than two elements are
    -- written as symbol with sequence of commas, handling such case in
    -- this function.
    let rname | all (== ',') (unpackFS x) =
                tupConName Boxed (lengthFS x + 1)
              | otherwise = mkVarRdrName x
        hsVar = HsVar NOEXT
    in  return (L l (hsVar (L l rname)))
  | otherwise = builderError
{-# INLINE b_varE #-}

b_unitE :: Code -> HExpr
b_unitE (LForm (L l _)) = case mkLHsTupleExpr [] of L _ t -> L l t
{-# INLINE b_unitE #-}

b_docString :: Code -> Builder (Located HsDocString)
b_docString (LForm (L l form))
  | Atom (AString x) <- form = return $! L l (hsDocString x)
  | otherwise                = builderError
{-# INLINE b_docString #-}

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

b_lcompE :: HExpr -> [HStmt] -> HExpr
b_lcompE ret stmts = L l (mkHsComp ListComp stmts ret)
  where l = getLoc ret
{-# INLINE b_lcompE #-}

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

tupConName :: Boxity -> Arity -> RdrName
tupConName boxity arity = getRdrName (tupleDataCon boxity arity)
{-# INLINE tupConName #-}

-- ---------------------------------------------------------------------
--
-- Statement
--
-- ---------------------------------------------------------------------

b_bindS :: Code -> HPat -> HExpr -> HStmt
b_bindS (LForm (L l _)) pat expr = L l (mkBindStmt pat expr)
{-# INLINE b_bindS #-}

b_letS :: Code -> [HDecl] -> Builder HStmt
b_letS (LForm (L l _)) decls = do
  cd <- cvBindsAndSigs (toOL decls)
  let valbinds = mkHsValBinds_compat (cd_binds cd) (cd_sigs cd)
      letStmt = LetStmt NOEXT
  return (L l (letStmt (L l valbinds)))
{-# INLINE b_letS #-}

b_bodyS :: HExpr -> HStmt
b_bodyS expr = L (getLoc expr) (mkBodyStmt expr)
{-# INLINE b_bodyS #-}
