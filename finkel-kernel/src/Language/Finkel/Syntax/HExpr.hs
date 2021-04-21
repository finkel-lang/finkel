{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
-- | Syntax for expression.
module Language.Finkel.Syntax.HExpr where

#include "Syntax.h"
#include "ghc_modules.h"

-- base
import Data.List                       (foldl', foldl1')
import Data.Maybe                      (fromMaybe)

-- ghc
import GHC_Builtin_Types               (tupleDataCon)
import GHC_Data_FastString             (FastString, fsLit, headFS, lengthFS,
                                        nullFS, unpackFS)
import GHC_Data_OrdList                (toOL)
import GHC_Hs_Doc                      (HsDocString)
import GHC_Hs_Expr                     (ArithSeqInfo (..), GRHS (..),
                                        HsExpr (..), HsMatchContext (..),
                                        HsStmtContext (..), HsTupArg (..),
                                        Match (..), StmtLR (..))
import GHC_Hs_Lit                      (HsLit (..), HsOverLit (..))
import GHC_Hs_Pat                      (HsRecFields (..))
import GHC_Hs_Type                     (mkHsWildCardBndrs)
import GHC_Hs_Utils                    (mkBodyStmt, mkHsApp, mkHsComp, mkHsDo,
                                        mkHsFractional, mkHsIf, mkHsLam,
                                        mkLHsPar, mkLHsSigWcType,
                                        mkLHsTupleExpr, mkMatchGroup)
import GHC_Parser_PostProcess          (mkRdrRecordCon, mkRdrRecordUpd)
import GHC_Types_Basic                 (Arity, Boxity (..), FractionalLit (..),
                                        Origin (..), SourceText (..))
import GHC_Types_Name_Reader           (RdrName, getRdrName)
import GHC_Types_SrcLoc                (GenLocated (..), Located, SrcSpan (..),
                                        getLoc, noLoc)
import GHC_Utils_Lexeme                (isLexCon, isLexSym, isLexVarId)

#if MIN_VERSION_ghc(9,0,0)
import GHC_Hs_Utils                    (mkPsBindStmt)
import GHC_Types_SrcLoc                (UnhelpfulSpanReason (..))
#else
import GHC_Hs_Utils                    (mkBindStmt)
#endif

#if MIN_VERSION_ghc(8,10,0)
import GHC_Hs_Extension                (noExtField)
#elif MIN_VERSION_ghc(8,6,0)
import GHC_Hs_Extension                (noExt)
#else
import GHC_Hs_Expr                     (isListCompExpr, noPostTcExpr)
import GHC_Hs_Lit                      (OverLitVal (..))
import PlaceHolder                     (placeHolderType)
#endif

#if MIN_VERSION_ghc(8,6,0)
import GHC_Hs_Expr                     (parenthesizeHsExpr)
#endif

-- Internal
import Language.Finkel.Builder
import Language.Finkel.Form
import Language.Finkel.Syntax.SynUtils


-- ---------------------------------------------------------------------
--
-- Expression
--
-- ---------------------------------------------------------------------

b_ifE :: Code -> HExpr -> HExpr -> HExpr -> HExpr
b_ifE (LForm (L l _)) p t f = L l (mkHsIf p t f)
{-# INLINABLE b_ifE #-}

b_lamE :: (HExpr,[HPat]) -> HExpr
b_lamE (body,pats) = mkHsLam pats body
{-# INLINABLE b_lamE #-}

b_tupE :: Code -> [HExpr] -> HExpr
b_tupE (LForm (L l _)) args = L l e
  where
    e = explicitTuple (map mkArg args) Boxed
    mkArg x@(L al _) = L al (present x)
    explicitTuple = ExplicitTuple NOEXT
    present = Present NOEXT
{-# INLINABLE b_tupE #-}

-- Expression for tuple constructor function (i.e. the (,)
-- function). See also 'b_varE' for tuples with more elements.
b_tupConE :: Code -> HExpr
b_tupConE (LForm (L l _)) = L l (HsVar NOEXT (L l (tupConName Boxed 2)))
{-# INLINABLE b_tupConE #-}

b_letE :: Code -> [HDecl] -> HExpr -> Builder HExpr
b_letE (LForm (L l _)) decls body = do
  cd <- cvBindsAndSigs (toOL decls)
  let valbinds = mkHsValBinds_compat (cd_binds cd) (cd_sigs cd)
      hsLet = HsLet NOEXT
  return (L l (hsLet (L l valbinds) body))
{-# INLINABLE b_letE #-}

b_caseE :: Code -> HExpr -> [HMatch] -> HExpr
b_caseE (LForm (L l _)) expr matches = L l (hsCase expr mg)
  where
    hsCase = HsCase NOEXT
    mg = mkMatchGroup FromSource matches
{-# INCLUDE b_caseE #-}

b_match :: HPat -> ([HGRHS],[HDecl]) -> HMatch
b_match pat (grhss,decls) =
#if MIN_VERSION_ghc(8,6,0)
    L l (Match NOEXT ctxt [pat] grhss')
#elif MIN_VERSION_ghc(8,4,0)
    L l (Match ctxt [pat] grhss')
#else
    L l (Match ctxt [pat] Nothing grhss')
#endif
  where
    grhss' = mkGRHSs grhss decls l
    ctxt = CaseAlt
    l = getLoc (dL pat)
{-# INLINABLE b_match #-}

b_hgrhs :: [HGRHS] -> (HExpr, [HGuardLStmt]) -> [HGRHS]
b_hgrhs rhss (body, gs) =
  let lrhs = case gs of
        [] -> noLoc rhs
        _  -> let l = getLoc (mkLocatedList gs) in L l rhs
      rhs = b_GRHS gs body
  in  (lrhs:rhss)
{-# INLINABLE b_hgrhs #-}

b_GRHS :: [HGuardLStmt] -> HExpr -> GRHS PARSED HExpr
b_GRHS = GRHS NOEXT
{-# INLINABLE b_GRHS #-}

b_doE :: Code -> [HStmt] -> HExpr
#if MIN_VERSION_ghc(9,0,0)
-- XXX: Does not support "[ModuleName.].do" syntax.
b_doE (LForm (L l _)) exprs = L l (mkHsDo (DoExpr Nothing) exprs)
#else
b_doE (LForm (L l _)) exprs = L l (mkHsDo DoExpr exprs)
#endif
{-# INLINABLE b_doE #-}

b_tsigE :: Code -> HExpr -> ([HType], HType) -> HExpr
b_tsigE (LForm (L l _)) e0 (ctxt,t) =
  let t' = case ctxt of
             [] -> t
             _  -> L l (mkHsQualTy_compat (mkLocatedList ctxt) t)
#if MIN_VERSION_ghc(8,8,0)
      e1 = ExprWithTySig NOEXT e0 (mkLHsSigWcType t')
#elif MIN_VERSION_ghc(8,6,0)
      e1 = ExprWithTySig (mkLHsSigWcType t') e0
#else
      e1 = ExprWithTySig e0 (mkLHsSigWcType t')
#endif
  in  mkLHsPar (L l e1)
{-# INLINABLE b_tsigE #-}

b_recConOrUpdE :: Code -> [(Located FastString,HExpr)] -> Builder HExpr
b_recConOrUpdE whole@(LForm (L l form)) flds =
  case form of
    Atom (ASymbol name) | isLexCon name
      -> return (L l (mkRdrRecordCon (L l (mkVarRdrName name)) cflds))
    _ -> b_varE whole >>= \v -> return (L l (mkRdrRecordUpd v uflds))
  where
    cflds = HsRecFields { rec_flds = map mkcfld flds
                        , rec_dotdot = Nothing }
    uflds = map mkufld flds
    mkufld  = cfld2ufld . mkcfld
{-# INLINABLE b_recConOrUpdE #-}

b_recUpdE :: Builder HExpr -> [(Located FastString, HExpr)]
          -> Builder HExpr
b_recUpdE expr flds = do
   expr' <- expr
   let uflds = map (cfld2ufld . mkcfld) flds
       l = getLoc expr'
   return (L l (mkRdrRecordUpd (mkLHsPar expr') uflds))
{-# INLINABLE b_recUpdE #-}

b_opOrAppE :: Code -> ([HExpr], [HType]) -> Builder HExpr
b_opOrAppE code (args, tys) = do
  fn <- b_varE code
  let fn' = mkAppTypes fn tys
      mkOp loc lhs rhs = L loc (mkOpApp fn' lhs (mkLHsParOp rhs))
  case code of
    -- Perform operator expansion, or delegate to `b_appE' if the head of the
    -- form was non-operator.
    LForm (L l (Atom (ASymbol name)))
      | let name' = fromMaybe name (snd <$> splitQualName name)
      , isLexSym name'
      , hd:rest@(_:_) <- args
      -> pure (foldl' (mkOp l) (mkLHsParOp hd) rest)
    _ -> pure (b_appE (fn':args, tys))
{-# INLINABLE b_opOrAppE #-}

mkLHsParOp :: HExpr -> HExpr
mkLHsParOp = parenthesizeHsExpr' opPrec
{-# INLINABLE mkLHsParOp #-}

mkOpApp :: HExpr -> HExpr -> HExpr -> HsExpr PARSED
mkOpApp op l =
#if MIN_VERSION_ghc(8,6,0)
  OpApp NOEXT l op
#else
  OpApp l op placeHolderType
#endif
{-# INLINABLE mkOpApp #-}

b_appE :: ([HExpr], [HType]) -> HExpr
b_appE (args,_tys) = foldl1' f args
  where
    f a b = mkHsApp a (mkLHsPar b)
{-# INLINABLE b_appE #-}

mkAppTypes :: HExpr -> [HType] -> HExpr
mkAppTypes = foldl' mkAppType
{-# INLINABLE mkAppTypes #-}

mkAppType :: HExpr -> HType -> HExpr
mkAppType (dL->expr@(L l _)) ty =
#if MIN_VERSION_ghc(8,8,0)
  cL l (HsAppType NOEXT expr (mkHsWildCardBndrs ty))
#elif MIN_VERSION_ghc(8,6,0)
  cL l (HsAppType (mkHsWildCardBndrs ty) expr)
#else
  cL l (HsAppType expr (mkHsWildCardBndrs ty))
#endif

b_charE :: Code -> Builder HExpr
b_charE (LForm (L l form)) =
  case form of
    Atom (AChar st x) -> return (L l (hsLit (HsChar st x)))
    _                 -> builderError
{-# INLINABLE b_charE #-}

b_stringE :: Code -> Builder HExpr
b_stringE (LForm (L l form)) =
  case form of
    Atom (AString st x) -> return (L l (hsLit (HsString st x)))
    _                   -> builderError
{-# INLINABLE b_stringE #-}

b_integerE :: Code -> Builder HExpr
b_integerE (LForm (L l form)) =
  case form of
    Atom (AInteger x)
      | il_value x < 0 -> return (L l (hsPar (expr x)))
      | otherwise      -> return (expr x)
    _                  -> builderError
  where
    expr x = L l (hsOverLit $! mkHsIntegral_compat x)
{-# INLINABLE b_integerE #-}

b_fracE :: Code -> Builder HExpr
b_fracE (LForm (L l form)) =
  case form of
    Atom (AFractional x)
      | fl_value x < 0 -> return (L l (hsPar (expr x)))
      | otherwise      -> return (expr x)
    _                  -> builderError
  where
    expr x = L l (hsOverLit $! hsFractional x)
{-# INLINABLE b_fracE #-}

b_varE :: Code -> Builder HExpr
b_varE (LForm (L l form))
  | Atom (ASymbol x) <- form
  , not (nullFS x)
  , let hdchr = headFS x
  , let tlchrs = tailFS x
  = case hdchr of
      -- Overloaded label starts with `#'. Tail characters need to be a valid
      -- variable identifier.
      '#' | isLexVarId tlchrs
          -> ret (HsOverLabel NOEXT Nothing tlchrs)

      -- Tuple constructor function with more than two elements are written as
      -- symbol with sequence of commas, handling such case in this function.
      ',' | all (== ',') (unpackFS tlchrs)
          -> ret (var (tupConName Boxed (lengthFS x + 1)))

      -- Plain variable identifier.
      _   -> ret (var (mkVarRdrName x))
  | otherwise = builderError
  where
    ret e = return (cL l e)
    var n = HsVar NOEXT (cL l n)
{-# INLINABLE b_varE #-}

b_unitE :: Code -> HExpr
b_unitE (LForm (L l _)) = case mkLHsTupleExpr [] of L _ t -> L l t
{-# INLINABLE b_unitE #-}

b_docString :: Code -> Builder (Located HsDocString)
b_docString (LForm (L l form)) =
  case form of
    Atom (AString _ x) -> return $! L l (hsDocString x)
    _                  -> builderError
{-# INLINABLE b_docString #-}

b_hsListE :: Either HExpr [HExpr] -> HExpr
b_hsListE expr =
  case expr of
    Right exprs -> L l (ExplicitList xEXPLICITLIST Nothing exprs)
      where
        l = getLoc (mkLocatedList exprs)
#if MIN_VERSION_ghc(8,6,0)
        xEXPLICITLIST = NOEXT
#else
        xEXPLICITLIST = placeHolderType
#endif
    Left arithSeqExpr -> arithSeqExpr
{-# INLINABLE b_hsListE #-}

b_lcompE :: HExpr -> [HStmt] -> HExpr
b_lcompE ret stmts = L l (mkHsComp ListComp stmts ret)
  where l = getLoc ret
{-# INLINABLE b_lcompE #-}

b_arithSeqE :: HExpr -> Maybe HExpr -> Maybe HExpr -> HExpr
b_arithSeqE fromE thenE toE =
#if MIN_VERSION_ghc(8,6,0)
  L l (ArithSeq NOEXT Nothing info)
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
{-# INLINABLE b_arithSeqE #-}

b_quoteE :: Code -> Builder HExpr
b_quoteE (LForm (L l form)) = do
  qualify <- fmap qualifyQuote getBState
  case form of
    Atom atom -> b_quoteAtomE l qualify atom
    List xs   -> b_quoteLocListE l (qListS qualify) xs
    HsList xs -> b_quoteLocListE l (qHsListS qualify) xs
    _         -> builderError
{-# INLINABLE b_quoteE #-}

b_quoteAtomE :: SrcSpan -> Bool -> Atom -> Builder HExpr
b_quoteAtomE l qualify atom =
  case atom of
    ASymbol s       -> mk_lapp qSymbolS (mk_sym s)
    AChar st c      -> mk_lapp qCharS (L l (hsLit (HsChar st c)))
    AString st str  -> mk_lapp qStringS (mk_str st str)
    AInteger _il    -> b_integerE orig >>= mk_lapp qIntegerS
    AFractional _fl -> b_fracE orig >>= mk_lapp qFractionalS
    AUnit           -> mk_unit
  where
    mk_sym s = L l (hsLit (HsString (SourceText (show s)) s))
    mk_str st str = L l (hsLit (HsString st str))
    orig = LForm (L l (Atom atom))
    mk_lapp lname arg = do
      let (fname, sl, sc, el, ec) = getLocInfo l
      fn <- b_varE (LForm (L l (Atom (ASymbol (lname qualify)))))
      return (b_appE ([fn, arg, fname, sl, sc, el, ec], []))
    mk_unit = do
      let (fname, sl, sc, el, ec) = getLocInfo l
      fn <- b_varE (LForm (L l (Atom (ASymbol (qUnitS qualify)))))
      return (b_appE ([fn, fname, sl, sc, el, ec], []))
{-# INLINABLE b_quoteAtomE #-}

b_quoteLocListE :: SrcSpan -> FastString -> [Code] -> Builder HExpr
b_quoteLocListE l fn_name xs = do
  mk_list <- b_varE (LForm (L l (Atom (ASymbol fn_name))))
  args <- fmap (b_hsListE . Right) (mapM b_quoteE xs)
  let (fname, sl, sc, el, ec) = getLocInfo l
  return (b_appE ([mk_list, args, fname, sl, sc, el, ec], []))
{-# INLINABLE b_quoteLocListE #-}

getLocInfo :: SrcSpan -> (HExpr, HExpr, HExpr, HExpr, HExpr)
getLocInfo l = withLocInfo l fname mk_int
  where
    -- Using unhelpful location for file names, lines, and columns. Otherwise,
    -- hpc code coverage will mark the location information as non-evaluated
    -- expressions.
    fname fs = L ql (hsLit (HsString (SourceText (show fs)) fs))
    mk_int n = L ql $! hsOverLit $! mkHsIntegral_compat $! mkIntegralLit n
#if MIN_VERSION_ghc(9,0,0)
    ql = UnhelpfulSpan (UnhelpfulOther (fsLit "<b_quoteE>"))
#else
    ql = UnhelpfulSpan (fsLit "<b_quoteE>")
#endif
{-# INLINABLE getLocInfo #-}


-- ------------------------------------------------------------------------
--
-- Auxiliary
--
-- ------------------------------------------------------------------------

#if MIN_VERSION_ghc(8,4,0)
hsLit :: HsLit PARSED -> HsExpr PARSED
#else
hsLit :: HsLit -> HsExpr PARSED
#endif
hsLit = HsLit NOEXT
{-# INLINABLE hsLit #-}

hsPar :: HExpr -> HsExpr PARSED
hsPar = HsPar NOEXT
{-# INLINABLE hsPar #-}

hsOverLit :: HsOverLit PARSED -> HsExpr PARSED
hsOverLit = HsOverLit NOEXT
{-# INLINABLE hsOverLit #-}

hsFractional :: FractionalLit -> HsOverLit PARSED
#if MIN_VERSION_ghc(8,6,0)
hsFractional = mkHsFractional
#else
hsFractional x = mkHsFractional x placeHolderType
#endif
{-# INLINABLE hsFractional #-}

tupConName :: Boxity -> Arity -> RdrName
tupConName boxity arity = getRdrName (tupleDataCon boxity arity)
{-# INLINABLE tupConName #-}

-- ---------------------------------------------------------------------
--
-- Statement
--
-- ---------------------------------------------------------------------

b_bindS :: Code -> HPat -> HExpr -> HStmt
#if MIN_VERSION_ghc(9,0,0)
b_bindS (LForm (L l _)) pat expr = L l (mkPsBindStmt pat expr)
#else
b_bindS (LForm (L l _)) pat expr = L l (mkBindStmt pat expr)
#endif
{-# INLINABLE b_bindS #-}

b_letS :: Code -> [HDecl] -> Builder HStmt
b_letS (LForm (L l _)) decls = do
  cd <- cvBindsAndSigs (toOL decls)
  let valbinds = mkHsValBinds_compat (cd_binds cd) (cd_sigs cd)
      letStmt = LetStmt NOEXT
  return (L l (letStmt (L l valbinds)))
{-# INLINABLE b_letS #-}

b_bodyS :: HExpr -> HStmt
b_bodyS expr = L (getLoc expr) (mkBodyStmt expr)
{-# INLINABLE b_bodyS #-}


-- ------------------------------------------------------------------------
--
-- Parenthesizing
--
-- ------------------------------------------------------------------------

-- Note: [Parenthesizing HsExpr for patterns]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Following "parenthesizeHsExpr'" is almost same as 'parenthesizeHsExpr' found
-- in the source code of ghc 8.6.x and above, but will add parentheses for
-- ELazyPat when given 'PprPrec' is equal or greater than 'appPrec'. This is to
-- support lazy constructor patterns (e.g.: ~(Just n)) inside 'as' pattern.
--
-- For instance, below codes:
--
--   (@ foo (~(Just n))) ;  Finkel
--
--   foo@(~(Just n))     -- Haskell
--
-- will fail to parse in Haskell when the "~(Just n)" is not surrounded by
-- parentheses.

#if MIN_VERSION_ghc(8,10,0)

parenthesizeHsExpr' :: PprPrec -> HExpr -> HExpr
parenthesizeHsExpr' = parenthesizeHsExpr

#elif MIN_VERSION_ghc(8,6,0)

parenthesizeHsExpr' :: PprPrec -> HExpr -> HExpr
parenthesizeHsExpr' p le@(dL->L loc e) =
  case e of
    ELazyPat{} | p >= appPrec -> L loc (HsPar NOEXT le)
    _                         -> parenthesizeHsExpr p le

#else

-- Following 'parenthesizeHsExpre' and 'hsExprNeedsParens' are backported from
-- "compiler/hsSyn/HsExpr.hs" in ghc source code.

-- | @'parenthesizeHsExpr' p e@ checks if @'hsExprNeedsParens' p e@ is true,
-- and if so, surrounds @e@ with an 'HsPar'. Otherwise, it simply returns @e@.
parenthesizeHsExpr' :: PprPrec -> HExpr -> HExpr
parenthesizeHsExpr' p le@(dL->L loc e)
  | hsExprNeedsParens p e = L loc (HsPar NOEXT le)
  | otherwise             = le

-- | @'hsExprNeedsParens' p e@ returns 'True' if the expression @e@ needs
-- parentheses under precedence @p@.
hsExprNeedsParens :: PprPrec -> HsExpr p -> Bool
hsExprNeedsParens p = go
  where
    go (HsVar{})                         = False
    go (HsUnboundVar{})                  = False
    go (HsConLikeOut{})                  = False
    go (HsIPVar{})                       = False
    go (HsOverLabel{})                   = False
    go (HsLit _EXT l)                    = hsLitNeedsParens p l
    go (HsOverLit _EXT ol)               = hsOverLitNeedsParens p ol
    go (HsPar{})                         = False
    go (HsCoreAnn _EXT _ _ (L _ e))      = go e
    go (HsApp{})                         = p >= appPrec
    go (HsAppType {})                    = p >= appPrec
    go (OpApp{})                         = p >= opPrec
    go (NegApp{})                        = p > topPrec
    go (SectionL{})                      = True
    go (SectionR{})                      = True
    go (ExplicitTuple{})                 = False
    go (ExplicitSum{})                   = False
    go (HsLam{})                         = p > topPrec
    go (HsLamCase{})                     = p > topPrec
    go (HsCase{})                        = p > topPrec
    go (HsIf{})                          = p > topPrec
    go (HsMultiIf{})                     = p > topPrec
    go (HsLet{})                         = p > topPrec
    go (HsDo sc _ _)
      | isListCompExpr sc                = False
      | otherwise                        = p > topPrec
    go (ExplicitList{})                  = False
    go (RecordUpd{})                     = False
    go (ExprWithTySig{})                 = p > topPrec
    go (ArithSeq{})                      = False
    go (EWildPat{})                      = False
    -- Adding parentheses to ELazyPatt when p >= appPrec
    -- go (ELazyPat{})                      = False
    go (ELazyPat {})                     = p >= appPrec
    go (EAsPat{})                        = False
    go (EViewPat{})                      = True
    go (HsSCC{})                         = p >= appPrec
    go (HsWrap _ e)                      = go e
    go (HsSpliceE{})                     = False
    go (HsBracket{})                     = False
    go (HsRnBracketOut{})                = False
    go (HsTcBracketOut{})                = False
    go (HsProc{})                        = p > topPrec
    go (HsStatic{})                      = p >= appPrec
    go (HsTick _EXT _ (L _ e))           = go e
    go (HsBinTick _EXT _ _ (L _ e))      = go e
    go (HsTickPragma _EXT _ _ _ (L _ e)) = go e
    go (HsArrApp{})                      = True
    go (HsArrForm{})                     = True
    go (RecordCon{})                     = False
    go (HsRecFld{})                      = False
    go _                                 = False

-- Following 'hsLitNeedsParens' and 'hsOverLitNeedsParens' are backported from
-- "compiler/hsSyn/HsLit.hs" in ghc source code. Using CPP macros for ghc 8.2.x
-- and 8.4.x compatibility.
--
-- + 'HsLit' type takes argument in ghc 8.4.x but not in 8.2.x.
--
-- + Field types and arity of constructors changed.

fl_neg' :: FractionalLit -> Bool

#if MIN_VERSION_ghc(8,4,0)
#define _XH  _
#define _ST {- st -}
fl_neg' = fl_neg
il_neg' :: IntegralLit -> Bool
il_neg' = il_neg
type HSLIT x = HsLit x
#else
#define _XH  {- xh -}
#define _ST _
fl_neg' fl = fl_value fl < 0
il_neg' :: Integer -> Bool
il_neg' n = n < 0
type HSLIT x = HsLit
#endif

-- | @'hsLitNeedsParens' p l@ returns 'True' if a literal @l@ needs
-- to be parenthesized under precedence @p@.
hsLitNeedsParens :: PprPrec -> HSLIT x -> Bool
hsLitNeedsParens p = go
  where
    go (HsChar {})          = False
    go (HsCharPrim {})      = False
    go (HsString {})        = False
    go (HsStringPrim {})    = False
    go (HsInt _ x)          = p > topPrec && il_neg' x
    go (HsIntPrim _ x)      = p > topPrec && x < 0
    go (HsWordPrim {})      = False
    go (HsInt64Prim _ x)    = p > topPrec && x < 0
    go (HsWord64Prim {})    = False
    go (HsInteger _ x _)    = p > topPrec && x < 0
    go (HsRat _XH x _)      = p > topPrec && fl_neg' x
    go (HsFloatPrim _XH x)  = p > topPrec && fl_neg' x
    go (HsDoublePrim _XH x) = p > topPrec && fl_neg' x

-- | @'hsOverLitNeedsParens' p ol@ returns 'True' if an overloaded literal
-- @ol@ needs to be parenthesized under precedence @p@.
hsOverLitNeedsParens :: PprPrec -> HsOverLit x -> Bool
hsOverLitNeedsParens p (OverLit { ol_val = olv }) = go olv
  where
    go :: OverLitVal -> Bool
    go (HsIntegral _ST x) = p > topPrec && il_neg' x
    go (HsFractional x)   = p > topPrec && fl_neg' x
    go (HsIsString {})    = False
#endif
