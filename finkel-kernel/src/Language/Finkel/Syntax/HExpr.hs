{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

-- | Syntax for expression.
module Language.Finkel.Syntax.HExpr where

#include "Syntax.h"
#include "ghc_modules.h"

-- base
import Control.Arrow                     (first, second)
import Data.Either                       (partitionEithers)
import Data.List                         (foldl1')

#if !MIN_VERSION_base(4,20,0)
import Data.List                         (foldl')
#endif

-- ghc
import GHC_Builtin_Types                 (tupleDataCon)
import GHC_Data_OrdList                  (toOL)
import GHC_Hs_Doc                        (HsDocString)
import GHC_Hs_Expr                       (ArithSeqInfo (..), GRHS (..),
                                          HsExpr (..), HsMatchContext (..),
                                          HsTupArg (..), Match (..),
                                          StmtLR (..))
import GHC_Hs_Lit                        (HsLit (..), HsOverLit (..))
import GHC_Hs_Pat                        (HsRecFields (..), LHsRecField)
import GHC_Hs_Type                       (mkHsWildCardBndrs)
import GHC_Hs_Utils                      (mkBodyStmt, mkHsApp, mkHsComp, mkHsDo,
                                          mkHsFractional, mkHsIf, mkHsIntegral,
                                          mkLHsPar, mkLHsTupleExpr,
                                          mkMatchGroup)
import GHC_Parser_PostProcess            (mkRdrRecordCon)
import GHC_Types_Basic                   (Arity, Boxity (..), Origin (..),
                                          opPrec)
import GHC_Types_Name_Reader             (RdrName, getRdrName)
import GHC_Types_SrcLoc                  (GenLocated (..), Located,
                                          SrcSpan (..), getLoc, noLoc)
import GHC_Utils_Lexeme                  (isLexCon, isLexSym, isLexVarId)

#if MIN_VERSION_ghc(9,10,0)
import Language.Haskell.Syntax.Expr      (HsLamVariant (..))
#elif MIN_VERSION_ghc(9,6,0)
import GHC.Hs.Extension                  (noHsTok)
#endif

#if !MIN_VERSION_ghc(9,10,0) && MIN_VERSION_ghc(9,6,0)
import Language.Haskell.Syntax.Concrete  (HsToken (..))
#elif !MIN_VERSION_ghc(9,10,0) && MIN_VERSION_ghc(9,4,0)
import Language.Haskell.Syntax.Extension (HsToken (..))
#endif

#if !MIN_VERSION_ghc(9,10,0) && MIN_VERSION_ghc(9,4,0)
import GHC.Parser.PostProcess            (mkTokenLocation)
#endif

#if MIN_VERSION_ghc(9,8,0)
import Language.Haskell.Syntax.Expr      (LHsRecUpdFields (..))
#endif

#if MIN_VERSION_ghc(9,6,0)
import GHC.Hs.Pat                        (RecFieldsDotDot (..))
#endif

#if MIN_VERSION_ghc(9,4,0)
import GHC.Hs.Expr                       (gHsPar)
import Language.Haskell.Syntax.Expr      (HsDoFlavour (..))
#else
import GHC_Hs_Expr                       (HsStmtContext (..))
#endif

#if MIN_VERSION_ghc(9,2,0) && !MIN_VERSION_ghc(9,6,0)
import GHC_Parser_Annotation             (locA)
#endif

#if MIN_VERSION_ghc(9,2,0)
import GHC_Hs_Utils                      (hsTypeToHsSigWcType)
#else
import GHC_Hs_Utils                      (mkLHsSigWcType)
import GHC_Parser_PostProcess            (mkRdrRecordUpd)
#endif

#if MIN_VERSION_ghc(9,0,0)
import GHC_Hs_Utils                      (mkPsBindStmt, mkSimpleMatch)
import GHC_Types_SrcLoc                  (UnhelpfulSpanReason (..))
#else
import GHC_Hs_Utils                      (mkBindStmt, mkHsLam)
#endif

import GHC_Hs_Expr                       (parenthesizeHsExpr)

-- Internal
import Language.Finkel.Builder
import Language.Finkel.Data.FastString   (FastString, fsLit, lengthFS, nullFS,
                                          unconsFS, unpackFS)
import Language.Finkel.Data.SourceText
import Language.Finkel.Form
import Language.Finkel.Syntax.HBind
import Language.Finkel.Syntax.HType
import Language.Finkel.Syntax.SynUtils


-- ---------------------------------------------------------------------
--
-- Expression
--
-- ---------------------------------------------------------------------

b_ifE :: Code -> HExpr -> HExpr -> HExpr -> HExpr
b_ifE (LForm (L l _)) p t f =
#if MIN_VERSION_ghc(9,2,0)
  lA l (mkHsIf p t f unused)
#else
  L l (mkHsIf p t f)
#endif
{-# INLINABLE b_ifE #-}

b_lamE :: (HExpr,[HPat]) -> HExpr
#if MIN_VERSION_ghc(9,0,0)
b_lamE (body,pats) = mkLHsPar (lA l hsLam)
  -- Using 'mkHsLam' will make a 'MatchGroup' value with 'Generated' origin
  -- instead of 'FromSource', and contains 'noLoc' location. These were causing
  -- some issues when "-Wincomplete-patterns" flag was turned on.
  where
#  if MIN_VERSION_ghc(9,10,0)
    hsLam = HsLam NOEXT LamSingle mg
#  else
    hsLam = HsLam NOEXT mg
#  endif
    l = getLoc (reLoc body)
    mg = mkMatchGroup FromSource ms
#  if MIN_VERSION_ghc(9,10,0)
    ms = reLocA (L l [mkSimpleMatch (LamAlt LamSingle) pats body])
#  elif MIN_VERSION_ghc(9,2,0)
    ms = reLocA (L l [mkSimpleMatch LambdaExpr pats body])
#  else
    ms = [mkSimpleMatch LambdaExpr pats body]
#  endif
#else
b_lamE (body,pats) = mkHsLam pats body
#endif
{-# INLINABLE b_lamE #-}

b_tupE :: Code -> [HExpr] -> HExpr
b_tupE (LForm (L l _)) args = lA l e
  where
    e = explicitTuple (map mkArg args) Boxed
#if MIN_VERSION_ghc(9,2,0)
    mkArg x          = present x
#else
    mkArg x@(L al _) = L al (present x)
#endif
    explicitTuple = ExplicitTuple NOEXT
    present = Present NOEXT
{-# INLINABLE b_tupE #-}

-- Expression for tuple constructor function (i.e. the (,)
-- function). See also 'b_varE' for tuples with more elements.
b_tupConE :: Code -> HExpr
b_tupConE (LForm (L l _)) = lA l (HsVar NOEXT (lN l (tupConName Boxed 2)))
{-# INLINABLE b_tupConE #-}

b_letE :: Code -> [HDecl] -> HExpr -> Builder HExpr
b_letE (LForm (L l _)) decls body = do
  cd <- cvBindsAndSigs (toOL decls)
#if MIN_VERSION_ghc(9,2,0)
  let valbinds = mkHsValBinds (cd_binds cd) (cd_sigs cd)
#else
  let valbinds = L l (mkHsValBinds (cd_binds cd) (cd_sigs cd))
#endif
#if MIN_VERSION_ghc(9,10,0)
  pure (lA l (HsLet (NOEXT, NOEXT) valbinds body))
#elif MIN_VERSION_ghc(9,4,0)
  let tokLet = L (mkTokenLocation l) HsTok
      tokIn = L (mkTokenLocation l) HsTok
  return (lA l (HsLet NOEXT tokLet valbinds tokIn body))
#else
  return (lA l (HsLet NOEXT valbinds body))
#endif
{-# INLINABLE b_letE #-}

b_caseE :: Code -> HExpr -> [HMatch] -> HExpr
b_caseE (LForm (L l _)) expr matches = lA l (hsCase expr mg)
  where
    hsCase = HsCase NOEXT
#if MIN_VERSION_ghc(9,2,0)
    mg = mkMatchGroup FromSource (lL l matches)
#else
    mg = mkMatchGroup FromSource matches
#endif
{-# INLINABLE b_caseE #-}

b_match :: HPat -> ([HGRHS],[HDecl]) -> HMatch
b_match pat (grhss,decls) = L l (Match NOEXT ctxt [pat] grhss')
  where
    grhss' = mkGRHSs grhss decls l
    ctxt = CaseAlt
    l = getLoc (dL pat)
{-# INLINABLE b_match #-}

b_hgrhs :: [HGRHS] -> (HExpr, [HGuardLStmt]) -> [HGRHS]
b_hgrhs rhss (body, gs) =
  let lrhs = case gs of
#if MIN_VERSION_ghc(9,10,0)
        [] -> reLocA (noLoc rhs)
        _  -> let l = getLoc (mkLocatedListA gs) in L l rhs
#elif MIN_VERSION_ghc(9,4,0)
        [] -> reLocA (noLoc rhs)
        _  -> let l = getLoc (mkLocatedListA gs) in la2la (L l rhs)
#else
        [] -> noLoc rhs
        _  -> let l = getLoc (mkLocatedListA gs) in reLoc (L l rhs)
#endif
      rhs = b_GRHS gs body
  in  (lrhs:rhss)
{-# INLINABLE b_hgrhs #-}

b_GRHS :: [HGuardLStmt] -> HExpr -> GRHS PARSED HExpr
b_GRHS = GRHS NOEXT
{-# INLINABLE b_GRHS #-}

b_doE :: Code -> [HStmt] -> HExpr
-- XXX: Does not support "[ModuleName.].do" syntax yet.
b_doE (LForm (L l _)) exprs =
#if MIN_VERSION_ghc(9,2,0)
  lA l (mkHsDo (DoExpr Nothing) (reLocA (L l exprs)))
#elif MIN_VERSION_ghc(9,0,0)
  L l (mkHsDo (DoExpr Nothing) exprs)
#else
  L l (mkHsDo DoExpr exprs)
#endif
{-# INLINABLE b_doE #-}

b_tsigE :: Code -> HExpr -> ([HType], HType) -> HExpr
b_tsigE (LForm (L l _)) e0 (ctxt,t) =
  let t' = case ctxt of
             [] -> t
#if MIN_VERSION_ghc(9,10,0)
             _  -> lA l (mkHsQualTy' (mkLocatedListA ctxt) t)
#else
             _  -> lA l (mkHsQualTy' (la2la (mkLocatedListA ctxt)) t)
#endif
#if MIN_VERSION_ghc(9,2,0)
      e1 = ExprWithTySig NOEXT e0 (hsTypeToHsSigWcType t')
#else
      e1 = ExprWithTySig NOEXT e0 (mkLHsSigWcType t')
#endif
  in  mkLHsPar (lA l e1)
{-# INLINABLE b_tsigE #-}

b_recConOrUpdE :: Code
               -> [Either Code (Located FastString, Maybe HExpr)]
               -> Builder HExpr
b_recConOrUpdE whole@(LForm (L l form)) flds =
  case form of
    Atom (ASymbol name) | isLexCon name ->
#if MIN_VERSION_ghc(9,2,0)
      pure (lA l (mkRdrRecordCon (lN l (mkVarRdrName name)) cflds unused))
#else
      pure (L l (mkRdrRecordCon (L l (mkVarRdrName name)) cflds))
#endif
    _ -> do
      v <- b_varE whole
#if MIN_VERSION_ghc(9,8,0)
      -- XXX: Use mkRdrRecordUpd, runPV, and unP?
      pure (lA l (RecordUpd { rupd_ext = unused
                            , rupd_expr = v
                            , rupd_flds = RegularRecUpdFields
                                { xRecUpdFields = unused
                                , recUpdFields = uflds }}))
#elif MIN_VERSION_ghc(9,2,0)
      pure (lA l (RecordUpd { rupd_ext = unused
                            , rupd_expr = v
                            , rupd_flds = Left uflds }))
#else
      pure (L l (mkRdrRecordUpd v uflds))
#endif
  where
    cflds = HsRecFields { rec_flds = map mkcfld' non_wilds
                        , rec_dotdot = mb_dotdot }
    uflds = map mkufld non_wilds
    mkufld  = cfld2ufld . mkcfld'
    (wilds, non_wilds) = partitionEithers flds
    mb_dotdot = case wilds of
      []               -> Nothing
#if MIN_VERSION_ghc(9,10,0)
      LForm (L wl _):_ -> Just (la2la
                                (L wl (RecFieldsDotDot (length non_wilds))))
#elif MIN_VERSION_ghc(9,6,0)
      LForm (L wl _):_ -> Just (L wl (RecFieldsDotDot (length non_wilds)))
#else
      LForm (L wl _):_ -> Just (L wl (length non_wilds))
#endif
{-# INLINABLE b_recConOrUpdE #-}

b_recUpdE :: Builder HExpr -> [PreRecField HExpr] -> Builder HExpr
b_recUpdE expr flds = do
   expr' <- expr
   let uflds = map (cfld2ufld . mkcfld') non_wilds
       (wilds, non_wilds) = partitionEithers flds
       l = getLoc expr'
   case wilds of
     (_:_) -> builderError
#if MIN_VERSION_ghc(9,8,0)
     -- XXX: Does not support OverloadedRecUpdFields. Use mkRdrRecordUpd, runPV,
     -- and unP?
     []    -> pure (L l (RecordUpd { rupd_ext = unused
                                   , rupd_expr = mkLHsPar expr'
                                   , rupd_flds = RegularRecUpdFields
                                      { xRecUpdFields = unused
                                      , recUpdFields = uflds }}))
#elif MIN_VERSION_ghc(9,2,0)
     -- XXX: Does not support record dot syntax yet.  The return type of
     -- 'mkRdrRecordUpd' function changed from previous ghc release, now the
     -- function returns 'PV (HsExpr GhcPs)', formerly it was 'HsExpr GhcPs'.
     []    -> pure (L l (RecordUpd { rupd_ext = unused
                                   , rupd_expr = mkLHsPar expr'
                                   , rupd_flds = Left uflds }))
#else
     []    -> pure (L l (mkRdrRecordUpd (mkLHsPar expr') uflds))
#endif
{-# INLINABLE b_recUpdE #-}

mkcfld' :: (Located FastString, Maybe HExpr) -> LHsRecField PARSED HExpr
mkcfld' (n,mb_e) =
  case mb_e of
    Just e  -> mkcfld False (n, e)
    Nothing -> mkcfld True (n, punned)
  where
    punned = lA l (HsVar NOEXT (lN l punRDR))
    l = getLoc n
{-# INLINABLE mkcfld' #-}

b_opOrAppE :: Code -> ([HExpr], [HType]) -> Builder HExpr
b_opOrAppE code (args, tys) = do
  fn <- b_varE code
  let fn' = mkAppTypes fn tys
      mkOp loc lhs rhs = lA loc (mkOpApp fn' lhs (mkLHsParOp rhs))
  case code of
    -- Perform operator expansion, or delegate to `b_appE' if the head of the
    -- form was non-operator.
    LForm (L l (Atom (ASymbol name)))
      | let name' = maybe name snd (splitQualName name)
      , isLexSym name'
      , hd:rest@(_:_) <- args
      -> pure (foldl' (mkOp l) (mkLHsParOp hd) rest)
    _ -> pure (b_appE (fn':args, tys))
{-# INLINABLE b_opOrAppE #-}

mkLHsParOp :: HExpr -> HExpr
mkLHsParOp = parenthesizeHsExpr opPrec
{-# INLINABLE mkLHsParOp #-}

mkOpApp :: HExpr -> HExpr -> HExpr -> HsExpr PARSED
mkOpApp op l = OpApp NOEXT l op
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
#if MIN_VERSION_ghc(9,10,0)
  L l (HsAppType NOEXT expr (mkHsWildCardBndrs ty))
#elif MIN_VERSION_ghc(9,6,0)
  L l (HsAppType NOEXT expr noHsTok (mkHsWildCardBndrs ty))
#elif MIN_VERSION_ghc(9,2,0)
  L l (HsAppType (locA l) expr (mkHsWildCardBndrs ty))
#else
  cL l (HsAppType NOEXT expr (mkHsWildCardBndrs ty))
#endif

b_charE :: Code -> Builder HExpr
b_charE (LForm (L l form)) =
  case form of
    Atom (AChar st x) -> return (lA l (hsLit (HsChar st x)))
    _                 -> builderError
{-# INLINABLE b_charE #-}

b_stringE :: Code -> Builder HExpr
b_stringE (LForm (L l form)) =
  case form of
    Atom (AString st x) -> return (lA l (hsLit (HsString st x)))
    _                   -> builderError
{-# INLINABLE b_stringE #-}

b_integerE :: Code -> Builder HExpr
b_integerE (LForm (L l form)) =
  case form of
    Atom (AInteger x)
      | il_value x < 0 -> return (lA l (hsPar (expr x)))
      | otherwise      -> return (expr x)
    _                  -> builderError
  where
    expr x = lA l (hsOverLit $! mkHsIntegral x)
{-# INLINABLE b_integerE #-}

b_fracE :: Code -> Builder HExpr
b_fracE (LForm (L l form)) =
  case form of
    Atom (AFractional x)
      | fl_value x < 0 -> return (lA l (hsPar (expr x)))
      | otherwise      -> return (expr x)
    _                  -> builderError
  where
    expr x = lA l (hsOverLit $! mkHsFractional x)
{-# INLINABLE b_fracE #-}

b_varE :: Code -> Builder HExpr
b_varE (LForm (L l form))
  | Atom (ASymbol x) <- form
  , not (nullFS x)
  , Just (hdchr,tlchrs) <- unconsFS x
  = case hdchr of
      -- Overloaded label starts with `#'. Tail characters need to be a valid
      -- variable identifier.
      '#' | isLexVarId tlchrs ->
#if MIN_VERSION_ghc(9,6,0)
          ret (HsOverLabel NOEXT (toQuotedSourceText tlchrs) tlchrs)
#elif MIN_VERSION_ghc(9,2,0)
          ret (HsOverLabel NOEXT tlchrs)
#else
          ret (HsOverLabel NOEXT Nothing tlchrs)
#endif

      -- Tuple constructor function with more than two elements are written as
      -- symbol with sequence of commas, handling such case in this function.
      ',' | all (== ',') (unpackFS tlchrs)
          -> ret (var (tupConName Boxed (lengthFS x + 1)))

      -- Plain variable identifier.
      _   -> ret (var (mkVarRdrName x))
  | otherwise = builderError
  where
    ret = return . lA l
    var n = HsVar NOEXT (lN l n)
{-# INLINABLE b_varE #-}

b_unitE :: Code -> HExpr
b_unitE (LForm (L l _)) =
#if MIN_VERSION_ghc(9,2,0)
  case mkLHsTupleExpr [] unused of L _ t -> lA l t
#else
  case mkLHsTupleExpr [] of L _ t -> L l t
#endif
{-# INLINABLE b_unitE #-}

b_docString :: Code -> Builder (Located HsDocString)
b_docString (LForm (L l form)) =
  case form of
    Atom (AString _ x) -> return $! L l (mkHsDocString x)
    _                  -> builderError
{-# INLINABLE b_docString #-}

b_hsListE :: Either HExpr [HExpr] -> HExpr
b_hsListE expr =
  case expr of
#if MIN_VERSION_ghc(9,2,0)
    Right exprs -> L l (ExplicitList NOEXT exprs)
#else
    Right exprs -> L l (ExplicitList NOEXT Nothing exprs)
#endif
      where
        l = getLoc (mkLocatedListA exprs)
    Left arithSeqExpr -> arithSeqExpr
{-# INLINABLE b_hsListE #-}

b_lcompE :: HExpr -> [HStmt] -> HExpr
b_lcompE ret stmts = L l (mkHsComp ListComp stmts ret)
  where l = getLoc ret
{-# INLINABLE b_lcompE #-}

b_arithSeqE :: HExpr -> Maybe HExpr -> Maybe HExpr -> HExpr
b_arithSeqE fromE thenE toE = L l (ArithSeq NOEXT Nothing info)
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
    AChar st c      -> mk_lapp qCharS (lA l (hsLit (HsChar st c)))
    AString st str  -> mk_lapp qStringS (mk_str st str)
    AInteger _il    -> b_integerE orig >>= mk_lapp qIntegerS
    AFractional _fl -> b_fracE orig >>= mk_lapp qFractionalS
    AUnit           -> mk_unit
  where
    mk_sym s = lA l (hsLit (HsString (toQuotedSourceText s) s))
    mk_str st str = lA l (hsLit (HsString st str))
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
    fname fs = lA ql (hsLit (HsString (toQuotedSourceText fs) fs))
    mk_int n = lA ql $! hsOverLit $! mkHsIntegral $! mkIntegralLit n
#if MIN_VERSION_ghc(9,0,0)
    ql = UnhelpfulSpan (UnhelpfulOther (fsLit "<b_quoteE>"))
#else
    ql = UnhelpfulSpan (fsLit "<b_quoteE>")
#endif
{-# INLINABLE getLocInfo #-}

b_rapp :: Either a b -> ([a],[b]) -> ([a],[b])
b_rapp = either (first . (:)) (second . (:))
{-# INLINABLE b_rapp #-}

b_exprOrTyArg :: Code -> Builder (Either HExpr HType)
b_exprOrTyArg lform = case lform of
  LForm (L l (Atom (ASymbol sym)))
    | Just ('@', rest) <- unconsFS sym, not (nullFS rest)
    -> fmap Right (b_symT (LForm (L l (Atom (ASymbol rest)))))
  _ -> fmap Left (b_varE lform)
{-# INLINABLE b_exprOrTyArg #-}


-- ------------------------------------------------------------------------
--
-- Auxiliary
--
-- ------------------------------------------------------------------------

hsLit :: HsLit PARSED -> HsExpr PARSED
hsLit = HsLit NOEXT
{-# INLINABLE hsLit #-}

hsPar :: HExpr -> HsExpr PARSED
#if MIN_VERSION_ghc(9,4,0)
hsPar = gHsPar
#else
hsPar = HsPar NOEXT
#endif
{-# INLINABLE hsPar #-}

hsOverLit :: HsOverLit PARSED -> HsExpr PARSED
hsOverLit = HsOverLit NOEXT
{-# INLINABLE hsOverLit #-}

tupConName :: Boxity -> Arity -> RdrName
tupConName boxity arity = getRdrName (tupleDataCon boxity arity)
{-# INLINABLE tupConName #-}


-- ---------------------------------------------------------------------
--
-- Statement
--
-- ---------------------------------------------------------------------

b_bindS :: Code -> HPat -> HExpr -> HStmt
b_bindS (LForm (L l _)) pat expr =
#if MIN_VERSION_ghc(9,2,0)
  lA l (mkPsBindStmt unused pat expr)
#elif MIN_VERSION_ghc(9,0,0)
  L l (mkPsBindStmt pat expr)
#else
  L l (mkBindStmt pat expr)
#endif
{-# INLINABLE b_bindS #-}

b_letS :: Code -> [HDecl] -> Builder HStmt
b_letS (LForm (L l _)) decls = do
  cd <- cvBindsAndSigs (toOL decls)
  let valbinds = mkHsValBinds (cd_binds cd) (cd_sigs cd)
      letStmt = LetStmt NOEXT
#if MIN_VERSION_ghc(9,2,0)
  return (lA l (letStmt valbinds))
#else
  return (L l (letStmt (L l valbinds)))
#endif
{-# INLINABLE b_letS #-}

b_bodyS :: HExpr -> HStmt
b_bodyS expr = L (getLoc expr) (mkBodyStmt expr)
{-# INLINABLE b_bodyS #-}


-- ------------------------------------------------------------------------
--
-- Parenthesizing
--
-- ------------------------------------------------------------------------

-- Below note is for parenthesizing under ghc < 8.10, which won't hold any more:

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
