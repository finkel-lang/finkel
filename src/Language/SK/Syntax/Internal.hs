-- | Auxiliary module for syntax.
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Language.SK.Syntax.Internal where

-- base
import Control.Monad (foldM)
import Data.Char (isUpper)
import Data.List (foldl1')
import Data.Maybe (fromMaybe)

-- Internal
import Language.SK.Builder
import Language.SK.GHC
import Language.SK.Form


-- ---------------------------------------------------------------------
--
-- Module
--
-- ---------------------------------------------------------------------

-- In GHC source code, there is a file "compiler/hsSyn/Convert.hs".
-- This module contains codes converting Template Haskell data types to
-- GHC's internal data type, which is a helpful resource for
-- understanding the values and types for constructing Haskell AST data.

b_module :: Code -> [HIE] ->  Maybe LHsDocString -> [HImportDecl]
         -> [HDecl] -> HModule
b_module form exports mbdoc imports decls =
  HsModule { hsmodName = Just (L l (mkModuleNameFS name))
           , hsmodExports = exports'
           , hsmodImports = imports
           -- Function `cvTopDecls' is used for mergeing multiple
           -- top-level FunBinds, which possibly taking different
           -- patterns in its arguments.
           , hsmodDecls = cvTopDecls (toOL decls)
           , hsmodDeprecMessage = Nothing
           , hsmodHaddockModHeader = mbdoc }
  where
    LForm (L l (Atom (ASymbol name))) = form
    exports'
      | null exports = Nothing
      | otherwise    = Just (L l exports)

b_implicitMainModule :: [HImportDecl] -> [HDecl] -> HModule
b_implicitMainModule =
  b_module (LForm (noLoc (Atom (ASymbol "Main")))) [] Nothing

b_ieSym :: Code -> HIE
b_ieSym (LForm (L l (Atom (ASymbol name)))) = thing
  where thing = L l (IEVar (L l (IEName (L l (mkRdrName name)))))

b_ieAbs :: Code -> HIE
b_ieAbs (LForm (L l (Atom (ASymbol name)))) = thing
  where
    thing = L l (IEThingAbs (L l (IEName (L l (mkUnqual tcName name)))))

b_ieAll :: Code -> HIE
b_ieAll (LForm (L l (Atom (ASymbol name)))) = thing
  where
    thing = L l (IEThingAll (L l (IEName (L l (mkUnqual tcName name)))))

b_ieWith :: Code -> [Code] -> HIE
b_ieWith (LForm (L l (Atom (ASymbol name)))) names = thing
  where
    thing = L l (IEThingWith (L l (IEName (L l name'))) wc ns fs)
    name' = case splitQualName name of
              Just qual -> mkQual tcName qual
              Nothing   -> mkUnqual tcName name
    wc = NoIEWildcard
    (ns, fs) = foldr f ([],[]) names
    f (LForm (L l0 (Atom (ASymbol n0)))) (ns0, fs0)
      | isUpper c || c == ':' =
        (L l0 (IEName (L l (mkUnqual tcName n0))) : ns0, fs0)
      | otherwise             = (ns0, L l0 (fl n0) : fs0)
      where
        c = headFS n0
    -- Does not support DuplicateRecordFields.
    fl x = FieldLabel { flLabel = x
                      , flIsOverloaded = False
                      , flSelector = mkRdrName x }

b_ieMdl :: [Code] -> HIE
b_ieMdl [LForm (L l (Atom (ASymbol name)))] = L l thing
  where thing = IEModuleContents (L l (mkModuleNameFS name))


-- ---------------------------------------------------------------------
--
-- Declarations
--
-- ---------------------------------------------------------------------

b_importD :: (Code, Bool, Maybe Code) -> (Bool, Maybe [HIE])
          -> HImportDecl
b_importD (name, qualified, mb_as) (hiding, mb_entities) =
  case name of
    LForm (L l (Atom (ASymbol m))) ->
      let decl = simpleImportDecl (mkModuleNameFS m)
          decl' = decl { ideclQualified = qualified
                       , ideclAs = fmap asModName mb_as
                       , ideclHiding = hiding' }
          asModName (LForm (L l' (Atom (ASymbol x)))) =
            L l' (mkModuleNameFS x)
          hiding' =
            case mb_entities of
              Nothing       -> Nothing
              Just entities -> Just (hiding, L l entities)
      in  L l decl'

b_isAs :: Code -> Builder Code
b_isAs form =
  case form of
    LForm (L _ (Atom (ASymbol as))) | as == "as" -> return form
    _                                            -> builderError

b_dataD :: Code
        -> (FastString, [HTyVarBndr])
        -> (HDeriving, [HConDecl])
        -> HDecl
b_dataD = mkNewtypeOrDataD DataType

b_newtypeD :: Code -> (FastString, [HTyVarBndr])
           -> (HDeriving, [HConDecl])
           -> HDecl
b_newtypeD = mkNewtypeOrDataD NewType

mkNewtypeOrDataD :: NewOrData -> Code
                 -> (FastString, [HTyVarBndr])
                 -> (HDeriving, [HConDecl])
                 -> HDecl
mkNewtypeOrDataD newOrData (LForm (L l _)) (name, tvs) (derivs, cs) =
  L l (TyClD decl)
  where
    decl = DataDecl { tcdLName = L l (mkUnqual tcName name)
                    , tcdFixity = Prefix
                    , tcdTyVars = mkHsQTvs tvs
                    , tcdDataDefn = defn
                    , tcdDataCusk = PlaceHolder
                    , tcdFVs = placeHolderNames }
    defn = HsDataDefn { dd_ND = newOrData
                      , dd_ctxt = noLoc []
                      , dd_cType = Nothing
                      , dd_kindSig = Nothing
                      , dd_cons = cs
                      , dd_derivs = derivs }

b_typeD :: Code -> (FastString, [HTyVarBndr]) -> HType -> HDecl
b_typeD (LForm (L l _)) (name, tvs) ty = L l (TyClD synonym)
  where
    synonym = SynDecl { tcdLName = L l (mkUnqual tcName name)
                      , tcdFixity = Prefix
                      , tcdTyVars = mkHsQTvs tvs
                      , tcdRhs = ty
                      , tcdFVs = placeHolderNames }

b_simpletypeD :: [Code] -> (FastString, [HTyVarBndr])
b_simpletypeD ((LForm (L _ (Atom (ASymbol name)))):tvs) = (name, tvs')
  -- XXX: Kind signatures not supported.
  where
    tvs' = map codeToUserTyVar tvs

b_conD :: Code -> HConDeclDetails -> HConDecl
b_conD (LForm (L l1 (Atom (ASymbol s1)))) details =
  L l1 ConDeclH98 { con_name = L l1 (mkUnqual srcDataName s1)
                  , con_qvars = Nothing
                  , con_cxt = Nothing
                  , con_details = details
                  , con_doc = Nothing }

b_forallD :: [Code] -> (HConDecl, [HType]) -> HConDecl
b_forallD vars ((L l cdecl), cxts) =
  L l cdecl { con_qvars = Just (mkHsQTvs (map codeToUserTyVar vars))
            , con_cxt = Just (mkLocatedList cxts) }

b_gadtD :: Code -> ([HType], HType) -> HConDecl
b_gadtD (LForm (L l1 (Atom (ASymbol name)))) (ctxt, bodyty) =
  let decl = ConDeclGADT { con_names = [nam]
                         , con_type = mkLHsSigType qty
                         , con_doc = Nothing }
      nam = L l1 (mkUnqual srcDataName name)
      qty = L l1 HsQualTy { hst_ctxt = mkLocatedList ctxt
                          , hst_body = bodyty }
  in  L l1 decl

b_conOnlyD :: Code -> HConDecl
b_conOnlyD name = b_conD name (PrefixCon [])

-- XXX: Infix data constructor not supported.
b_conDeclDetails :: [HType] -> HConDeclDetails
b_conDeclDetails = PrefixCon

b_recFieldsD :: [HConDeclField] -> HConDeclDetails
b_recFieldsD flds = RecCon (mkLocatedList flds)

b_recFieldD :: [Code] -> HType -> HConDeclField
b_recFieldD names ty = L loc field
  where
    field = ConDeclField { cd_fld_names = names'
                         , cd_fld_type = ty
                         , cd_fld_doc = Nothing }
    loc = getLoc (mkLocatedForm names)
    names' = map f names
    f (LForm (L l (Atom (ASymbol name)))) =
        L l (mkFieldOcc (L l (mkRdrName name)))

-- 'HsDeriving' changed in git head since ghc-8.0.2 release.
b_derivD :: (HDeriving, [HConDecl])
         -> [HType]
         -> (HDeriving, [HConDecl])
b_derivD (_, cs) tys = (L l dcs, cs)
  where
    l = getLoc (mkLocatedList tys)
    dcs = [L l (HsDerivingClause
                 { deriv_clause_strategy = Nothing
                 , deriv_clause_tys = L l (map mkLHsSigType tys)})]

b_classD :: ([HType],HType) -> [HDecl] -> Builder HDecl
b_classD (tys,ty) decls = do
    -- XXX: Does not support multi-arg type class.
    let categorize (ms,ss) (L ld decl) =
          case decl of
            SigD d -> return (ms, L ld d : ss)
            ValD d -> return (L ld d : ms, ss)
            _      -> builderError
        unAppTy t =
          case t of
            L l (HsAppTy (L _ (HsTyVar _promo1 n))
                         (L _ (HsTyVar _promo2 v))) ->
              return (l, n, v)
            L _ (HsParTy t') -> unAppTy t'
            _                -> builderError
    (l, name, L lv tvar) <- unAppTy ty
    (meths,sigs) <- foldM categorize ([],[]) decls

    let bndrs = [L lv (UserTyVar (L lv tvar))]
        cls = ClassDecl { tcdCtxt = mkLocatedList tys
                        , tcdLName = name
                        , tcdFixity = Prefix
                        , tcdTyVars = mkHsQTvs bndrs
                        , tcdFDs = []
                        , tcdSigs = mkClassOpSigs sigs
                        , tcdMeths = listToBag meths
                        , tcdATs = []
                        , tcdATDefs = []
                        , tcdDocs = []
                        , tcdFVs = placeHolderNames }
    return (L l (TyClD cls))

b_instD :: ([HType], HType) -> [HDecl] -> HDecl
b_instD (ctxts,ty@(L l _)) decls = L l (InstD (ClsInstD decl))
  where
    decl = ClsInstDecl { cid_poly_ty = mkLHsSigType qty
                       , cid_binds = listToBag binds
                       , cid_sigs = mkClassOpSigs []
                       , cid_tyfam_insts = []
                       , cid_datafam_insts = []
                       , cid_overlap_mode = Nothing }
    qty = L l HsQualTy { hst_ctxt = mkLocatedList ctxts
                       , hst_body = ty }
    binds = foldr declToBind [] decls
    -- XXX: Skipping non-functional bindings.
    declToBind d acc =
      case d of
        L l1 (ValD bind) -> L l1 bind : acc
        _  -> acc

b_qtyclC :: [HType] -> Builder ([HType], HType)
b_qtyclC ts =
  case ts of
    []  -> builderError
    [_] -> builderError
    _   -> do
      let (ctxt,t) = splitAt (length ts - 1) ts
      return (ctxt, head t)

b_defaultD :: [HType] -> HDecl
b_defaultD types = L l (DefD (DefaultDecl types))
  where
    l = getLoc (mkLocatedList types)

b_fixityD :: FixityDirection -> Code -> [Code] -> HDecl
b_fixityD dir (LForm (L l (Atom (AInteger n)))) syms = L l (SigD fsig)
  where
    fsig = FixSig (FixitySig names fixity)
    names = map lname syms
    lname (LForm (L l0 (Atom (ASymbol name)))) = L l0 (mkRdrName name)
    fixity = Fixity dir' (fromIntegral n) dir
    dir' = case dir of
             InfixL -> SourceText "infixl"
             InfixR -> SourceText "infixr"
             InfixN -> SourceText "infix"

b_ffiD :: Code -> Code -> HCCallConv -> (Maybe (Located Safety), Code)
       -> (Code, HType) -> Builder HDecl
b_ffiD (LForm (L l _)) imp_or_exp ccnv (mb_safety, ename) (nm, ty) =
  case unCode imp_or_exp of
    Atom (ASymbol ie)
      | ie == "import"
      , Just ispec <- parseCImport ccnv safety name ename' source -> do
        let fi = ForeignImport { fd_name = lname
                               , fd_sig_ty = tsig
                               , fd_co = noForeignImportCoercionYet
                               , fd_fi = ispec }
        return (L l (ForD fi))
      | ie == "export" -> do
        let fe = ForeignExport { fd_name = lname
                               , fd_sig_ty = tsig
                               , fd_co = noForeignExportCoercionYet
                               , fd_fe = e }
            e = CExport (L l (CExportStatic (SourceText ename')
                                            (fsLit ename')
                                            (unLoc ccnv)))
                        (L l (SourceText ename'))
        return (L l (ForD fe))
    _ -> builderError
    where
      lname = L ln (mkRdrName name)
      LForm (L ln (Atom (ASymbol name))) = nm
      tsig = mkLHsSigType ty
      LForm (L _ls (Atom (AString ename'))) = ename
      source = L l (quotedSourceText ename')
      safety = fromMaybe (noLoc PlayRisky) mb_safety

b_callConv :: Code -> Builder (Located CCallConv)
b_callConv (LForm (L l (Atom (ASymbol sym)))) =
  case sym of
    "capi"       -> return (L l CApiConv)
    "ccall"      -> return (L l CCallConv)
    "prim"       -> return (L l PrimCallConv)
    "javascript" -> return (L l JavaScriptCallConv)
    "stdcall"    -> return (L l StdCallConv)
    _            -> builderError

b_safety :: Code -> Builder (Located Safety)
b_safety (LForm (L l (Atom (ASymbol sym)))) =
  case sym of
    "interruptible" -> return (L l PlayInterruptible)
    "safe"          -> return (L l PlaySafe)
    "unsafe"        -> return (L l PlayRisky)
    _               -> builderError

b_funBindD :: Code -> (([HGRHS],[HDecl]), [HPat]) -> HDecl
b_funBindD (LForm (L l (Atom (ASymbol name)))) ((grhss,decls), args) =
  let body = GRHSs grhss (declsToBinds l decls)
      match = L l (Match ctxt args Nothing body)
      ctxt = FunRhs { mc_fun = lrname
                    , mc_fixity = Prefix
                      -- XXX: Get strictness info from somewhere?
                    , mc_strictness = NoSrcStrict }
      lrname = L l (mkRdrName name)
      bind = mkFunBind lrname [match]
  in  L l (ValD bind)

b_patBindD :: ([HGRHS],[HDecl]) -> HPat -> HDecl
b_patBindD (grhss,decls) pat@(L l _) =
  let bind = PatBind { pat_lhs = pat
                     , pat_rhs = GRHSs grhss (declsToBinds l decls)
                     , pat_rhs_ty = placeHolderType
                     , bind_fvs = placeHolderNames
                     , pat_ticks = ([],[]) }
  in  L l (ValD bind)

b_tsigD :: [Code] -> ([HType], HType) -> HDecl
b_tsigD names (ctxts,typ) =
  let typ' = mkLHsSigWcType qtyp
      qtyp | null ctxts = typ
           | otherwise = L l HsQualTy { hst_ctxt = mkLocatedList ctxts
                                      , hst_body = typ }
      mkName (LForm (L l1 (Atom (ASymbol name)))) =
        L l1 (mkRdrName name)
      l = getLoc (mkLocatedForm names)
  in  L l (SigD (TypeSig (map mkName names) typ'))

b_inlineD :: InlineSpec -> Code -> HDecl
b_inlineD ispec (LForm (L l (Atom (ASymbol name)))) =
  L l (SigD (InlineSig (L l (mkRdrName name)) ipragma))
  where
    ipragma =
      case ispec of
        Inline    -> alwaysInlinePragma
        NoInline  ->
          defaultInlinePragma { inl_inline = NoInline
                              , inl_src = SourceText "{-# NOINLINE" }
        Inlinable ->
          defaultInlinePragma { inl_inline = Inlinable
                              , inl_src = SourceText "{-# INLINABLE" }
        _         -> defaultInlinePragma

b_specializeD :: Code -> (Code, HType) -> Builder HDecl
b_specializeD (LForm (L l _)) (nameSym, tsig) = do
  let LForm (L ln (Atom (ASymbol name))) = nameSym
      lname = L ln (mkRdrName name)
      ip = defaultInlinePragma {inl_src = SourceText "{-# SPECIALIZE"}
  return (L l (SigD (SpecSig lname [mkLHsSigType tsig] ip)))

-- ---------------------------------------------------------------------
--
-- Types
--
-- ---------------------------------------------------------------------

b_symT :: Code -> HType
b_symT (LForm (L l (Atom (ASymbol name)))) =
  L l (HsTyVar NotPromoted (L l ty))
  where
    ty = case splitQualName name of
           Nothing   -> mkUnqual namespace name
           Just qual -> mkQual namespace qual
    namespace
      | isUpper x || ':' == x = tcName
      | otherwise             = tvName
    x = headFS name

b_unitT :: Code -> HType
b_unitT (LForm (L l _)) = L l (HsTupleTy HsBoxedTuple [])

b_funT :: [HType] -> Builder HType
b_funT ts =
  case ts of
    [] -> builderError
    _  -> return (foldr1 f ts)
  where
    f a@(L l1 _) b = L l1 (HsFunTy a b)

b_appT :: [HType] -> HType
b_appT whole@(x:xs) = L l0 (HsParTy (foldl f x xs))
  where
    l0 = getLoc (mkLocatedList whole)
    f b a = L (getLoc b) (HsAppTy b a)

b_listT :: HType -> HType
b_listT ty@(L l _) = L l (HsListTy ty)

b_nilT :: Code -> HType
b_nilT (LForm (L l _)) =
  L l (HsTyVar NotPromoted (L l (getRdrName listTyCon)))

b_tupT :: Code -> [HType] -> HType
b_tupT (LForm (L l _)) ts = L l (HsTupleTy HsBoxedTuple ts)

b_bangT :: Code -> HType -> HType
b_bangT (LForm (L l _)) t = L l (HsBangTy srcBang t)
  where
    srcBang = HsSrcBang (SourceText "b_bangT") NoSrcUnpack SrcStrict

b_forallT :: (Code, [Code]) -> ([HType], HType) -> HType
b_forallT ((LForm (L l0 _)), vars) (ctxts, body) = L l0 forAllTy
  where
    forAllTy = HsForAllTy {hst_bndrs = bndrs, hst_body = ty}
    ty = L l0 HsQualTy { hst_ctxt = mkLocatedList ctxts
                       , hst_body = body}
    bndrs = map codeToUserTyVar vars

b_unpackT :: Code -> HType -> HType
b_unpackT (LForm (L l _)) t = L l (HsBangTy bang t')
  where
    bang = HsSrcBang (SourceText "b_unpackT") SrcUnpack strictness
    (strictness, t') =
      case t of
        L _ (HsBangTy (HsSrcBang _ _ st) t0) -> (st, t0)
        _                                    -> (NoSrcStrict, t)


-- ---------------------------------------------------------------------
--
-- Pattern
--
-- ---------------------------------------------------------------------

b_intP :: Code -> HPat
b_intP (LForm (L l (Atom (AInteger n)))) =
  L l (mkNPat (L l lit) Nothing)
  where
     lit = mkHsIntegral (SourceText (show n)) n placeHolderType

b_stringP :: Code -> HPat
b_stringP (LForm (L l (Atom (AString s)))) =
  L l (mkNPat (L l lit) Nothing)
  where
    lit = mkHsIsString (SourceText (show s)) (fsLit s) placeHolderType

b_charP :: Code -> HPat
b_charP (LForm (L l (Atom (AChar c)))) =
  let lit = HsChar (SourceText (show c)) c
  in  L l (LitPat lit)

b_symP :: Code -> HPat
b_symP (LForm (L l (Atom (ASymbol name))))
   | name == "_"
    = L l (WildPat placeHolderType)
   | isUpper x || x == ':'
    = L l (ConPatIn (L l (mkVarRdrName name)) (PrefixCon []))
   | otherwise
    = L l (VarPat (L l (mkRdrName name)))
   where
     x = headFS name

b_hsListP :: [HPat] -> HPat
b_hsListP pats = L l (ListPat pats placeHolderType Nothing)
  where l = getLoc (mkLocatedList pats)

b_labeledP :: Code -> [(Code, HPat)] -> Builder HPat
b_labeledP (LForm (L l (Atom (ASymbol name)))) ps
  | isUpper x || x == ':' =  do
    let rc = HsRecFields { rec_flds = map mkcfld' ps
                         , rec_dotdot = Nothing }
        mkcfld' (LForm (L _ (Atom (ASymbol n))), p) = mkcfld (n, p)
    return (L l (ConPatIn (L l (mkVarRdrName name)) (RecCon rc)))
  | otherwise = builderError
  where x = headFS name

b_tupP :: Code -> [HPat] -> HPat
b_tupP (LForm (L l _)) ps = L l (TuplePat ps Boxed [])

b_asP :: Code -> HPat -> HPat
b_asP (LForm (L l (Atom (ASymbol name)))) pat =
  L l (AsPat (L l (mkRdrName name)) (mkParPat pat))

b_lazyP :: HPat -> HPat
b_lazyP pat@ (L l _) = L l (ParPat (L l (LazyPat pat)))

b_conP :: Code -> [HPat] -> Builder HPat
b_conP (LForm (L l (Atom (ASymbol name)))) rest
  | isUpper x || x == ':'
    = return (L l (ParPat (L l (ConPatIn (L l (mkVarRdrName name))
                                         (PrefixCon rest)))))
  | otherwise = builderError
  where
    x = headFS name


-- ---------------------------------------------------------------------
--
-- Expression
--
-- ---------------------------------------------------------------------

b_ifE :: Code -> HExpr -> HExpr -> HExpr -> HExpr
b_ifE (LForm (L l (Atom _))) p t f = L l (mkHsIf p t f)

b_lamE :: (HExpr,[HPat]) -> HExpr
b_lamE (body,pats) = mkHsLam pats body

b_tupE :: Code -> [HExpr] -> HExpr
b_tupE (LForm (L l _)) args = L l (ExplicitTuple (map mkArg args) Boxed)
  where mkArg x@(L al _) = L al (Present x)

b_letE :: Code -> [HDecl] -> HExpr -> HExpr
b_letE (LForm (L l _)) decls body =
  let (mbs, sigs) = cvBindsAndSigs (toOL decls)
  in  L l (HsLet (L l (HsValBinds (ValBindsIn mbs sigs))) body)

b_caseE :: Code -> HExpr -> [HMatch] -> HExpr
b_caseE (LForm (L l _)) expr matches = L l (HsCase expr mg)
  where mg = mkMatchGroup FromSource matches

b_match :: HPat -> ([HGRHS],[HDecl]) -> HMatch
b_match pat@(L l _) (grhss,decls) =
    L l (Match ctxt [pat] Nothing grhss')
  where
    grhss' = GRHSs grhss (declsToBinds l decls)
    ctxt = CaseAlt

b_hgrhs :: [HGRHS] -> (HExpr, [HGuardLStmt]) -> [HGRHS]
b_hgrhs rhss (body, gs) =
  let rhs = GRHS gs body
      lrhs = case gs of
        [] -> noLoc rhs
        _  -> let l = getLoc (mkLocatedList gs) in L l rhs
  in  (lrhs:rhss)

b_doE :: Code -> [HStmt] -> HExpr
b_doE (LForm (L l _)) exprs = L l (mkHsDo DoExpr exprs)

b_tsigE :: Code -> HExpr -> ([HType], HType) -> HExpr
b_tsigE (LForm (L l _)) e (ctxt,t) =
  let t' = case ctxt of
             [] -> t
             _  -> L l HsQualTy { hst_ctxt = mkLocatedList ctxt
                                , hst_body = t }
  in  mkLHsPar (L l (ExprWithTySig e (mkLHsSigWcType t')))

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

b_recUpdE :: Builder HExpr -> [(FastString,HExpr)] -> Builder HExpr
b_recUpdE expr flds = do
   expr' <- expr
   let uflds = map (cfld2ufld . mkcfld) flds
       l = getLoc expr'
   return (L l (mkRdrRecordUpd (mkLHsPar expr') uflds))

b_appE :: [HExpr] -> HExpr
b_appE = foldl1' f
  where
    f a b = let e = mkHsApp a (mkLHsPar b) in e

b_charE :: Code -> HExpr
b_charE (LForm (L l (Atom (AChar x)))) =
  L l (HsLit (HsChar (SourceText (show x)) x))

b_stringE :: Code -> HExpr
b_stringE (LForm (L l (Atom (AString x)))) =
  L l (HsLit (HsString (SourceText (show x)) (fsLit x)))

b_integerE :: Code -> HExpr
b_integerE (LForm (L l (Atom (AInteger x))))
   | x < 0     = L l (HsPar expr)
   | otherwise = expr
  where
    expr =     L l (HsOverLit $! mkHsIntegral st x placeHolderType)
    st = SourceText (show x)

b_floatE :: Code -> HExpr
b_floatE (LForm (L l (Atom (AFractional x))))
  | fl_value x < 0 = L l (HsPar expr)
  | otherwise      = expr
  where
    expr = L l (HsOverLit $! mkHsFractional x placeHolderType)

b_varE :: Code -> HExpr
b_varE (LForm (L l (Atom (ASymbol x)))) =
  L l (HsVar (L l (mkVarRdrName x)))

b_unitE :: Code -> HExpr
b_unitE (LForm (L l _)) = L l (ExplicitTuple [] Boxed)

b_commentStringE :: Code -> Located HsDocString
b_commentStringE (LForm (L l (Atom (AComment x)))) =
  L l (HsDocString (fsLit x))

b_hsListE :: Either HExpr [HExpr] -> HExpr
b_hsListE expr =
  case expr of
    Right exprs -> L l (ExplicitList placeHolderType Nothing exprs)
      where l = getLoc (mkLocatedList exprs)
    Left arithSeqExpr -> arithSeqExpr

b_arithSeqE :: HExpr -> Maybe HExpr -> Maybe HExpr -> HExpr
b_arithSeqE fromE thenE toE =
  L l (ArithSeq noPostTcExpr Nothing info)
  where
    info | Just thenE' <- thenE, Just toE' <- toE =
           FromThenTo fromE thenE' toE'
         | Just thenE' <- thenE =
           FromThen fromE thenE'
         | Just toE' <- toE =
           FromTo fromE toE'
         | otherwise = From fromE
    l = getLoc fromE


-- ---------------------------------------------------------------------
--
-- Statement
--
-- ---------------------------------------------------------------------

b_bindS :: Code -> HPat -> HExpr -> HStmt
b_bindS (LForm (L l _)) pat expr = L l (mkBindStmt pat expr)

b_letS :: Code -> [HDecl] -> HStmt
b_letS (LForm (L l _)) decls =
  let (mbs, sigs) = cvBindsAndSigs (toOL decls)
  in  L l (LetStmt (L l (HsValBinds (ValBindsIn mbs sigs))))

b_bodyS :: HExpr -> HStmt
b_bodyS expr = L (getLoc expr) (mkBodyStmt expr)


-- ---------------------------------------------------------------------
--
-- Auxiliary
--
-- ---------------------------------------------------------------------

-- | Build 'HLocalBinds' from list of 'HDecl's.
declsToBinds :: SrcSpan -> [HDecl] -> HLocalBinds
declsToBinds l decls = L l binds'
  where
    binds' = case decls of
      [] -> emptyLocalBinds
      _  -> HsValBinds (ValBindsIn (listToBag binds) sigs)
    -- Using 'RdrHsSyn.cvTopDecls' to group same names in where
    -- clause. Perhaps better to do similar things done in
    -- 'RdrHsSyn.cvBindGroup', which is dedicated for 'P' monad ...
    decls' = cvTopDecls (toOL decls)
    (binds, sigs) = go ([],[]) decls'
    go (bs,ss) ds =
      case ds of
        [] -> (bs, ss)
        d:ds' -> case d of
          L ld (ValD b) -> go (L ld b:bs,ss) ds'
          L ld (SigD s) -> go (bs,L ld s:ss) ds'

-- Function defined in 'HsUtils', not exported.
mkLocatedList ::  [Located a] -> Located [Located a]
mkLocatedList [] = noLoc []
mkLocatedList ms = L (combineLocs (head ms) (last ms)) ms

-- | Convert record field constructor expression to record field update
-- expression.
cfld2ufld :: Located (HsRecField RdrName HExpr)
          -> Located (HsRecUpdField RdrName)
-- Almost same as 'mk_rec_upd_field' in 'RdrHsSyn'
cfld2ufld (L l0 (HsRecField (L l1 (FieldOcc rdr _)) arg pun)) =
  L l0 (HsRecField (L l1 (Unambiguous rdr PlaceHolder)) arg pun)

-- | Make 'HsRecField' with given name and located data.
mkcfld :: (FastString, Located a) -> LHsRecField RdrName (Located a)
mkcfld (name, e@(L fl _)) =
  L fl HsRecField { hsRecFieldLbl = mkfname fl name
                  , hsRecFieldArg = e
                  , hsRecPun = False }
  where
    mkfname nl n = L nl (mkFieldOcc (L nl (mkRdrName n)))

quotedSourceText :: String -> SourceText
quotedSourceText s = SourceText $ "\"" ++ s ++ "\""

-- Following `cvBindsAndSigs`, `getMonoBind`, `has_args`, and
-- `makeFunBind` functions are based on resembling functions defined in
-- `RdrHsSyn` module in ghc package, since these functions were not
-- exported.
--
-- Unlike the original version, `cvBindsAndSigs` has pattern matches
-- for 'ValD' and 'SigD' only, and `getMonoBind` ignores 'DocD'
-- declarations.

cvBindsAndSigs :: OrdList HDecl -> (HBinds, [HSig])
cvBindsAndSigs fb = go (fromOL fb)
  where
    go [] = (emptyBag, [])
    go (L l (ValD d) : ds)
      = let (b', ds') = getMonoBind (L l d) ds
            (bs, ss) = go ds'
        in  (b' `consBag` bs, ss)
    go (L l (SigD s) : ds)
      = let (bs, ss) = go ds
        in  (bs, L l s : ss)

getMonoBind :: LHsBind RdrName -> [LHsDecl RdrName]
            -> (LHsBind RdrName, [LHsDecl RdrName])
getMonoBind (L loc1 (FunBind { fun_id = fun_id1@(L _ f1),
                               fun_matches
                                 = MG { mg_alts = L _ mtchs1 }}))
            binds
  | has_args mtchs1 = go mtchs1 loc1 binds
  where
    go mtchs loc
       (L loc2 (ValD (FunBind { fun_id = L _ f2,
                                fun_matches
                                  = MG { mg_alts = L _ mtchs2 }}))
                : binds2)
      | f1 == f2 = go (mtchs2 ++ mtchs)
                      (combineSrcSpans loc loc2) binds2
    go mtchs loc binds2
      = (L loc (makeFunBind fun_id1 (reverse mtchs)), binds2)
      -- Reverse the final matches, to get it back in the right order

getMonoBind bind binds = (bind, binds)

-- Don't group together FunBinds if they have no arguments.  This is
-- necessary now that variable bindings with no arguments are now
-- treated as FunBinds rather than pattern bindings.
has_args :: [LMatch RdrName (LHsExpr RdrName)] -> Bool
has_args ((L _ (Match _ args _ _)) : _) = not (null args)
has_args []                             =
  error "Language.SK.Syntax.Internal:has_args"

-- Like HsUtils.mkFunBind, but we need to be able to set the fixity too
makeFunBind :: Located RdrName -> [LMatch RdrName (LHsExpr RdrName)]
            -> HsBind RdrName
makeFunBind fn ms
  = FunBind { fun_id = fn,
              fun_matches = mkMatchGroup FromSource ms,
              fun_co_fn = idHsWrapper,
              bind_fvs = placeHolderNames,
              fun_tick = [] }

codeToUserTyVar :: Code -> HTyVarBndr
codeToUserTyVar code =
  case code of
    LForm (L l (Atom (ASymbol name))) ->
      L l (UserTyVar (L l (mkUnqual tvName name)))
    _ -> error "Language.SK.Syntax.Internal:codeToUserTyVar"
