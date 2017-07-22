{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Builder functions for Haskell syntax data type.
module SK.Core.Builder where

-- base
import Control.Monad (ap, foldM, liftM)
import Data.Char (isUpper)
import Data.List (foldl1')

-- transformers
import Control.Monad.Trans.Except
import Control.Monad.Trans.State

-- Internal
import SK.Core.GHC
import SK.Core.Form

-- -------------------------------------------------------------------
--
-- Builder data type
--
-- -------------------------------------------------------------------

-- | State for 'Builder'.
data BState = BState
    { -- | Input tokens to parse.
      inputs :: [Code]
      -- | Last token, for error message.
    , lastToken :: Maybe Code
    }

-- | Newtype wrapper for parsing form data with Happy.
newtype Builder a = Builder {
    unBuilder :: StateT BState (Either String) a
}

runBuilder :: Builder a
           -> [Code]
           -> Either String (a, [Code])
runBuilder bld toks =
    case runStateT (unBuilder bld) (BState toks Nothing) of
      Left e -> Left e
      Right (a, st) -> Right (a, inputs st)

evalBuilder :: Builder a -> [Code] -> Either String a
evalBuilder bld toks = fmap fst (runBuilder bld toks)

evalBuilder' :: Monad m
             => Builder a
             -> [Code]
             -> ExceptT String m a
evalBuilder' bld toks = case evalBuilder bld toks of
  Right a -> return a
  Left err -> throwE err

failB :: String -> Builder a
failB err = Builder (StateT (\_ -> Left err))

instance Functor Builder where
    fmap = liftM

instance Applicative Builder where
    pure = return
    (<*>) = ap

instance Monad Builder where
  return a = Builder (return a)
  m >>= k  =
      Builder
        (StateT (\st ->
                   case runStateT (unBuilder m) st of
                     Right (a,st') -> runStateT (unBuilder (k a)) st'
                     Left err -> Left err))

getBState :: Builder BState
getBState = Builder get

putBState :: BState -> Builder ()
putBState = Builder . put

-- | Parse with builder using given tokens, continue on successful
-- parse.
parse :: Builder a -> [Code] -> Builder a
parse bld toks =
  case runBuilder bld toks of
    Right (a, _) -> return a
    Left err -> failB err

-- | Simple lexer to parse forms.
formLexer :: (Code -> Builder a) -> Builder a
formLexer cont = do
    st <- getBState
    case inputs st of
      [] -> cont (LForm (L undefined TEnd))
      x:xs -> do
        putBState (st {inputs = xs, lastToken = Just x})
        cont x


-- ---------------------------------------------------------------------
--
-- Type synonyms
--
-- ---------------------------------------------------------------------

type HExpr = LHsExpr RdrName

type HDecl = LHsDecl RdrName

type HConDecl = LConDecl RdrName

type HConDeclField = LConDeclField RdrName

type HTyVarBndr = LHsTyVarBndr RdrName

type HBind = LHsBind RdrName

type HSigWcType = LHsSigWcType RdrName

type HType = LHsType RdrName

type HPat = LPat RdrName

type HStmt = ExprLStmt RdrName

type HLocalBinds = Located (HsLocalBinds RdrName)

type HMatch = LMatch RdrName HExpr

type HGRHS = LGRHS RdrName HExpr

type HGuardLStmt = GuardLStmt RdrName

type HImportDecl = LImportDecl RdrName


-- ---------------------------------------------------------------------
--
-- Auxiliary
--
-- ---------------------------------------------------------------------

builderError :: Builder a
builderError = do
  st <- getBState
  case lastToken st of
    Nothing -> failB "no location"
    Just x  ->
      failB (showLoc x ++ "parse error on input `" ++
             show (pForm x) ++ "'")


-- | Unwrap the element of 'List' and 'HsList', otherwise returns '[]'.
unwrapListL :: Code -> [Code]
unwrapListL (LForm (L _ form)) =
    case form of
      List xs -> xs
      HsList xs -> xs
      _ -> []

-- XXX: Currently, cannot tell the difference between 'Qualified.fun'
-- and 'DataConstr'.
mkRdrName :: FastString -> RdrName
mkRdrName name
  -- ':' is special syntax. It is defined in module "GHC.Types" in
  -- package "ghc-prim", but not exported.
  | name == ":" = nameRdrName consDataConName

  -- Data constructor starts from capital letter or ':'.
  | isUpper x || x == ':' = mkUnqual srcDataName name

  -- Variable.
  | otherwise = mkVarUnqual name
  where
    x = headFS name

-- | Build 'HLocalBinds' from list of 'HDecl's.
declsToBinds :: SrcSpan -> [HDecl] -> HLocalBinds
declsToBinds l decls = L l binds'
  where
    binds' = case decls of
      [] -> emptyLocalBinds
      _  -> HsValBinds (ValBindsIn (listToBag binds) sigs)
    (binds, sigs) = go ([],[]) decls
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
cfld2ufld :: Located (HsRecField RdrName (LHsExpr RdrName))
          -> Located (HsRecUpdField RdrName)
-- Almost same as 'mk_rec_upd_field' in 'RdrHsSyn'
cfld2ufld (L l0 (HsRecField (L l1 (FieldOcc rdr _)) arg pun)) =
  L l0 (HsRecField (L l1 (Unambiguous rdr PlaceHolder)) arg pun)


-- ---------------------------------------------------------------------
--
-- Module
--
-- ---------------------------------------------------------------------

-- In GHC source code, there is a file "compiler/hsSyn/Convert.hs".
-- This module contains codes converting Template Haskell data types to
-- GHC's internal data type, which is a helpful resource for
-- understanding the values and types for constructing Haskell AST data.

b_module :: Code -> Maybe LHsDocString -> [HImportDecl]
         -> [HDecl] -> HsModule RdrName
b_module (LForm (L l (Atom (ASymbol name)))) mbdoc imports decls =
    HsModule { hsmodName = Just (L l (mkModuleNameFS name))
             , hsmodExports = Nothing
             , hsmodImports = imports
             -- Function `cvTopDecls' is used for mergeing multiple
             -- top-level FunBinds, which possibly taking different
             -- patterns in its arguments.
             , hsmodDecls = cvTopDecls (toOL decls)
             , hsmodDeprecMessage = Nothing
             , hsmodHaddockModHeader = mbdoc }

b_implicitMainModule :: [HImportDecl] -> [HDecl] -> HsModule RdrName
b_implicitMainModule =
  b_module (LForm (noLoc (Atom (ASymbol "Main")))) Nothing


-- ---------------------------------------------------------------------
--
-- Declarations
--
-- ---------------------------------------------------------------------

b_importD :: Code -> HImportDecl
b_importD (LForm (L l (Atom (ASymbol m)))) =
    L l (simpleImportDecl (mkModuleNameFS m))

b_dataD :: Code
        -> (FastString, [HTyVarBndr])
        -> (HsDeriving RdrName, [HConDecl])
        -> HDecl
b_dataD = mkNewtypeOrDataD DataType

b_newtypeD :: Code -> (FastString, [HTyVarBndr])
           -> (HsDeriving RdrName, [HConDecl])
           -> HDecl
b_newtypeD = mkNewtypeOrDataD NewType

mkNewtypeOrDataD :: NewOrData -> Code
                 -> (FastString, [HTyVarBndr])
                 -> (HsDeriving RdrName, [HConDecl])
                 -> HDecl
mkNewtypeOrDataD newOrData (LForm (L l _)) (name, tvs) (derivs, cs) =
  L l (TyClD decl)
  where
    decl = DataDecl { tcdLName = L l (mkUnqual tcName name)
                    , tcdTyVars = mkHsQTvs tvs
                    , tcdDataDefn = defn
                    , tcdDataCusk = PlaceHolder
                    , tcdFVs = placeHolderNames }
    defn = HsDataDefn { dd_ND = newOrData
                      , dd_ctxt = noLoc []
                      , dd_cType = Nothing
                      , dd_kindSig = Nothing
                      , dd_cons = cs
                      -- `dd_derivs' field changed since ghc-8.0.2.
                      , dd_derivs = derivs }

b_typeD :: Code -> (FastString, [HTyVarBndr]) -> HType -> HDecl
b_typeD (LForm (L l _)) (name, tvs) ty = L l (TyClD synonym)
  where
    -- Fields in 'SynDecl' changed since ghc-8.0.2.
    synonym = SynDecl { tcdLName = L l (mkUnqual tcName name)
                      , tcdTyVars = mkHsQTvs tvs
                      , tcdRhs = ty
                      , tcdFVs = placeHolderNames }

b_simpletypeD :: [Code] -> (FastString, [HTyVarBndr])
b_simpletypeD ((LForm (L _ (Atom (ASymbol name)))):tvs) = (name, tvs')
  -- XXX: Kind signatures not supported.
  where
    tvs' = map (f . unLForm) tvs
    f (L l (Atom (ASymbol tname))) =
        L l (UserTyVar (L l (mkUnqual tvName tname)))

b_conD :: Code -> HsConDeclDetails RdrName -> HConDecl
b_conD (LForm (L l1 (Atom (ASymbol s1)))) details =
    L l1 ConDeclH98 { con_name = L l1 (mkUnqual srcDataName s1)
                     , con_qvars = Nothing
                     , con_cxt = Nothing
                     , con_details = details
                     , con_doc = Nothing }

b_conOnlyD :: Code -> HConDecl
b_conOnlyD name = b_conD name (PrefixCon [])

-- XXX: Infix data constructor not supported.
b_conDeclDetails :: [HType] -> HsConDeclDetails RdrName
b_conDeclDetails = PrefixCon

b_recFieldsD :: [HConDeclField] -> HsConDeclDetails RdrName
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
b_derivD :: (HsDeriving RdrName, [HConDecl])
         -> [HType]
         -> (HsDeriving RdrName, [HConDecl])
b_derivD (_, cs) tys = (Just (L l (map mkLHsSigType tys)), cs)
  where l = getLoc (mkLocatedList tys)

b_classD :: ([HType],HType) -> [HDecl] -> Builder HDecl
b_classD (tys,ty) decls = do
    -- XXX: Does not support multiple type class.
    let categorize (ms,ss) (L ld decl) =
          case decl of
            SigD d -> return (ms, L ld d : ss)
            ValD d -> return (L ld d : ms, ss)
            _      -> builderError

    (l, name, L lv tvar) <-
       case ty of
          L l (HsAppTy (L _ (HsTyVar n)) (L _ (HsTyVar v))) ->
            return (l, n, v)
          _ -> builderError
    (meths,sigs) <- foldM categorize ([],[]) decls

    let bndrs = [L lv (UserTyVar (L lv tvar))]
        cls = ClassDecl { tcdCtxt = mkLocatedList tys
                        , tcdLName = name
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

b_funBindD :: Code -> (([HGRHS],[HDecl]), [HPat]) -> HDecl
b_funBindD (LForm (L l (Atom (ASymbol name)))) ((grhss,decls), args) =
  let match = L l (Match ctxt args Nothing body)
      body = GRHSs grhss (declsToBinds l decls)
      ctxt = NonFunBindMatch
      bind = mkFunBind (L l (mkRdrName name)) [match]
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


-- ---------------------------------------------------------------------
--
-- Types
--
-- ---------------------------------------------------------------------

b_symT :: Code -> HType
b_symT (LForm (L l (Atom (ASymbol name)))) = L l (HsTyVar (L l ty))
  where
    ty = mkUnqual namespace name
    namespace =
      case () of
        _ | isUpper x || ':' == x -> tcName
        _                         -> tvName
        where x = headFS name

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
b_appT (x:xs) = foldl f x xs
  where f b a = L (getLoc b) (HsAppTy b a)

b_listT :: HType -> HType
b_listT ty@(L l _) = L l (HsListTy ty)

b_tupT :: Code -> [HType] -> HType
b_tupT (LForm (L l _)) ts = L l (HsTupleTy HsBoxedTuple ts)

b_bangT :: Code -> HType -> HType
b_bangT (LForm (L l _)) t = L l (HsBangTy srcBang t)
  where
    -- HsSrcBang field changed in ghc >= 8.0.2.
    srcBang = HsSrcBang (Just "bang") NoSrcUnpack SrcStrict


-- ---------------------------------------------------------------------
--
-- Pattern
--
-- ---------------------------------------------------------------------

b_intP :: Code -> HPat
b_intP (LForm (L l (Atom (AInteger n)))) =
  L l (mkNPat (L l lit) Nothing)
  where
     lit = mkHsIntegral (show n) n placeHolderType

b_stringP :: Code -> HPat
b_stringP (LForm (L l (Atom (AString s)))) =
  L l (mkNPat (L l lit) Nothing)
  where
    lit = mkHsIsString s (fsLit s) placeHolderType

b_charP :: Code -> HPat
b_charP (LForm (L l (Atom (AChar c)))) =
  let lit = HsChar (show c) c
  in  L l (LitPat lit)

b_symP :: Code -> HPat
b_symP (LForm (L l (Atom (ASymbol name))))
   | name == "_" = L l (WildPat placeHolderType)
   | isUpper x || x == ':'
    = L l (ConPatIn (L l (mkRdrName name)) (PrefixCon []))
   | otherwise = L l (VarPat (L l (mkRdrName name)))
   where x = headFS name

b_hsListP :: [HPat] -> HPat
b_hsListP pats = L l (ListPat pats placeHolderType Nothing)
  where l = getLoc (mkLocatedList pats)

b_tupP :: Code -> [HPat] -> HPat
b_tupP (LForm (L l _)) ps = L l (TuplePat ps Boxed [])

b_asP :: Code -> HPat -> HPat
b_asP (LForm (L l (Atom (ASymbol name)))) pat =
  L l (AsPat (L l (mkRdrName name)) pat)

b_lazyP :: HPat -> HPat
b_lazyP pat@(L l _) = L l (LazyPat pat)

b_conP :: Code -> [HPat] -> Builder HPat
b_conP (LForm (L l (Atom (ASymbol conName)))) rest =
  case () of
    _ | isUpper x || x == ':' ->
      return (L l (ConPatIn (L l (mkRdrName conName))
                            (PrefixCon rest)))
    _                         -> builderError
    where x = headFS conName


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
  L l (HsLet (declsToBinds l decls) body)

b_caseE :: Code -> HExpr -> [HMatch] -> HExpr
b_caseE (LForm (L l _)) expr matches = L l (HsCase expr mg)
  where mg = mkMatchGroup FromSource matches

b_match :: HPat -> ([HGRHS],[HDecl]) -> HMatch
b_match pat@(L l _) (grhss,decls) =
    L l (Match NonFunBindMatch [pat] Nothing grhss')
  where
    grhss' = GRHSs grhss (declsToBinds l decls)

b_hgrhs :: [HGRHS] -> (HExpr, [GuardLStmt RdrName]) -> [HGRHS]
b_hgrhs rhss (body, gs) =
  let rhs = GRHS gs body
      lrhs = case gs of
        [] -> noLoc rhs
        _  -> let l = getLoc (mkLocatedList gs) in L l rhs
  in  (lrhs:rhss)

b_grhs :: HExpr -> HExpr -> HGRHS
b_grhs guard@(L l _) body = L l (GRHS [L l (mkBodyStmt guard)] body)

b_doE :: Code -> [HStmt] -> HExpr
b_doE (LForm (L l _)) exprs = L l (mkHsDo DoExpr exprs)

b_tsigE :: Code -> HExpr -> ([HType], HType) -> HExpr
b_tsigE (LForm (L l _)) e (ctxt,t) =
  let t' = case ctxt of
             [] -> t
             _  -> L l HsQualTy { hst_ctxt = mkLocatedList ctxt
                                , hst_body = t }
  in  L l (ExprWithTySig e (mkLHsSigWcType t'))

b_recConOrUpdE :: Code -> [(FastString,HExpr)] -> HExpr
b_recConOrUpdE sym@(LForm (L l _)) flds = L l expr
  where
    expr =
      case () of
        _ | isUpper (headFS name) -> mkRdrRecordCon rName cflds
        _ -> mkRdrRecordUpd (b_varE sym) uflds
    name = symbolNameFS sym
    rName = L l (mkRdrName name)
    cflds = HsRecFields { rec_flds = map mkcfld flds
                        , rec_dotdot = Nothing }
    uflds = map mkufld flds
    mkufld  = cfld2ufld . mkcfld

mkcfld :: (FastString,HExpr) -> LHsRecField RdrName HExpr
mkcfld (name, e@(L fl _)) =
  L fl HsRecField { hsRecFieldLbl = mkfname fl name
                  , hsRecFieldArg = e
                  , hsRecPun = False }
  where
    mkfname nl n = L nl (mkFieldOcc (L nl (mkRdrName n)))

b_recUpdE :: Builder HExpr -> [(FastString,HExpr)] -> Builder HExpr
b_recUpdE expr flds = do
   expr' <- expr
   let uflds = map (cfld2ufld . mkcfld) flds
       l = getLoc expr'
   return (L l (mkRdrRecordUpd expr' uflds))

b_appE :: [HExpr] -> HExpr
b_appE = foldl1' (\a b -> L (getLoc a) (HsApp a b))

b_lbindB :: (HExpr -> HsBind RdrName) -> HExpr -> HBind
b_lbindB f e = L (getLoc e) (f e)

b_charE :: Code -> HExpr
b_charE (LForm (L l (Atom (AChar x)))) = L l (HsLit (HsChar (show x) x))

b_stringE :: Code -> HExpr
b_stringE (LForm (L l (Atom (AString x)))) =
  L l (HsLit (HsString x (fsLit x)))

b_integerE :: Code -> HExpr
b_integerE (LForm (L l (Atom (AInteger x)))) =
    L l (HsOverLit $! mkHsIntegral (show x) x placeHolderType)

b_floatE :: Code -> HExpr
b_floatE (LForm (L l (Atom (AFractional x)))) =
   L l (HsOverLit $! mkHsFractional x placeHolderType)

b_varE :: Code -> HExpr
b_varE (LForm (L l (Atom (ASymbol x)))) =
  L l (HsVar (L l (mkRdrName x)))

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
b_letS (LForm (L l _)) decls = L l (LetStmt (declsToBinds l decls))

b_bodyS :: HExpr -> HStmt
b_bodyS expr = L (getLoc expr) (mkBodyStmt expr)
