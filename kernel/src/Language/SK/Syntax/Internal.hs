-- | Auxiliary module for syntax.
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Language.SK.Syntax.Internal where

-- base
import Control.Monad (foldM)
import Data.Char (isUpper)
import Data.List (foldl1')
import Data.Maybe (fromMaybe)

-- ghc
import HsSyn

import Bag (emptyBag, consBag, listToBag)
import BasicTypes ( Boxity(..), Fixity(..), FixityDirection(..)
                  , FractionalLit(..), InlinePragma(..)
                  , InlineSpec(..), LexicalFixity(..)
                  , Origin(..), OverlapMode(..), SourceText(..)
                  , alwaysInlinePragma
                  , fl_value
                  , defaultInlinePragma
#if MIN_VERSION_ghc(8,4,0)
                  , IntegralLit(..)
#endif
                  )
import FastString (FastString, headFS, lengthFS, unpackFS)
import FieldLabel (FieldLbl(..))
import ForeignCall (CCallConv(..), CExportSpec(..), Safety(..))
#if MIN_VERSION_ghc(8,6,0)
import HsDoc (mkHsDocString)
import HsExtension (noExt)
#endif
import HsUtils (mkHsIntegral, mkLHsTupleExpr)
import OccName (srcDataName, tcName, tvName)
import OrdList (OrdList, fromOL, toOL)
import Module (mkModuleNameFS)
import RdrHsSyn ( cvTopDecls, parseCImport, mkConDeclH98
                , mkGadtDecl, mkRdrRecordCon, mkRdrRecordUpd )
import RdrName (getRdrName, mkQual, mkUnqual)
import SrcLoc (Located, combineLocs, combineSrcSpans, noLoc)
import TysWiredIn (listTyCon, tupleTyCon)

-- Internal
import Language.SK.Builder
import Language.SK.Form

-- ---------------------------------------------------------------------
--
-- C macro for GHC
--
-- ---------------------------------------------------------------------

-- From ghc 8.6.0, many number of data type used by the internal AST in
-- GHC were modified to take extra argument. Following `NOEXT' macro
-- will pass the `noExt' argument to such constructors, and behaves as
-- empty code when compiling with GHC version prior to 8.6.0.

#if MIN_VERSION_ghc(8,6,0)
#define NOEXT noExt
#else
#define NOEXT {- empty -}
#endif


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
  where
    thing = L l (iEVar (L l (IEName (L l (mkRdrName name)))))
    iEVar = IEVar NOEXT

b_ieAbs :: Code -> HIE
b_ieAbs (LForm (L l (Atom (ASymbol name)))) = thing
  where
    thing = L l (iEThingAbs (L l (IEName (L l (mkUnqual tcName name)))))
    iEThingAbs = IEThingAbs NOEXT

b_ieAll :: Code -> HIE
b_ieAll (LForm (L l (Atom (ASymbol name)))) = thing
  where
    thing = L l (iEThingAll (L l (IEName (L l (mkUnqual tcName name)))))
    iEThingAll = IEThingAll NOEXT

b_ieWith :: Code -> [Code] -> HIE
b_ieWith (LForm (L l (Atom (ASymbol name)))) names = thing
  where
    thing = L l (iEThingWith (L l (IEName (L l name'))) wc ns fs)
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
    iEThingWith = IEThingWith NOEXT

b_ieMdl :: [Code] -> HIE
b_ieMdl [LForm (L l (Atom (ASymbol name)))] = L l thing
  where
    thing = iEModuleContents (L l (mkModuleNameFS name))
    iEModuleContents = IEModuleContents NOEXT


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
  L l (mkTyClD decl)
  where
    decl = DataDecl { tcdLName = L l (mkUnqual tcName name)
                    , tcdFixity = Prefix
                    , tcdTyVars = mkHsQTvs tvs
                    , tcdDataDefn = defn
#if MIN_VERSION_ghc(8,6,0)
                    , tcdDExt = noExt
#else
                    , tcdDataCusk = PlaceHolder
                    , tcdFVs = placeHolderNames
#endif
                    }
    defn = HsDataDefn { dd_ND = newOrData
                      , dd_ctxt = noLoc []
                      , dd_cType = Nothing
                      , dd_kindSig = Nothing
                      , dd_cons = cs
                      , dd_derivs = derivs
#if MIN_VERSION_ghc(8,6,0)
                      , dd_ext = noExt
#endif
                      }

b_typeD :: Code -> (FastString, [HTyVarBndr]) -> HType -> HDecl
b_typeD (LForm (L l _)) (name, tvs) ty = L l (mkTyClD synonym)
  where
    synonym = SynDecl { tcdLName = L l (mkUnqual tcName name)
                      , tcdFixity = Prefix
                      , tcdTyVars = mkHsQTvs tvs
                      , tcdRhs = ty
#if MIN_VERSION_ghc(8,6,0)
                      , tcdSExt = noExt
#else
                      , tcdFVs = placeHolderNames
#endif
                      }

b_simpletypeD :: [Code] -> (FastString, [HTyVarBndr])
b_simpletypeD ((LForm (L _ (Atom (ASymbol name)))):tvs) = (name, tvs')
  -- XXX: Kind signatures not supported.
  where
    tvs' = map codeToUserTyVar tvs

b_conD :: Code -> HConDeclDetails -> HConDecl
b_conD (LForm (L l1 (Atom (ASymbol s1)))) details =
  L l1 (mkConDeclH98 name Nothing cxt details)
    where
      name = L l1 (mkUnqual srcDataName s1)
#if MIN_VERSION_ghc(8,6,0)
      cxt = Nothing
#else
      cxt = L l1 []
#endif

b_forallD :: [Code] -> (HConDecl, [HType]) -> HConDecl
b_forallD vars ((L l cdecl), cxts) =
#if MIN_VERSION_ghc(8,6,0)
  L l cdecl { con_forall = noLoc True
            , con_ex_tvs = map codeToUserTyVar vars
            , con_mb_cxt = Just (mkLocatedList cxts) }
#else
  L l cdecl { con_qvars = Just (mkHsQTvs (map codeToUserTyVar vars))
            , con_cxt = Just (mkLocatedList cxts) }
#endif

b_gadtD :: Code -> ([HType], HType) -> HConDecl
b_gadtD (LForm (L l1 (Atom (ASymbol name)))) (ctxt, bodyty) =
  let name' = L l1 (mkUnqual srcDataName name)
      ty = L l1 qty
      qty = mkHsQualTy_compat (mkLocatedList ctxt) bodyty
      decl = mkGadtDecl [name'] ty'
#if MIN_VERSION_ghc(8,6,0)
      ty' = ty
      decl' = fst decl
#else
      ty' = mkLHsSigType ty
      decl' = decl
#endif
  in L l1 decl'

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
#if MIN_VERSION_ghc(8,6,0)
                         , cd_fld_ext = noExt
#endif
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
#if MIN_VERSION_ghc(8,6,0)
                 , deriv_clause_ext = noExt
#endif
                 , deriv_clause_tys = L l (map mkLHsSigType tys)})]

b_classD :: ([HType],HType) -> [HDecl] -> Builder HDecl
b_classD (tys,ty) decls = do
    let categorize (ms,ss) (L ld decl) =
          case decl of
#if MIN_VERSION_ghc(8,6,0)
            SigD _ d -> return (ms, L ld d : ss)
            ValD _ d -> return (L ld d : ms, ss)
#else
            SigD d -> return (ms, L ld d : ss)
            ValD d -> return (L ld d : ms, ss)
#endif
            _      -> builderError
        -- Recursing in `HsAppTy' to support MultiParamTypeClasses.
        unAppTy t =
          case t of
#if MIN_VERSION_ghc(8,6,0)
            L l (HsTyVar _p _ n) -> return (l, n, [])
            L _ (HsAppTy _ t1 (L lv (HsTyVar _ _ v))) -> do
              (l, n, vs) <- unAppTy t1
              return (l, n, L lv (UserTyVar noExt v):vs)
            L _ (HsParTy _ t')   -> unAppTy t'
            _                    -> builderError
#else
            L l (HsTyVar _p n) -> return (l, n, [])
            L _ (HsAppTy t1 (L lv (HsTyVar _ v))) -> do
              (l, n, vs) <- unAppTy t1
              return (l, n, L lv (UserTyVar v):vs)
            L _ (HsParTy t') -> unAppTy t'
            _                -> builderError
#endif
    (l, name, bndrs) <- unAppTy ty
    (meths,sigs) <- foldM categorize ([],[]) decls

    -- Note that the `bndrs' are gathered from left to right,
    -- re-ordering with reverse at this point.
    let bndrs' = reverse bndrs
        cls = ClassDecl { tcdCtxt = mkLocatedList tys
                        , tcdLName = name
                        , tcdFixity = Prefix
                        , tcdTyVars = mkHsQTvs bndrs'
                        , tcdFDs = []
                        , tcdSigs = mkClassOpSigs sigs
                        , tcdMeths = listToBag meths
                        , tcdATs = []
                        , tcdATDefs = []
                        , tcdDocs = []
#if MIN_VERSION_ghc(8,6,0)
                        , tcdCExt = noExt
#else
                        , tcdFVs = placeHolderNames
#endif
                        }
    return (L l (mkTyClD cls))

b_instD :: Maybe (Located OverlapMode) -> ([HType], HType)
        -> [HDecl] -> HDecl
b_instD overlap (ctxts,ty@(L l _)) decls =
  L l (instD (clsInstD decl))
  where
    decl = ClsInstDecl { cid_poly_ty = mkLHsSigType qty
                       , cid_binds = binds
                       , cid_sigs = mkClassOpSigs []
                       , cid_tyfam_insts = []
                       , cid_datafam_insts = []
                       , cid_overlap_mode = overlap
#if MIN_VERSION_ghc(8,6,0)
                       , cid_ext = noExt
#endif
                       }
    qty = L l (mkHsQualTy_compat (mkLocatedList ctxts) ty)
    (binds, _) = cvBindsAndSigs (toOL decls)
    instD = InstD NOEXT
    clsInstD = ClsInstD NOEXT

b_overlapP :: Code -> Maybe (Located OverlapMode)
b_overlapP (LForm (L _ lst)) =
  case mode of
    "OVERLAPPABLE" -> pragma Overlappable
    "OVERLAPPING"  -> pragma Overlapping
    "OVERLAPS"     -> pragma Overlaps
    "INCOHERENT"   -> pragma Incoherent
    _              -> Nothing
  where
    pragma con = Just (L l (con stxt))
    (List [(LForm (L l (Atom (ASymbol mode))))]) = lst
    -- XXX: Adding extra pragma comment header to support translation to
    -- Haskell source code.
    stxt = SourceText ("{-# " ++ unpackFS mode)

b_qtyclC :: [HType] -> Builder ([HType], HType)
b_qtyclC ts =
  case ts of
    []  -> builderError
    [_] -> builderError
    _   -> do
      let (ctxt,t) = splitAt (length ts - 1) ts
      return (ctxt, head t)

b_defaultD :: [HType] -> HDecl
b_defaultD types = L l (defD (defaultDecl types))
  where
    l = getLoc (mkLocatedList types)
    defD = DefD NOEXT
    defaultDecl = DefaultDecl NOEXT

b_fixityD :: FixityDirection -> Code -> [Code] -> HDecl
b_fixityD dir (LForm (L l (Atom (AInteger n)))) syms =
    L l (mkSigD_compat fsig)
  where
    fsig = fixSig (fixitySig names fixity)
    names = map lname syms
    lname (LForm (L l0 (Atom (ASymbol name)))) = L l0 (mkRdrName name)
    fixity = Fixity dir' (fromIntegral n) dir
    dir' = case dir of
             InfixL -> SourceText "infixl"
             InfixR -> SourceText "infixr"
             InfixN -> SourceText "infix"
    fixSig = FixSig NOEXT
    fixitySig = FixitySig NOEXT

b_ffiD :: Code -> Code -> HCCallConv -> (Maybe (Located Safety), Code)
       -> (Code, HType) -> Builder HDecl
b_ffiD (LForm (L l _)) imp_or_exp ccnv (mb_safety, ename) (nm, ty) =
  case unCode imp_or_exp of
    Atom (ASymbol ie)
      | ie == "import"
      , Just ispec <- parseCImport ccnv safety name ename' source -> do
        let fi = ForeignImport { fd_name = lname
                               , fd_sig_ty = tsig
#if MIN_VERSION_ghc(8,6,0)
                               , fd_i_ext = noExt
#else
                               , fd_co = noForeignImportCoercionYet
#endif
                               , fd_fi = ispec}
        return (L l (forD fi))
      | ie == "export" -> do
        let fe = ForeignExport { fd_name = lname
                               , fd_sig_ty = tsig
#if MIN_VERSION_ghc(8,6,0)
                               , fd_e_ext = noExt
#else
                               , fd_co = noForeignExportCoercionYet
#endif
                               , fd_fe = e }
            e = CExport (L l (CExportStatic (SourceText ename')
                                            (fsLit ename')
                                            (unLoc ccnv)))
                        (L l (SourceText ename'))
        return (L l (forD fe))
    _ -> builderError
    where
      lname = L ln (mkRdrName name)
      LForm (L ln (Atom (ASymbol name))) = nm
      tsig = mkLHsSigType ty
      LForm (L _ls (Atom (AString ename'))) = ename
      source = L l (quotedSourceText ename')
      safety = fromMaybe (noLoc PlayRisky) mb_safety
      forD = ForD NOEXT

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
  let body = mkGRHSs grhss decls l
#if MIN_VERSION_ghc(8,6,0)
      match = L l (Match noExt ctxt args body)
#elif MIN_VERSION_ghc(8,4,0)
      match = L l (Match ctxt args body)
#else
      match = L l (Match ctxt args Nothing body)
#endif
      ctxt = FunRhs { mc_fun = lrname
                    , mc_fixity = Prefix
                      -- XXX: Get strictness info from somewhere?
                    , mc_strictness = NoSrcStrict }
      lrname = L l (mkRdrName name)
      bind = mkFunBind lrname [match]
  in  L l (mkValD_compat bind)

b_patBindD :: ([HGRHS],[HDecl]) -> HPat -> HDecl
b_patBindD (grhss,decls) pat@(L l _) =
  let bind = PatBind { pat_lhs = pat
                     , pat_rhs = mkGRHSs grhss decls l
#if MIN_VERSION_ghc(8,6,0)
                     , pat_ext = noExt
#else
                     , pat_rhs_ty = placeHolderType
                     , bind_fvs = placeHolderNames
#endif
                     , pat_ticks = ([],[]) }
  in  L l (mkValD_compat bind)

b_tsigD :: [Code] -> ([HType], HType) -> HDecl
b_tsigD names (ctxts,typ) =
  let typ' = mkLHsSigWcType qtyp
      qtyp | null ctxts = typ
           | otherwise =
             L l (mkHsQualTy_compat (mkLocatedList ctxts) typ)
      mkName (LForm (L l1 (Atom (ASymbol name)))) =
        L l1 (mkRdrName name)
      l = getLoc (mkLocatedForm names)
      typeSig = TypeSig NOEXT
  in  L l (mkSigD_compat (typeSig (map mkName names) typ'))

b_inlineD :: InlineSpec -> Code -> HDecl
b_inlineD ispec (LForm (L l (Atom (ASymbol name)))) =
  L l (mkSigD_compat (inlineSig (L l (mkRdrName name)) ipragma))
  where
    inlineSig = InlineSig NOEXT
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
      specSig = SpecSig NOEXT
      ssig = specSig lname [mkLHsSigType tsig] ip
  return (L l (mkSigD_compat ssig))

mkTyClD :: TyClDecl PARSED -> HsDecl PARSED
mkTyClD = TyClD NOEXT

mkValD_compat :: HsBind PARSED -> HsDecl PARSED
mkValD_compat = ValD NOEXT

mkSigD_compat :: Sig PARSED -> HsDecl PARSED
mkSigD_compat = SigD NOEXT


-- ---------------------------------------------------------------------
--
-- Types
--
-- ---------------------------------------------------------------------

b_symT :: Code -> HType
b_symT (LForm (L l (Atom (ASymbol name)))) = L l tyvar
  where
    tyvar = mkHsTyVar_compat NotPromoted (L l ty)
    ty = case splitQualName name of
           Nothing
             | ',' == x  -> getRdrName (tupleTyCon Boxed arity)
             | otherwise -> mkUnqual namespace name
           Just qual -> mkQual namespace qual
    namespace
      | isUpper x || ':' == x = tcName
      | otherwise             = tvName
    x = headFS name
    arity = 1 + lengthFS name

b_unitT :: Code -> HType
b_unitT (LForm (L l _)) = L l (mkHsTupleTy_compat HsBoxedTuple [])

b_funT :: [HType] -> Builder HType
b_funT ts =
  case ts of
    []          -> builderError
    (L l0 _):_  -> return (L l0 (mkHsParTy_compat (foldr1 f ts)))
  where
    f a@(L l1 _) b = L l1 (hsFunTy a b)
    hsFunTy = HsFunTy NOEXT

b_appT :: [HType] -> HType
b_appT whole@(x:xs) =
  case xs of
    [] -> x
    _  -> L l0 (mkHsParTy_compat (mkHsAppTys x xs))
  where
    l0 = getLoc (mkLocatedList whole)

b_listT :: HType -> HType
b_listT ty@(L l _) = L l lty
  where
    lty = HsListTy NOEXT ty

b_nilT :: Code -> HType
b_nilT (LForm (L l _)) =
  L l (mkHsTyVar_compat NotPromoted (L l (getRdrName listTyCon)))

b_tupT :: Code -> [HType] -> HType
b_tupT (LForm (L l _)) ts =
  case ts of
   [] -> L l (mkHsTyVar_compat NotPromoted (L l tup))
     where
       tup = getRdrName (tupleTyCon Boxed 2)
   _  -> L l (mkHsTupleTy_compat HsBoxedTuple ts)

b_bangT :: Code -> HType -> HType
b_bangT (LForm (L l _)) t = L l (mkHsBangTy srcBang t)
  where
    srcBang = HsSrcBang (SourceText "b_bangT") NoSrcUnpack SrcStrict

b_forallT :: (Code, [Code]) -> ([HType], HType) -> HType
b_forallT ((LForm (L l0 _)), vars) (ctxts, body) = L l0 forAllTy
  where
    forAllTy = mkHsParTy_compat (L l0 forAllTy')
    forAllTy' = mkForAllTy_compat bndrs ty
    ty = L l0 (mkHsQualTy_compat (mkLocatedList ctxts) body)
    bndrs = map codeToUserTyVar vars

b_unpackT :: Code -> HType -> HType
b_unpackT (LForm (L l _)) t = L l (mkHsBangTy bang t')
  where
    bang = HsSrcBang (SourceText "b_unpackT") SrcUnpack strictness
    (strictness, t') =
      case t of
#if MIN_VERSION_ghc(8,6,0)
        L _ (HsBangTy _ (HsSrcBang _ _ st) t0) -> (st, t0)
#else
        L _ (HsBangTy (HsSrcBang _ _ st) t0) -> (st, t0)
#endif
        _                                    -> (NoSrcStrict, t)

mkHsTupleTy_compat :: HsTupleSort -> [HType] -> HsType PARSED
mkHsTupleTy_compat = HsTupleTy NOEXT

mkHsBangTy :: HsSrcBang -> HType -> HsType PARSED
mkHsBangTy = HsBangTy NOEXT

mkHsQualTy_compat :: LHsContext PARSED -> HType -> HsType PARSED
mkHsQualTy_compat ctxt body =
  HsQualTy { hst_ctxt = ctxt
#if MIN_VERSION_ghc(8,6,0)
           , hst_xqual = noExt
#endif
           , hst_body = body }

mkForAllTy_compat :: [LHsTyVarBndr PARSED] -> HType -> HsType PARSED
mkForAllTy_compat bndrs body =
  HsForAllTy { hst_bndrs = bndrs
#if MIN_VERSION_ghc(8,6,0)
             , hst_xforall = noExt
#endif
             , hst_body = body }

mkHsParTy_compat :: HType -> HsType PARSED
mkHsParTy_compat = HsParTy NOEXT

#if MIN_VERSION_ghc(8,4,0)
mkHsTyVar_compat :: Promoted -> Located (IdP PARSED) -> HsType PARSED
#else
mkHsTyVar_compat :: Promoted -> Located name -> HsType name
#endif
mkHsTyVar_compat = HsTyVar NOEXT


-- ---------------------------------------------------------------------
--
-- Pattern
--
-- ---------------------------------------------------------------------

b_intP :: Code -> HPat
b_intP (LForm (L l (Atom (AInteger n)))) =
  L l (mkNPat (L l (mkHsIntegral_compat n)) Nothing)

b_stringP :: Code -> HPat
b_stringP (LForm (L l (Atom (AString str)))) =
  L l (mkNPat (L l lit) Nothing)
  where
    lit = hsIsString (SourceText (show str)) (fsLit str)
    hsIsString s t =
#if MIN_VERSION_ghc(8,6,0)
      mkHsIsString s t
#else
      mkHsIsString s t placeHolderType
#endif

b_charP :: Code -> HPat
b_charP (LForm (L l (Atom (AChar c)))) = L l (litPat lit)
  where
    lit = HsChar (SourceText (show c)) c
    litPat = LitPat NOEXT

b_unitP :: Code -> HPat
b_unitP (LForm (L l (Atom AUnit))) = L l (mkTuplePat_compat [])

b_symP :: Code -> HPat
b_symP (LForm (L l (Atom (ASymbol name))))
   | name == "_"
    = L l mkWildPat_compat
   | isUpper x || x == ':'
    = L l (ConPatIn (L l (mkVarRdrName name)) (PrefixCon []))
   | otherwise
    = L l (varPat (L l (mkRdrName name)))
   where
     x = headFS name
     varPat = VarPat NOEXT

mkWildPat_compat :: Pat PARSED
mkWildPat_compat = case nlWildPat of L _ pat -> pat

b_hsListP :: [HPat] -> HPat
b_hsListP pats = L l (listPat pats)
  where
    l = getLoc (mkLocatedList pats)
    listPat ps =
#if MIN_VERSION_ghc(8,6,0)
      ListPat noExt ps
#else
      ListPat ps placeHolderType Nothing
#endif

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
b_tupP (LForm (L l _)) ps = L l (mkTuplePat_compat ps)

mkTuplePat_compat :: [HPat] -> Pat PARSED
mkTuplePat_compat ps = tuplePat ps Boxed
  where
    tuplePat pats boxity =
#if MIN_VERSION_ghc(8,6,0)
      TuplePat noExt pats boxity
#else
      TuplePat pats boxity []
#endif

b_asP :: Code -> HPat -> HPat
b_asP (LForm (L l (Atom (ASymbol name)))) pat =
  L l (asPat (L l (mkRdrName name)) (mkParPat_compat pat))
  where
    asPat = AsPat NOEXT

b_lazyP :: HPat -> HPat
b_lazyP pat@ (L l _) = mkParPat_compat (L l (lazyPat pat))
  where
    lazyPat = LazyPat NOEXT

b_conP :: Code -> [HPat] -> Builder HPat
b_conP (LForm (L l (Atom (ASymbol name)))) rest
  | isUpper x || x == ':'
    = return (mkParPat_compat
               (L l (ConPatIn (L l (mkVarRdrName name))
                              (PrefixCon rest))))
  | otherwise = builderError
  where
    x = headFS name

mkParPat_compat :: HPat -> HPat
mkParPat_compat (p @ (L l _)) = L l (parPat p)
  where
    parPat = ParPat NOEXT


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
b_tupE (LForm (L l _)) args = L l e
  where
    e = explicitTuple (map mkArg args) Boxed
    mkArg x@(L al _) = L al (present x)
    explicitTuple = ExplicitTuple NOEXT
    present = Present NOEXT

b_letE :: Code -> [HDecl] -> HExpr -> HExpr
b_letE (LForm (L l _)) decls body =
  let (mbs, sigs) = cvBindsAndSigs (toOL decls)
      valbinds = mkHsValBinds_compat mbs sigs
      hsLet = HsLet NOEXT
  in  L l (hsLet (L l valbinds) body)

b_caseE :: Code -> HExpr -> [HMatch] -> HExpr
b_caseE (LForm (L l _)) expr matches = L l (hsCase expr mg)
  where
    hsCase = HsCase NOEXT
    mg = mkMatchGroup FromSource matches

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

mkGRHSs :: [LGRHS PARSED t] -> [HDecl] -> SrcSpan -> GRHSs PARSED t
mkGRHSs grhss decls l = gRHSs grhss (declsToBinds l decls)
  where
    gRHSs = GRHSs NOEXT

b_hgrhs :: [HGRHS] -> (HExpr, [HGuardLStmt]) -> [HGRHS]
b_hgrhs rhss (body, gs) =
  let lrhs = case gs of
        [] -> noLoc rhs
        _  -> let l = getLoc (mkLocatedList gs) in L l rhs
      rhs = b_GRHS gs body
  in  (lrhs:rhss)

b_GRHS :: [HGuardLStmt] -> HExpr -> GRHS PARSED HExpr
b_GRHS = GRHS NOEXT

b_doE :: Code -> [HStmt] -> HExpr
b_doE (LForm (L l _)) exprs = L l (mkHsDo DoExpr exprs)

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
  L l (mkHsLit_compat (HsChar (SourceText (show x)) x))

b_stringE :: Code -> HExpr
b_stringE (LForm (L l (Atom (AString x)))) =
  L l (mkHsLit_compat (HsString (SourceText (show x)) (fsLit x)))

#if MIN_VERSION_ghc(8,4,0)
mkHsLit_compat :: HsLit PARSED -> HsExpr PARSED
#else
mkHsLit_compat :: HsLit -> HsExpr PARSED
#endif

mkHsLit_compat = HsLit NOEXT

b_integerE :: Code -> HExpr
b_integerE (LForm (L l (Atom (AInteger x))))
   | x < 0     = L l (mkHsPar_compat expr)
   | otherwise = expr
  where
    expr =     L l (mkHsOverLit_compat $! mkHsIntegral_compat x)

b_floatE :: Code -> HExpr
b_floatE (LForm (L l (Atom (AFractional x))))
  | fl_value x < 0 = L l (mkHsPar_compat expr)
  | otherwise      = expr
  where
    expr = L l (mkHsOverLit_compat $! mkHsFractional_compat x)

b_varE :: Code -> HExpr
b_varE (LForm (L l (Atom (ASymbol x)))) = L l (hsVar (L l rname))
  where
    hsVar = HsVar NOEXT
    rname = mkVarRdrName x

b_unitE :: Code -> HExpr
b_unitE (LForm (L l _)) =
  let L _ tuple = mkLHsTupleExpr []
  in  L l tuple

b_commentStringE :: Code -> Located HsDocString
b_commentStringE (LForm (L l (Atom (AComment x)))) =
  L l (mkHsDocString_compat x)

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
      valbinds = mkHsValBinds_compat mbs sigs
      letStmt = LetStmt NOEXT
  in  L l (letStmt (L l valbinds))

b_bodyS :: HExpr -> HStmt
b_bodyS expr = L (getLoc expr) (mkBodyStmt expr)

mkHsValBinds_compat :: HBinds -> [HSig] -> HsLocalBindsLR PARSED PARSED
mkHsValBinds_compat binds sigs =
#if MIN_VERSION_ghc(8,6,0)
  HsValBinds noExt (ValBinds noExt binds sigs)
#else
  HsValBinds (ValBindsIn binds sigs)
#endif


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
      _  -> mkHsValBinds_compat (listToBag binds) sigs
    -- Using 'RdrHsSyn.cvTopDecls' to group same names in where
    -- clause. Perhaps better to do similar things done in
    -- 'RdrHsSyn.cvBindGroup', which is dedicated for 'P' monad ...
    decls' = cvTopDecls (toOL decls)
    (binds, sigs) = go ([],[]) decls'
    go (bs,ss) ds =
      case ds of
        []    -> (bs, ss)
        d:ds' -> case d of
#if MIN_VERSION_ghc(8,6,0)
          L ld (ValD _ b) -> go (L ld b:bs,ss) ds'
          L ld (SigD _ s) -> go (bs,L ld s:ss) ds'
#else
          L ld (ValD b) -> go (L ld b:bs,ss) ds'
          L ld (SigD s) -> go (bs,L ld s:ss) ds'
#endif

-- Function defined in 'HsUtils', not exported.
mkLocatedList ::  [Located a] -> Located [Located a]
mkLocatedList [] = noLoc []
mkLocatedList ms = L (combineLocs (head ms) (last ms)) ms
{-# INLINE mkLocatedList #-}

-- | Convert record field constructor expression to record field update
-- expression.
cfld2ufld :: LHsRecField PARSED HExpr
          -> LHsRecUpdField PARSED
-- Almost same as 'mk_rec_upd_field' in 'RdrHsSyn'
#if MIN_VERSION_ghc(8,6,0)
cfld2ufld (L l0 (HsRecField (L l1 (FieldOcc _ rdr)) arg pun)) =
  L l0 (HsRecField (L l1 unambiguous) arg pun)
  where
    unambiguous = Unambiguous noExt rdr
#else
cfld2ufld (L l0 (HsRecField (L l1 (FieldOcc rdr _)) arg pun)) =
  L l0 (HsRecField (L l1 unambiguous) arg pun)
  where
    unambiguous = Unambiguous rdr PlaceHolder
#endif
{-# INLINE cfld2ufld #-}

-- | Make 'HsRecField' with given name and located data.
mkcfld :: (FastString, Located a) -> LHsRecField PARSED (Located a)
mkcfld (name, e@(L fl _)) =
  L fl HsRecField { hsRecFieldLbl = mkfname fl name
                  , hsRecFieldArg = e
                  , hsRecPun = False }
  where
    mkfname nl n = L nl (mkFieldOcc (L nl (mkRdrName n)))
{-# INLINE mkcfld #-}

quotedSourceText :: String -> SourceText
quotedSourceText s = SourceText $ "\"" ++ s ++ "\""
{-# INLINE quotedSourceText #-}

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
#if MIN_VERSION_ghc(8,6,0)
    go (L l (ValD _ d) : ds)
#else
    go (L l (ValD d) : ds)
#endif
      = let (b', ds') = getMonoBind (L l d) ds
            (bs, ss) = go ds'
        in  (b' `consBag` bs, ss)
#if MIN_VERSION_ghc(8,6,0)
    go (L l (SigD _ s) : ds)
#else
    go (L l (SigD s) : ds)
#endif
      = let (bs, ss) = go ds
        in  (bs, L l s : ss)

getMonoBind :: LHsBind PARSED -> [LHsDecl PARSED]
            -> (LHsBind PARSED, [LHsDecl PARSED])
getMonoBind (L loc1 (FunBind { fun_id = fun_id1@(L _ f1),
                               fun_matches
                                 = MG { mg_alts = L _ mtchs1 }}))
            binds
  | has_args mtchs1 = go mtchs1 loc1 binds
  where
    go mtchs loc
#if MIN_VERSION_ghc(8,6,0)
       (L loc2 (ValD _ (FunBind { fun_id = L _ f2,
                                  fun_matches
                                    = MG { mg_alts = L _ mtchs2 }}))

#else
       (L loc2 (ValD (FunBind { fun_id = L _ f2,
                                fun_matches
                                  = MG { mg_alts = L _ mtchs2 }}))
#endif
                : binds2)
      | f1 == f2 = go (mtchs2 ++ mtchs)
                      (combineSrcSpans loc loc2) binds2
    go mtchs loc binds2
      = (L loc (mkFunBind fun_id1 (reverse mtchs)), binds2)
      -- Reverse the final matches, to get it back in the right order

getMonoBind bind binds = (bind, binds)

-- Don't group together FunBinds if they have no arguments.  This is
-- necessary that variable bindings with no arguments are now treated as
-- FunBinds rather than pattern bindings.
has_args :: [LMatch PARSED (LHsExpr PARSED)] -> Bool
has_args (L _ mtch:_) = not (null (m_pats mtch))
has_args []                             =
  error "Language.SK.Syntax.Internal:has_args"

codeToUserTyVar :: Code -> HTyVarBndr
codeToUserTyVar code =
  case code of
    LForm (L l (Atom (ASymbol name))) ->
      L l (UserTyVar NOEXT (L l (mkUnqual tvName name)))
    _ -> error "Language.SK.Syntax.Internal:codeToUserTyVar"
{-# INLINE codeToUserTyVar #-}

-- | Auxiliary function to absorb version compatibiity of
-- 'mkHsIntegral'.
mkHsIntegral_compat :: Integer -> HsOverLit PARSED
mkHsIntegral_compat n =
#if MIN_VERSION_ghc(8,6,0)
  let il = IL { il_text = SourceText (show n)
              , il_neg = n < 0
              , il_value = n }
  in  mkHsIntegral il
#elif MIN_VERSION_ghc(8,4,0)
  let il = IL { il_text = SourceText (show n)
              , il_neg = n < 0
              , il_value = n }
  in  mkHsIntegral il placeHolderType
#else
  mkHsIntegral (SourceText (show n)) n placeHolderType
#endif
{-# INLINE mkHsIntegral_compat #-}
