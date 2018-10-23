{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
-- | Syntax for declaration.
module Language.SK.Syntax.HDecl where

-- base
import Control.Monad (foldM)
import Data.Maybe (fromMaybe)

-- ghc
import Bag (listToBag)
import BasicTypes ( Fixity(..),  FixityDirection(..), InlinePragma(..)
                  , InlineSpec(..), LexicalFixity(..)
                  , OverlapMode(..), SourceText(..)
                  , alwaysInlinePragma, defaultInlinePragma )
import DataCon (SrcStrictness(..))
import FastString (FastString, unpackFS)
import ForeignCall (CCallConv(..), CExportSpec(..), Safety(..))
import HsBinds (FixitySig(..), HsBind, HsBindLR(..), Sig(..))
import HsDecls ( ClsInstDecl(..), ConDecl (..), DefaultDecl(..)
               , ForeignDecl(..), ForeignExport (..), HsDataDefn(..)
               , HsDecl(..), HsDerivingClause(..), InstDecl(..)
               , NewOrData(..), TyClDecl(..) )
import HsExpr (HsMatchContext(..), Match(..))
import HsTypes ( ConDeclField(..), HsConDetails(..), HsType(..)
               , HsTyVarBndr(..), mkFieldOcc, mkHsQTvs )
import HsUtils (mkClassOpSigs, mkFunBind, mkLHsSigType, mkLHsSigWcType)
import OccName (srcDataName, tcName)
import OrdList (toOL)
import RdrHsSyn (mkConDeclH98, mkGadtDecl, parseCImport)
import RdrName (mkUnqual)
import SrcLoc (Located, noLoc)

#if MIN_VERSION_ghc(8,6,0)
import HsExtension (noExt)
#else
import HsDecls (noForeignImportCoercionYet, noForeignExportCoercionYet)
import PlaceHolder (PlaceHolder(..), placeHolderNames, placeHolderType)
#endif

-- Internal
import Language.SK.Builder
import Language.SK.Form
import Language.SK.Syntax.SynUtils

#include "Syntax.h"

-- ---------------------------------------------------------------------
--
-- Declarations
--
-- ---------------------------------------------------------------------

b_dataD :: Code
        -> (FastString, [HTyVarBndr])
        -> (HDeriving, [HConDecl])
        -> HDecl
b_dataD = mkNewtypeOrDataD DataType
{-# INLINE b_dataD #-}

b_newtypeD :: Code -> (FastString, [HTyVarBndr])
           -> (HDeriving, [HConDecl])
           -> HDecl
b_newtypeD = mkNewtypeOrDataD NewType
{-# INLINE b_newtypeD #-}

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
{-# INLINE mkNewtypeOrDataD #-}

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
{-# INLINE b_typeD #-}

b_simpletypeD :: [Code] -> (FastString, [HTyVarBndr])
b_simpletypeD ((LForm (L _ (Atom (ASymbol name)))):tvs) = (name, tvs')
  -- XXX: Kind signatures not supported.
  where
    tvs' = map codeToUserTyVar tvs
{-# INLINE b_simpletypeD #-}

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
{-# INLINE b_conD #-}

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
{-# INLINE b_forallD #-}

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
{-# INLINE b_gadtD #-}

b_conOnlyD :: Code -> HConDecl
b_conOnlyD name = b_conD name (PrefixCon [])
{-# INLINE b_conOnlyD #-}

-- XXX: Infix data constructor not supported.
b_conDeclDetails :: [HType] -> HConDeclDetails
b_conDeclDetails = PrefixCon
{-# INLINE b_conDeclDetails #-}

b_recFieldsD :: [HConDeclField] -> HConDeclDetails
b_recFieldsD flds = RecCon (mkLocatedList flds)
{-# INLINE b_recFieldsD #-}

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
{-# INLINE b_recFieldD #-}

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
{-# INLINE b_derivD #-}

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
{-# INLINE b_classD #-}

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
{-# INLINE b_instD #-}

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
{-# INLINE b_overlapP #-}

b_qtyclC :: [HType] -> Builder ([HType], HType)
b_qtyclC ts =
  case ts of
    []  -> builderError
    [_] -> builderError
    _   -> do
      let (ctxt,t) = splitAt (length ts - 1) ts
      return (ctxt, head t)
{-# INLINE b_qtyclC #-}

b_defaultD :: [HType] -> HDecl
b_defaultD types = L l (defD (defaultDecl types))
  where
    l = getLoc (mkLocatedList types)
    defD = DefD NOEXT
    defaultDecl = DefaultDecl NOEXT
{-# INLINE b_defaultD #-}

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
{-# INLINE b_fixityD #-}

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
      -- source = L l (quotedSourceText ename')
      source = L l (SourceText ("\"" ++ ename' ++ "\""))
      safety = fromMaybe (noLoc PlayRisky) mb_safety
      forD = ForD NOEXT
{-# INLINE b_ffiD #-}

b_callConv :: Code -> Builder (Located CCallConv)
b_callConv (LForm (L l (Atom (ASymbol sym)))) =
  case sym of
    "capi"       -> return (L l CApiConv)
    "ccall"      -> return (L l CCallConv)
    "prim"       -> return (L l PrimCallConv)
    "javascript" -> return (L l JavaScriptCallConv)
    "stdcall"    -> return (L l StdCallConv)
    _            -> builderError
{-# INLINE b_callConv #-}

b_safety :: Code -> Builder (Located Safety)
b_safety (LForm (L l (Atom (ASymbol sym)))) =
  case sym of
    "interruptible" -> return (L l PlayInterruptible)
    "safe"          -> return (L l PlaySafe)
    "unsafe"        -> return (L l PlayRisky)
    _               -> builderError
{-# INLINE b_safety #-}

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
{-# INLINE b_funBindD #-}

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
{-# INLINE b_patBindD #-}

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
{-# INLINE b_tsigD #-}

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
{-# INLINE b_inlineD #-}

b_specializeD :: Code -> (Code, HType) -> Builder HDecl
b_specializeD (LForm (L l _)) (nameSym, tsig) = do
  let LForm (L ln (Atom (ASymbol name))) = nameSym
      lname = L ln (mkRdrName name)
      ip = defaultInlinePragma {inl_src = SourceText "{-# SPECIALIZE"}
      specSig = SpecSig NOEXT
      ssig = specSig lname [mkLHsSigType tsig] ip
  return (L l (mkSigD_compat ssig))
{-# INLINE b_specializeD #-}

mkTyClD :: TyClDecl PARSED -> HsDecl PARSED
mkTyClD = TyClD NOEXT
{-# INLINE mkTyClD #-}

mkValD_compat :: HsBind PARSED -> HsDecl PARSED
mkValD_compat = ValD NOEXT
{-# INLINE mkValD_compat #-}

mkSigD_compat :: Sig PARSED -> HsDecl PARSED
mkSigD_compat = SigD NOEXT
{-# INLINE mkSigD_compat #-}
