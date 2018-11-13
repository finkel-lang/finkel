{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
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
               , DocDecl(..), ForeignDecl(..), ForeignExport (..)
               , HsDataDefn(..), HsDecl(..), HsDerivingClause(..)
               , InstDecl(..), NewOrData(..), TyClDecl(..) )
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
  L l (tyClD decl)
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
b_typeD (LForm (L l _)) (name, tvs) ty = L l (tyClD synonym)
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

b_simpletypeD :: [Code] -> Builder (FastString, [HTyVarBndr])
b_simpletypeD codes
  | ((LForm (L _ (Atom (ASymbol name)))):tvs) <- codes
  -- XXX: Kind signatures not supported.
  = return (name, map codeToUserTyVar tvs)
  | otherwise
  = builderError
{-# INLINE b_simpletypeD #-}

b_conD :: Code -> HConDeclDetails -> Builder HConDecl
b_conD form details
 | (LForm (L l (Atom (ASymbol name)))) <- form
 = let name' = L l (mkUnqual srcDataName name)
#if MIN_VERSION_ghc(8,6,0)
       cxt = Nothing
#else
       cxt = L l []
#endif
   in  return (L l (mkConDeclH98 name' Nothing cxt details))
 | otherwise
 = builderError
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

b_gadtD :: Code -> ([HType], HType) -> Builder HConDecl
b_gadtD (LForm (L l1 form)) (ctxt, bodyty)
  | Atom (ASymbol name) <- form
  = let name' = L l1 (mkUnqual srcDataName name)
        ty = L l1 qty
        qty = mkHsQualTy_compat (mkLocatedList ctxt) bodyty
#if MIN_VERSION_ghc(8,6,0)
        decl = fst (mkGadtDecl [name'] ty)
#else
        decl = mkGadtDecl [name'] (mkLHsSigType ty)
#endif
    in  return (L l1 decl)
  | otherwise = builderError
{-# INLINE b_gadtD #-}

b_conOnlyD :: Code -> Builder HConDecl
b_conOnlyD name = b_conD name (PrefixCon [])
{-# INLINE b_conOnlyD #-}

-- XXX: Infix data constructor not supported.
b_conDeclDetails :: [HType] -> HConDeclDetails
b_conDeclDetails = PrefixCon
{-# INLINE b_conDeclDetails #-}

b_recFieldsD :: [HConDeclField] -> HConDeclDetails
b_recFieldsD flds = RecCon (mkLocatedList flds)
{-# INLINE b_recFieldsD #-}

b_recFieldD :: [Code] -> HType -> Builder HConDeclField
b_recFieldD names ty = do
  let f (LForm (L l form))
        | Atom (ASymbol name) <- form
        = return (L l (mkFieldOcc (L l (mkRdrName name))))
        | otherwise
        = builderError
  names' <- mapM f names
  let field = ConDeclField { cd_fld_names = names'
#if MIN_VERSION_ghc(8,6,0)
                           , cd_fld_ext = noExt
#endif
                           , cd_fld_type = ty
                           , cd_fld_doc = Nothing }
      loc = getLoc (mkLocatedForm names)
  return (L loc field)
{-# INLINE b_recFieldD #-}

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
            SigD _EXT d -> return (ms, L ld d : ss)
            ValD _EXT d -> return (L ld d : ms, ss)
            _           -> builderError
        userTyVar = UserTyVar NOEXT
        -- Recursing in `HsAppTy' to support MultiParamTypeClasses.
        unAppTy t =
          case t of
            L l (HsTyVar _ _EXT n) -> return (l, n, [])
            L _ (HsAppTy _EXT t1 (L lv (HsTyVar _ _EXT v))) ->
              do (l, n, vs) <- unAppTy t1
                 return (l, n, L lv (userTyVar v):vs)
            L _ (HsParTy _EXT t')  -> unAppTy t'
            _                      -> builderError
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
    return (L l (tyClD cls))
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

b_fixityD :: FixityDirection -> Code -> [Code] -> Builder HDecl
b_fixityD dir (LForm (L l form)) syms
  | Atom (AInteger n) <- form = do
    let lname (LForm (L l0 x))
          | (Atom (ASymbol name)) <- x
          = return (L l0 (mkRdrName name))
          | otherwise
          = builderError
        fixity = Fixity dir' (fromIntegral n) dir
        dir' = case dir of
                 InfixL -> SourceText "infixl"
                 InfixR -> SourceText "infixr"
                 InfixN -> SourceText "infix"
        fixSig = FixSig NOEXT
        fixitySig = FixitySig NOEXT
    names <- mapM lname syms
    return (L l (sigD (fixSig (fixitySig names fixity))))
  | otherwise = builderError
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
      source = L l (quotedSourceText ename')
      safety = fromMaybe (noLoc PlayRisky) mb_safety
      forD = ForD NOEXT
{-# INLINE b_ffiD #-}

b_callConv :: Code -> Builder (Located CCallConv)
b_callConv (LForm (L l form))
  | Atom (ASymbol sym) <- form =
    case sym of
      "capi"       -> return (L l CApiConv)
      "ccall"      -> return (L l CCallConv)
      "prim"       -> return (L l PrimCallConv)
      "javascript" -> return (L l JavaScriptCallConv)
      "stdcall"    -> return (L l StdCallConv)
      _            -> builderError
  | otherwise = builderError
{-# INLINE b_callConv #-}

b_safety :: Code -> Builder (Located Safety)
b_safety (LForm (L l form))
  | Atom (ASymbol sym) <- form =
    case sym of
      "interruptible" -> return (L l PlayInterruptible)
      "safe"          -> return (L l PlaySafe)
      "unsafe"        -> return (L l PlayRisky)
      _               -> builderError
  | otherwise = builderError
{-# INLINE b_safety #-}

b_funBindD :: Code -> (([HGRHS],[HDecl]), [HPat]) -> Builder HDecl
b_funBindD (LForm (L l form)) ((grhss,decls), args)
  | Atom (ASymbol name) <- form = do
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
                        -- XXX: Get strictness info from ... where?
                      , mc_strictness = NoSrcStrict }
        lrname = L l (mkRdrName name)
        bind = mkFunBind lrname [match]
    return (L l (valD bind))
  | otherwise = builderError
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
  in  L l (valD bind)
{-# INLINE b_patBindD #-}

b_tsigD :: [Code] -> ([HType], HType) -> Builder HDecl
b_tsigD names (ctxts,typ) = do
  let typ' = mkLHsSigWcType qtyp
      qtyp | null ctxts = typ
           | otherwise =
             L l (mkHsQualTy_compat (mkLocatedList ctxts) typ)
      mkName form
        | (LForm (L l1 (Atom (ASymbol name)))) <- form
        = return (L l1 (mkRdrName name))
        | otherwise
        = builderError
      l = getLoc (mkLocatedForm names)
      typeSig = TypeSig NOEXT
  names' <- mapM mkName names
  return (L l (sigD (typeSig names' typ')))
{-# INLINE b_tsigD #-}

b_inlineD :: InlineSpec -> Code -> Builder HDecl
b_inlineD ispec (LForm (L l form))
  | Atom (ASymbol name) <- form
  = let inlineSig = InlineSig NOEXT
    in  return (L l (sigD (inlineSig (L l (mkRdrName name)) ipragma)))
  | otherwise
  = builderError
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
{-# INLINE b_inlineD #-}

b_specializeD :: Code -> (Code, HType) -> Builder HDecl
b_specializeD (LForm (L l _)) (nameSym, tsig) = do
  let LForm (L ln (Atom (ASymbol name))) = nameSym
      lname = L ln (mkRdrName name)
      ip = defaultInlinePragma {inl_src = SourceText "{-# SPECIALIZE"}
      specSig = SpecSig NOEXT
      ssig = specSig lname [mkLHsSigType tsig] ip
  return (L l (sigD ssig))
{-# INLINE b_specializeD #-}

b_docnextD :: Code -> Builder HDecl
b_docnextD (LForm (L l form))
  | Atom (AString str) <- form =
    return $! L l (DocD NOEXT (docCommentNext str))
  | otherwise                  = builderError
{-# INLINE b_docnextD #-}

b_docprevD :: Code -> Builder HDecl
b_docprevD (LForm (L l form))
  | Atom (AString str) <- form =
    return $! L l (DocD NOEXT (DocCommentPrev (hsDocString str)))
  | otherwise                  = builderError
{-# INLINE b_docprevD #-}

docCommentNext :: String -> DocDecl
docCommentNext = DocCommentNext . hsDocString
{-# INLINE docCommentNext #-}

tyClD :: TyClDecl PARSED -> HsDecl PARSED
tyClD = TyClD NOEXT
{-# INLINE tyClD #-}

valD :: HsBind PARSED -> HsDecl PARSED
valD = ValD NOEXT
{-# INLINE valD #-}

sigD :: Sig PARSED -> HsDecl PARSED
sigD = SigD NOEXT
{-# INLINE sigD #-}
