{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Syntax for declaration.
module Language.SK.Syntax.HDecl where

-- base
import Data.Maybe (fromMaybe)

-- ghc
import BasicTypes ( Activation(..), Fixity(..),  FixityDirection(..)
                  , InlinePragma(..), InlineSpec(..), LexicalFixity(..)
                  , OverlapMode(..), PhaseNum, RuleMatchInfo(..)
                  , SourceText(..) )
import DataCon (SrcStrictness(..))
import FastString (FastString, fsLit, unpackFS)
import ForeignCall (CCallConv(..), CExportSpec(..), Safety(..))
import HsBinds (FixitySig(..), HsBind, HsBindLR(..), Sig(..))
import HsDecls ( ClsInstDecl(..), ConDecl (..), DataFamInstDecl(..)
               , DefaultDecl(..), DocDecl(..)
               , FamilyDecl (..), FamilyInfo(..), FamilyResultSig (..)
               , ForeignDecl(..), ForeignExport (..)
               , HsDataDefn(..), HsDecl(..), HsDerivingClause(..)
               , InstDecl(..), NewOrData(..), TyClDecl(..)
               , TyFamInstDecl(..), TyFamInstEqn )
import HsExpr (HsMatchContext(..), Match(..))
import HsTypes ( ConDeclField(..), HsConDetails(..), HsType(..)
               , HsTyVarBndr(..)
               , mkFieldOcc, mkHsImplicitBndrs, mkHsQTvs )
import HsUtils (mkClassOpSigs, mkFunBind, mkLHsSigType, mkLHsSigWcType)
import OccName (dataName, tcName)
import OrdList (toOL)
import Outputable (showSDocUnsafe)
import RdrHsSyn ( mkATDefault, mkConDeclH98, mkGadtDecl, mkInlinePragma
                , parseCImport )
import RdrName (RdrName, mkUnqual)
import SrcLoc (GenLocated(..), Located, getLoc, noLoc, unLoc)

#if MIN_VERSION_ghc(8,6,0)
import HsExtension (noExt)
#else
import HsDecls (noForeignImportCoercionYet, noForeignExportCoercionYet)
import PlaceHolder (PlaceHolder(..), placeHolderNames, placeHolderType)
#endif

#if MIN_VERSION_ghc(8,4,0)
import HsDecls (FamEqn(..))
#else
import HsDecls (TyFamEqn(..))
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

b_conD :: Code -> HConDeclDetails -> Builder HConDecl
b_conD form@(LForm (L l _)) details = do
  name <- getConId form
  let name' = L l (mkUnqual dataName name)
#if MIN_VERSION_ghc(8,6,0)
      cxt = Nothing
#else
      cxt = L l []
#endif
  return (L l (mkConDeclH98 name' Nothing cxt details))
{-# INLINE b_conD #-}

b_forallD :: [HTyVarBndr] -> (HConDecl, [HType]) -> HConDecl
b_forallD vars ((L l cdecl), cxts) =
#if MIN_VERSION_ghc(8,6,0)
  L l cdecl { con_forall = noLoc True
            , con_ex_tvs = vars
            , con_mb_cxt = Just (mkLocatedList cxts) }
#else
  L l cdecl { con_qvars = Just (mkHsQTvs vars)
            , con_cxt = Just (mkLocatedList cxts) }
#endif
{-# INLINE b_forallD #-}

b_gadtD :: Code -> ([HType], HType) -> Builder HConDecl
b_gadtD form@(LForm (L l1 _)) (ctxt, bodyty) = do
  name <- getConId form
  let name' = L l1 (mkUnqual dataName name)
      ty = L l1 qty
      qty = mkHsQualTy_compat (mkLocatedList ctxt) bodyty
#if MIN_VERSION_ghc(8,6,0)
      decl = fst (mkGadtDecl [name'] ty)
#else
      decl = mkGadtDecl [name'] (mkLHsSigType ty)
#endif
  return (L l1 decl)
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
    cd <- cvBindsAndSigs (toOL decls)
    let userTV = UserTyVar NOEXT
        kindedTV = KindedTyVar NOEXT
        -- Recursing in `HsAppTy' to support MultiParamTypeClasses.
        unAppTy t =
          case t of
            L l (HsTyVar _ _EXT n) ->
              return (l, n, [L l (userTV n)])
            L _ (HsAppTy _EXT t1 v) -> do
              (l1, n1, vs1) <- unAppTy t1
              (_, _, vs2) <- unAppTy v
              return (l1, n1, vs2 ++ vs1)
            L _ (HsParTy _EXT t')  -> unAppTy t'
            L l1 (HsKindSig _EXT t1 k) -> do
              (_, n, _) <- unAppTy t1
              return (l1, n, [L l1 (kindedTV n k)])
            _ -> builderError
        toATDef d = case mkATDefault d of
                      Right lty -> return lty
                      Left (_, sdoc) -> failB (showSDocUnsafe sdoc)
    (l, name, bndrs) <- unAppTy ty
    atdefs <- mapM toATDef (cd_tfis cd)
    -- Note that the `bndrs' are gathered from left to right,
    -- re-ordering with reverse and removing the duplicated head at this
    -- point.
    let bndrs' = tail (reverse bndrs)
        cls = ClassDecl { tcdCtxt = mkLocatedList tys
                        , tcdLName = name
                        , tcdFixity = Prefix
                        , tcdTyVars = mkHsQTvs bndrs'
                        , tcdFDs = []
                        , tcdSigs = mkClassOpSigs (cd_sigs cd)
                        , tcdMeths = cd_binds cd
                        , tcdATs = cd_fds cd
                        , tcdATDefs = atdefs
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
        -> [HDecl] -> Builder HDecl
b_instD overlap (ctxts,ty@(L l _)) decls = do
  cd <- cvBindsAndSigs (toOL decls)
  let decl = ClsInstDecl { cid_poly_ty = mkLHsSigType qty
                         , cid_binds = cd_binds cd
                         , cid_sigs = mkClassOpSigs (cd_sigs cd)
                         , cid_tyfam_insts = cd_tfis cd
                         , cid_datafam_insts = cd_dfis cd
                         , cid_overlap_mode = overlap
#if MIN_VERSION_ghc(8,6,0)
                         , cid_ext = noExt
#endif
                         }
      qty = L l (mkHsQualTy_compat (mkLocatedList ctxts) ty)
      instD = InstD NOEXT
      clsInstD = ClsInstD NOEXT
  return (L l (instD (clsInstD decl)))
{-# INLINE b_instD #-}

b_datafamD :: Code -> (FastString, [HTyVarBndr], Maybe HType) -> HDecl
b_datafamD = mkFamilyDecl DataFamily
{-# INLINE b_datafamD #-}

b_tyfamD :: [(Located FastString, [HType], HType)]
         -> Code
         -> (FastString, [HTyVarBndr], Maybe HType)
         -> HDecl
b_tyfamD insts
  | null insts = mkFamilyDecl OpenTypeFamily
  | otherwise  = mkFamilyDecl (ClosedTypeFamily (Just tfies))
  where
    tfies = map f insts
    f ((L l name), argtys, ty) =
      let rname = L l (mkUnqual tcName name)
      in  L l (mkTyFamInstEqn rname argtys ty)
{-# INLINE b_tyfamD #-}

-- See: "RdrHsSyn.mkFamDecl" and 'Convert.cvtDec'.
mkFamilyDecl :: FamilyInfo PARSED
             -> Code
             -> (FastString, [HTyVarBndr], Maybe HType)
             -> HDecl
mkFamilyDecl finfo (LForm (L l _)) (name, bndrs, mb_kind) =
  let fam = FamilyDecl
        { fdInfo = finfo
        , fdLName = lname
        , fdTyVars = hsqtyvars
        , fdFixity = Prefix
        , fdResultSig = L l rsig
        , fdInjectivityAnn = Nothing
#if MIN_VERSION_ghc(8,6,0)
        , fdExt = noExt
#endif
        }
      lname = L l (mkUnqual tcName name)
      hsqtyvars = mkHsQTvs bndrs
      rsig = case mb_kind of
                -- XXX: Does not ssupport 'TyVarsig'.
                Nothing   -> NoSig NOEXT
                Just lsig -> KindSig NOEXT lsig
  in  L l (TyClD NOEXT (FamDecl NOEXT fam))
{-# INLINE mkFamilyDecl #-}

-- See: "Convert.cvtDec".
b_datainstD :: Code
            -> (Located FastString, [HType])
            -> (HDeriving, [HConDecl])
            -> HDecl
b_datainstD = mk_data_or_newtype_instD DataType
{-# INLINE b_datainstD #-}

b_newtypeinstD :: Code
               -> (Located FastString, [HType])
               -> (HDeriving, [HConDecl])
               -> HDecl
b_newtypeinstD = mk_data_or_newtype_instD NewType
{-# INCLUDE b_newtypeinsD #-}

mk_data_or_newtype_instD :: NewOrData
                         -> Code
                         -> (Located FastString, [HType])
                         -> (HDeriving, [HConDecl])
                         -> HDecl
mk_data_or_newtype_instD new_or_data (LForm (L l _)) (L ln name, pats)
                         (deriv, condecls) =
  let faminst = DataFamInstD { dfid_inst = inst
#if MIN_VERSION_ghc(8,6,0)
                             , dfid_ext = noExt
#endif
                             }
      -- XXX: Contexts and kind signatures not supported.
      rhs = HsDataDefn { dd_ND = new_or_data
                       , dd_cType = Nothing
                       , dd_ctxt = L l []
                       , dd_kindSig = Nothing
                       , dd_cons = condecls
                       , dd_derivs = deriv
#if MIN_VERSION_ghc(8,6,0)
                       , dd_ext = noExt
#endif
                       }
      tycon = L ln (mkUnqual tcName name)
      inst = mkDataFamInstDecl tycon pats rhs
  in  L l (InstD NOEXT faminst)
{-# INLINE mk_data_or_newtype_instD #-}

b_tyinstD :: Code -> (Located FastString, [HType]) -> HType -> HDecl
b_tyinstD (LForm (L l _)) (L ln name, pats) rhs =
  let rname = L ln (mkUnqual tcName name)
      inst = mkTyFamInstEqn rname pats rhs
      tyfaminstD = TyFamInstD NOEXT tfid
#if MIN_VERSION_ghc(8,4,0)
      tfid = TyFamInstDecl inst
#else
      tfid = TyFamInstDecl { tfid_eqn = L l inst
                           , tfid_fvs = placeHolderNames }
#endif
  in  L l (InstD NOEXT tyfaminstD)
{-# INLINE b_tyinstD #-}

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
b_funBindD form@(LForm (L l _)) ((grhss,decls), args) = do
  name <- getVarId form
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
b_tsigD names (ctxts,typ0) = do
  let typ' = mkLHsSigWcType qtyp
      qtyp | null ctxts = typ1
           | otherwise =
             L l (mkHsQualTy_compat (mkLocatedList ctxts) typ1)
      typ1 = unParTy typ0
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

b_inlineD :: InlineSpec -> Maybe Activation -> Code -> Builder HDecl
b_inlineD ispec mb_act (LForm (L l form))
  | Atom (ASymbol name) <- form
  = let inlineSig = InlineSig NOEXT
    in  return (L l (sigD (inlineSig (L l (mkRdrName name)) ipragma)))
  | otherwise
  = builderError
  where
    ipragma = mkInlinePragma (SourceText source) (ispec, FunLike) mb_act
    source = case ispec of
               NoInline  -> "{-# NOINLINE"
               Inlinable -> "{-# INLINABLE"
               _         -> "{-# INLINE"
{-# INLINE b_inlineD #-}

b_activation :: (SourceText -> PhaseNum -> Activation)
             -> Code -> Builder Activation
b_activation f code@(LForm (L _l atom))
  | Atom (AInteger n) <- atom = return (f source (fromIntegral n))
  -- Supporting symbols in "~N" form, where "N" is an integer.
  | Atom (ASymbol s) <- atom
  , '~':rest <- unpackFS s
  , [(n,"")] <- reads rest = return (f source n)
  | otherwise = builderError
  where
    source = SourceText (show code)
{-# INLINE b_activation #-}

b_specializeD :: Code -> Maybe Activation -> (Code, HType) -> Builder HDecl
b_specializeD = specializeBuilder noUserInline "{-# SPECIALISE"
{-# INLINE b_specializeD #-}

b_specializeInlineD :: Code -> Maybe Activation -> (Code, HType)
                    -> Builder HDecl
b_specializeInlineD = specializeBuilder Inline "{-# SPECIALISE INLINE"
{-# INLINE b_specializeInlineD #-}

specializeBuilder :: InlineSpec
                  -> String
                  -> Code -> Maybe Activation -> (Code, HType)
                  -> Builder HDecl
specializeBuilder ispec txt (LForm (L l _)) mb_act (nsym, tsig) = do
  let LForm (L ln (Atom (ASymbol name))) = nsym
      lname = L ln (mkRdrName name)
      ipragma = mkInlinePragma source (ispec, FunLike) mb_act
      source = SourceText txt
      specSig = SpecSig NOEXT lname [mkLHsSigType tsig] ipragma
  return (L l (sigD specSig))
{-# INLINE specializeBuilder #-}

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

b_docGroupD :: Int -> Code -> Builder HDecl
b_docGroupD n form@(LForm (L l _))
  | List [_,doc_code] <- unCode form
  , Atom (AString doc) <- unCode doc_code
  = return $! L l (DocD NOEXT (DocGroup (fromIntegral n)
                                        (hsDocString doc)))
  | otherwise = setLastToken form >> failB "Invalid group doc"
{-# INLINE b_docGroupD #-}

b_docNamed :: Code -> Builder HDecl
b_docNamed form@(LForm (L l body))
  | List [_,name_code,doc_code] <- body
  , Atom (ASymbol name) <- unCode name_code
  , Atom (AString doc) <- unCode doc_code
  = let name' = unpackFS name
        doc' = hsDocString doc
    in return $! L l (DocD NOEXT (DocCommentNamed name' doc'))
  | otherwise
  = setLastToken form >> failB "Invalid named doc"
{-# INLINE b_docNamed #-}

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

mkDataFamInstDecl :: Located RdrName
                  -> [HType]
                  -> HsDataDefn PARSED
                  -> DataFamInstDecl PARSED
mkDataFamInstDecl tycon pats rhs =
#if MIN_VERSION_ghc(8,4,0)
  DataFamInstDecl (mkHsImplicitBndrs feqn)
  where
    feqn = FamEqn { feqn_tycon = tycon
                  , feqn_pats = pats
                  , feqn_fixity = Prefix
                  , feqn_rhs = rhs
#if MIN_VERSION_ghc(8,6,0)
                  , feqn_ext = noExt
#endif
                  }
#else
  DataFamInstDecl { dfid_tycon = tycon
                  , dfid_pats = mkHsImplicitBndrs pats
                  , dfid_fixity = Prefix
                  , dfid_defn = rhs
                  , dfid_fvs = placeHolderNames }
#endif
{-# INLINE mkDataFamInstDecl #-}

mkTyFamInstEqn :: Located RdrName
               -> [HType]
               -> HType
               -> TyFamInstEqn PARSED
mkTyFamInstEqn tycon pats rhs =
#if MIN_VERSION_ghc(8,4,0)
  let fameqn = FamEqn { feqn_tycon = tycon
#if MIN_VERSION_ghc(8,6,0)
                      , feqn_ext = noExt
#endif
                      , feqn_pats = pats
                      , feqn_fixity = Prefix
                      , feqn_rhs = rhs }
  in  mkHsImplicitBndrs fameqn
#else
  TyFamEqn { tfe_tycon = tycon
           , tfe_pats = mkHsImplicitBndrs pats
           , tfe_fixity = Prefix
           , tfe_rhs = rhs }
#endif
{-# INLINE mkTyFamInstEqn #-}

unParTy :: HType -> HType
unParTy t0 =
  case t0 of
    L _ (HsParTy _EXT t1) -> t1
    _                       -> t0
{-# INLINE unParTy #-}

noUserInline :: InlineSpec
#if MIN_VERSION_ghc (8,4,0)
noUserInline = NoUserInline
#else
noUserInline = EmptyInlineSpec
#endif
