{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
-- | Syntax for declaration.
module Language.Finkel.Syntax.HDecl where

#include "Syntax.h"
#include "ghc_modules.h"

-- base
import Data.Maybe                      (fromMaybe)

-- ghc
import GHC_Core_DataCon                (SrcStrictness (..))
import GHC_Data_FastString             (FastString, unpackFS)
import GHC_Data_OrdList                (toOL)
import GHC_Hs_Binds                    (FixitySig (..), HsBind, HsBindLR (..),
                                        Sig (..))
import GHC_Hs_Decls                    (ClsInstDecl (..), ConDecl (..),
                                        DataFamInstDecl (..), DefaultDecl (..),
                                        DerivDecl (..), DocDecl (..),
                                        FamilyDecl (..), FamilyInfo (..),
                                        FamilyResultSig (..), ForeignDecl (..),
                                        ForeignExport (..), HsDataDefn (..),
                                        HsDecl (..), HsDerivingClause (..),
                                        InstDecl (..), NewOrData (..),
                                        TyClDecl (..), TyFamInstDecl (..),
                                        TyFamInstEqn)
import GHC_Hs_Doc                      (LHsDocString)
import GHC_Hs_Expr                     (HsMatchContext (..), Match (..))
import GHC_Hs_Pat                      (Pat (..))
import GHC_Hs_Type                     (ConDeclField (..), HsConDetails (..),
                                        HsTyVarBndr (..), HsType (..),
                                        HsWildCardBndrs (..), mkFieldOcc,
                                        mkHsImplicitBndrs, mkHsQTvs)
import GHC_Hs_Utils                    (mkClassOpSigs, mkFunBind, mkLHsSigType,
                                        mkLHsSigWcType)
import GHC_Parser_PostProcess          (mkConDeclH98, mkGadtDecl,
                                        mkInlinePragma, parseCImport)
import GHC_Types_Basic                 (Activation (..), Fixity (..),
                                        FixityDirection (..), InlineSpec (..),
                                        LexicalFixity (..), OverlapMode (..),
                                        PhaseNum, RuleMatchInfo (..),
                                        SourceText (..))
import GHC_Types_ForeignCall           (CCallConv (..), CExportSpec (..),
                                        Safety (..))
import GHC_Types_Name_Occurrence       (dataName, tcName)
import GHC_Types_Name_Reader           (RdrName, mkUnqual)
import GHC_Types_SrcLoc                (GenLocated (..), Located, getLoc, noLoc,
                                        unLoc)

#if MIN_VERSION_ghc(9,0,0)
import GHC_Hs_Type                     (HsArrow (..), HsScaled (..),
                                        LHsTyVarBndr)
import GHC_Parser_Annotation           (IsUnicodeSyntax (..))
import GHC_Parser_Lexer                (P (..), ParseResult (..))
import GHC_Types_SrcLoc                (LayoutInfo (..))
import GHC_Types_Var                   (Specificity (..))
#endif

#if MIN_VERSION_ghc(8,10,0)
import GHC_Hs_Decls                    (LTyFamDefltDecl)
import GHC_Types_Basic                 (Origin (..))
#else
import GHC_Hs_Decls                    (LTyFamDefltEqn)
import Outputable                      (showSDocUnsafe)
import RdrHsSyn                        (mkATDefault)
#endif

#if MIN_VERSION_ghc(8,10,0)
import GHC_Hs_Extension                (noExtField)
#elif MIN_VERSION_ghc(8,6,0)
import GHC_Hs_Extension                (noExt)
#else
import GHC_Hs_Decls                    (noForeignExportCoercionYet,
                                        noForeignImportCoercionYet)
import PlaceHolder                     (PlaceHolder (..), placeHolderNames,
                                        placeHolderType)
#endif

#if MIN_VERSION_ghc(8,10,0)
import GHC_Hs_Decls                    (FamEqn (..))
#elif MIN_VERSION_ghc(8,4,0)
import GHC_Hs_Decls                    (FamEqn (..), HsTyPats)
#else
import GHC_Hs_Decls                    (TyFamEqn (..))
#endif

#if MIN_VERSION_ghc(8,8,0)
import GHC_Hs_Type                     (HsArg (..))
#endif

#if MIN_VERSION_ghc(8,6,0)
import GHC_Hs_Decls                    (DerivStrategy (..))
#endif

-- Internal
import Language.Finkel.Builder
import Language.Finkel.Form
import Language.Finkel.Syntax.HType
import Language.Finkel.Syntax.SynUtils

#if !MIN_VERSION_ghc(8,6,0)
import Language.Finkel.Syntax.HPat
#endif

-- ---------------------------------------------------------------------
--
-- Declarations
--
-- ---------------------------------------------------------------------

b_dataD :: Code
        -> (FastString, [HTyVarBndr], Maybe HKind)
        -> (HDeriving, [HConDecl])
        -> HDecl
b_dataD = mkNewtypeOrDataD DataType
{-# INLINABLE b_dataD #-}

b_newtypeD :: Code
           -> (FastString, [HTyVarBndr], Maybe HKind)
           -> (HDeriving, [HConDecl])
           -> HDecl
b_newtypeD = mkNewtypeOrDataD NewType
{-# INLINABLE b_newtypeD #-}

mkNewtypeOrDataD :: NewOrData
                 -> Code
                 -> (FastString, [HTyVarBndr], Maybe HKind)
                 -> (HDeriving, [HConDecl])
                 -> HDecl
mkNewtypeOrDataD newOrData (LForm (L l _)) (name, tvs, ksig) (derivs, cs) =
  L l (tyClD decl)
  where
    decl = DataDecl { tcdLName = L l (mkUnqual tcName name)
                    , tcdFixity = Prefix
                    , tcdTyVars = mkHsQTvs tvs
                    , tcdDataDefn = defn
#if MIN_VERSION_ghc(8,6,0)
                    , tcdDExt = NOEXT
#else
                    , tcdDataCusk = PlaceHolder
                    , tcdFVs = placeHolderNames
#endif
                    }
    defn = HsDataDefn { dd_ND = newOrData
                      , dd_ctxt = noLoc []
                      , dd_cType = Nothing
                      , dd_kindSig = ksig
                      , dd_cons = cs
                      , dd_derivs = derivs
#if MIN_VERSION_ghc(8,6,0)
                      , dd_ext = NOEXT
#endif
                      }
{-# INLINABLE mkNewtypeOrDataD #-}

b_typeD :: Code
        -> (FastString, [HTyVarBndr], Maybe HKind)
        -> HType
        -> HDecl
b_typeD (LForm (L l _)) (name, tvs, _) ty = L l (tyClD synonym)
  where
    synonym = SynDecl { tcdLName = L l (mkUnqual tcName name)
                      , tcdFixity = Prefix
                      , tcdTyVars = mkHsQTvs tvs
                      , tcdRhs = ty
#if MIN_VERSION_ghc(8,6,0)
                      , tcdSExt = NOEXT
#else
                      , tcdFVs = placeHolderNames
#endif
                      }
{-# INLINABLE b_typeD #-}

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
{-# INLINABLE b_conD #-}

b_qtyconD :: (HConDecl, [HType]) -> HConDecl
b_qtyconD (whole@(L l decl), tys) =
  case tys of
    [] -> whole
#if MIN_VERSION_ghc(8,6,0)
    _  -> L l (decl { con_mb_cxt = Just (mkLocatedList tys) })
#else
    _  -> L l (decl { con_cxt = Just (mkLocatedList tys) })
#endif
{-# INLINABLE b_qtyconD #-}

#if MIN_VERSION_ghc(9,0,0)
b_forallD :: [LHsTyVarBndr Specificity PARSED] -> (HConDecl, [HType]) -> HConDecl
#else
b_forallD :: [HTyVarBndr] -> (HConDecl, [HType]) -> HConDecl
#endif
b_forallD vars (L l cdecl, cxts) =
#if MIN_VERSION_ghc(8,6,0)
  L l cdecl { con_forall = noLoc True
            , con_ex_tvs = vars
            , con_mb_cxt = Just (mkLocatedList cxts) }
#else
  L l cdecl { con_qvars = Just (mkHsQTvs vars)
            , con_cxt = Just (mkLocatedList cxts) }
#endif
{-# INLINABLE b_forallD #-}

b_gadtD :: Code -> ([HType], HType) -> Builder HConDecl
b_gadtD form@(LForm (L l1 _)) (ctxt, bodyty) = do
  name <- getConId form
  let name' = L l1 (mkUnqual dataName name)
#if MIN_VERSION_ghc(9,0,0)
      -- Removing parentheses of the body type, so that the 'mkGadtDecl' can
      -- split the internal elements. Parentheses are added to the body of GADT
      -- when it is HsForAllTy, to support documentation string.
      ty = case qty of
             HsParTy _ unpar_ty -> unpar_ty
             _                  -> L l1 qty

#else
      ty = L l1 qty
#endif
      qty = mkHsQualTy_compat (mkLocatedList ctxt) bodyty
#if MIN_VERSION_ghc(9,0,0)
  decl <-
    do ps <- fmap ghcPState getBState
       case unP (mkGadtDecl [name'] ty) ps of
         POk _ d -> return (fst d)
         _       -> builderError
#elif MIN_VERSION_ghc(8,6,0)
  let decl = fst (mkGadtDecl [name'] ty)
#else
  let decl = mkGadtDecl [name'] (mkLHsSigType ty)
#endif
  return (L l1 decl)
{-# INLINABLE b_gadtD #-}

b_conOnlyD :: Code -> Builder HConDecl
b_conOnlyD name = b_conD name (PrefixCon [])
{-# INLINABLE b_conOnlyD #-}

-- XXX: Infix data constructor not supported.
b_conDeclDetails :: [HType] -> HConDeclDetails
#if MIN_VERSION_ghc(9,0,0)
b_conDeclDetails = PrefixCon . map (hsScaled . parTyApp)
  where
    -- XXX: Does not support liner types and unicode syntax.
    hsScaled = HsScaled (HsUnrestrictedArrow NormalSyntax)
#else
b_conDeclDetails = PrefixCon . map parTyApp
#endif
{-# INLINABLE b_conDeclDetails #-}

b_recFieldsD :: [HConDeclField] -> HConDeclDetails
b_recFieldsD = RecCon . mkLocatedList
{-# INLINABLE b_recFieldsD #-}

b_recFieldD :: Maybe LHsDocString -> ([Code], HType) -> Builder HConDeclField
b_recFieldD mb_doc (names, ty) = do
  let f (LForm (L l form)) =
        case form of
          Atom (ASymbol name) ->
            return (L l (mkFieldOcc (L l (mkRdrName name))))
          _ -> builderError
  names' <- mapM f names
  let field = ConDeclField { cd_fld_names = names'
#if MIN_VERSION_ghc(8,6,0)
                           , cd_fld_ext = NOEXT
#endif
                           , cd_fld_type = ty
                           , cd_fld_doc = mb_doc }
      loc = getLoc (mkLocatedForm names)
  return (L loc field)
{-# INLINABLE b_recFieldD #-}

b_derivD :: Maybe HDerivStrategy -> [HType] -> HDeriving
b_derivD mb_strat tys = L l dcs
  where
    l = getLoc (mkLocatedList tys)
    dcs = [L l dc]
    dc = HsDerivingClause NOEXT mb_strat (L l (map mkLHsSigType tys))
{-# INLINABLE b_derivD #-}

b_derivsD :: HDeriving -> HDeriving -> HDeriving
b_derivsD (dL->L _ new) (dL->L _ acc) = mkLocatedList (new ++ acc)
{-# INLINABLE b_derivsD #-}

b_viaD :: HType -> Builder (Maybe HDerivStrategy)
#if MIN_VERSION_ghc(8,6,0)
b_viaD ty@(dL-> L l _) = return (Just (L l (ViaStrategy (mkLHsSigType ty))))
#else
b_viaD _               = builderError
#endif
{-# INLINABLE b_viaD #-}

b_standaloneD :: Maybe HDerivStrategy
              -> Maybe (Located OverlapMode)
              -> HType -> HDecl
b_standaloneD mb_strategy mb_overlap ty0@(dL-> L l _) = L l (DerivD NOEXT dd)
  where
    dd = DerivDecl NOEXT ty1 mb_strategy mb_overlap
#if MIN_VERSION_ghc(8,6,0)
    ty1 = mkLHsSigWcType ty0
#else
    ty1 = mkLHsSigType ty0
#endif
{-# INCLUDE b_standaloneD #-}

b_classD :: ([HType],HType) -> [HDecl] -> Builder HDecl
b_classD (tys,ty) decls = do
    cd <- cvBindsAndSigs (toOL decls)
    let
#if MIN_VERSION_ghc(9,0,0)
        userTV = UserTyVar NOEXT ()
        kindedTV = KindedTyVar NOEXT ()
#else
        userTV = UserTyVar NOEXT
        kindedTV = KindedTyVar NOEXT
#endif
        -- Recursing in `HsAppTy' to support MultiParamTypeClasses.
    let unAppTy t =
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
    atdefs <- cd2atdefs cd
    (l, name, bndrs) <- unAppTy ty
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
                        , tcdDocs = cd_docs cd
#if MIN_VERSION_ghc(9,0,0)
                        , tcdCExt = NoLayoutInfo
#elif MIN_VERSION_ghc(8,6,0)
                        , tcdCExt = NOEXT
#else
                        , tcdFVs = placeHolderNames
#endif
                        }
    return (L l (tyClD cls))
{-# INLINABLE b_classD #-}

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
                         , cid_ext = NOEXT
#endif
                         }
      qty = L l (mkHsQualTy_compat (mkLocatedList ctxts) ty)
      instD = InstD NOEXT
      clsInstD = ClsInstD NOEXT
  return (L l (instD (clsInstD decl)))
{-# INLINABLE b_instD #-}

b_datafamD :: Code -> (FastString, [HTyVarBndr], Maybe HType) -> HDecl
b_datafamD = mkFamilyDecl DataFamily
{-# INLINABLE b_datafamD #-}

b_tyfamD :: [(Located FastString, [HType], HType)]
         -> Code
         -> (FastString, [HTyVarBndr], Maybe HType)
         -> HDecl
b_tyfamD insts =
  if null insts
     then mkFamilyDecl OpenTypeFamily
     else mkFamilyDecl (ClosedTypeFamily (Just tfies))
  where
    tfies = map f insts
    f (L l name, argtys, ty) =
      let rname = L l (mkUnqual tcName name)
      in  L l (mkTyFamInstEqn rname argtys ty)
{-# INLINABLE b_tyfamD #-}

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
        , fdExt = NOEXT
#endif
        }
      lname = L l (mkUnqual tcName name)
      hsqtyvars = mkHsQTvs bndrs
      rsig = case mb_kind of
                -- XXX: Does not ssupport 'TyVarsig'.
                Nothing   -> NoSig NOEXT
                Just lsig -> KindSig NOEXT lsig
  in  L l (TyClD NOEXT (FamDecl NOEXT fam))
{-# INLINABLE mkFamilyDecl #-}

b_dfltSigD :: HDecl -> Builder HDecl
b_dfltSigD (dL->L l decl) =
  case decl of
    SigD _EXT (TypeSig _EXT ids ty) ->
     return (cL l (sigD (ClassOpSig NOEXT True ids (hswc_body ty))))
    _                               -> builderError
{-# INLINABLE b_dfltSigD #-}

-- See: "Convert.cvtDec".
b_datainstD :: Code
            -> (Located FastString, [HType])
            -> (HDeriving, [HConDecl])
            -> HDecl
b_datainstD = mk_data_or_newtype_instD DataType
{-# INLINABLE b_datainstD #-}

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
                             , dfid_ext = NOEXT
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
                       , dd_ext = NOEXT
#endif
                       }
      tycon = L ln (mkUnqual tcName name)
      inst = mkDataFamInstDecl tycon pats rhs
  in  L l (InstD NOEXT faminst)
{-# INLINABLE mk_data_or_newtype_instD #-}

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
{-# INLINABLE b_tyinstD #-}

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
    List [(LForm (L l (Atom (ASymbol mode))))] = lst
    -- XXX: Adding extra pragma comment header to support translation to
    -- Haskell source code.
    stxt = SourceText ("{-# " ++ unpackFS mode)
{-# INLINABLE b_overlapP #-}

b_qtyclC :: [HType] -> Builder ([HType], HType)
b_qtyclC ts =
  case ts of
    []  -> builderError
    _   -> case splitAt (length ts - 1) ts of
            (ctxt, [t]) -> return (ctxt, t)
            _           -> builderError
{-# INLINABLE b_qtyclC #-}

b_defaultD :: [HType] -> HDecl
b_defaultD types = L l (defD (defaultDecl types))
  where
    l = getLoc (mkLocatedList types)
    defD = DefD NOEXT
    defaultDecl = DefaultDecl NOEXT
{-# INLINABLE b_defaultD #-}

b_fixityD :: FixityDirection -> Code -> [Code] -> Builder HDecl
b_fixityD dir (LForm (L l form)) syms =
  case form of
    Atom (AInteger IL {il_value=n}) -> do
      let lname (LForm (L l0 x)) =
            case x of
              Atom (ASymbol name) -> return (L l0 (mkRdrName name))
              _                   -> builderError
          fixity = Fixity dir' (fromIntegral n) dir
          dir' = case dir of
                   InfixL -> SourceText "infixl"
                   InfixR -> SourceText "infixr"
                   InfixN -> SourceText "infix"
          fixSig = FixSig NOEXT
          fixitySig = FixitySig NOEXT
      names <- mapM lname syms
      return (L l (sigD (fixSig (fixitySig names fixity))))
    _ -> builderError
{-# INLINABLE b_fixityD #-}

b_ffiD :: Code -> Code -> HCCallConv -> Maybe (Located Safety)
       -> Code -> (Code, HType) -> Builder HDecl
b_ffiD (LForm (L l _)) imp_or_exp ccnv mb_safety ename (nm, ty) =
  case unCode imp_or_exp of
    Atom (ASymbol ie)
      | ie == "import"
      , Just ispec <- parseCImport ccnv safety name ename' source -> do
        let fi = ForeignImport { fd_name = lname
                               , fd_sig_ty = tsig
#if MIN_VERSION_ghc(8,6,0)
                               , fd_i_ext = NOEXT
#else
                               , fd_co = noForeignImportCoercionYet
#endif
                               , fd_fi = ispec}
        return (L l (forD fi))
      | ie == "export" -> do
        let fe = ForeignExport { fd_name = lname
                               , fd_sig_ty = tsig
#if MIN_VERSION_ghc(8,6,0)
                               , fd_e_ext = NOEXT
#else
                               , fd_co = noForeignExportCoercionYet
#endif
                               , fd_fe = e }
            e = CExport (L l (CExportStatic (SourceText ename')
                                            ename'_fs
                                            (unLoc ccnv)))
                        (L l (SourceText ename'))
        return (L l (forD fe))
    _ -> builderError
    where
      lname = L ln (mkRdrName name)
      LForm (L ln (Atom (ASymbol name))) = nm
      tsig = mkLHsSigType ty
      LForm (L _ls (Atom (AString _ ename'_fs))) = ename
      ename' = unpackFS ename'_fs
      source =
         case ename' of
            "" -> L l NoSourceText
            _  -> L l (quotedSourceText ename')
      safety = fromMaybe (noLoc PlaySafe) mb_safety
      forD = ForD NOEXT
{-# INLINABLE b_ffiD #-}

b_callConv :: Code -> Builder (Located CCallConv)
b_callConv (LForm (L l form)) =
  case form of
    Atom (ASymbol sym)
      | sym == "capi" -> r CApiConv
      | sym == "ccall" -> r CCallConv
      | sym == "prim" -> r PrimCallConv
      | sym == "javascript" -> r JavaScriptCallConv
      | sym == "stdcall" -> r StdCallConv
    _ -> builderError
  where
    r = return . L l
{-# INLINABLE b_callConv #-}

b_safety :: Code -> Builder (Located Safety)
b_safety (LForm (L l form)) =
  case form of
    Atom (ASymbol sym) ->
      case sym of
        "interruptible" -> return (L l PlayInterruptible)
        "safe"          -> return (L l PlaySafe)
        "unsafe"        -> return (L l PlayRisky)
        _               -> builderError
    _ -> builderError
{-# INLINABLE b_safety #-}

b_funOrPatD :: Code -> [HPat] -> ([HGRHS], [HDecl]) -> Builder HDecl
b_funOrPatD eq_form pats gxd@(grhss,decls) =
  case pats of
    [] -> setLastToken eq_form >> failB "empty binding"
    lpat@(dL->L l (BangPat _EXT pat)):pats' ->
      case pats' of
        [] -> return (b_patBindD gxd lpat)
        _  -> let name = L l (mkRdrName "!")
              in  b_funBindD name (pat:pats') grhss decls
    lpat@(dL->L _ pat):pats'
      | isVarPat pat -> do
        name <- varToName pat
        b_funBindD name pats' grhss decls
      | null pats'   -> return (b_patBindD gxd lpat)
      | otherwise    -> setLastToken eq_form >> failB "malformed binding"
  where
    isVarPat VarPat {} = True
    isVarPat _         = False
    varToName (VarPat _EXT lname) = return lname
    varToName _                   = failB "invalid name"
{-# INLINABLE b_funOrPatD #-}

b_funBindD :: Located RdrName -> [HPat] -> [HGRHS] -> [HDecl]
            -> Builder HDecl
b_funBindD lname@(L l _) args grhss decls = do
  let body = mkGRHSs grhss decls l
#if MIN_VERSION_ghc(8,6,0)
      match = L l (Match NOEXT ctxt args body)
#elif MIN_VERSION_ghc(8,4,0)
      match = L l (Match ctxt args' body)
      args' = map (parenthesizePat' appPrec) args
#else
      match = L l (Match ctxt args' Nothing body)
      args' = map (parenthesizePat' appPrec) args
#endif
      ctxt = FunRhs { mc_fun = lname
                    , mc_fixity = Prefix
                      -- XXX: Get strictness from ... where?
                    , mc_strictness = NoSrcStrict }
#if MIN_VERSION_ghc(8,10,0)
      mkFunBind' = mkFunBind FromSource
#else
      mkFunBind' = mkFunBind
#endif
      bind = mkFunBind' lname [match]
  return (L l (valD bind))
{-# INLINABLE b_funBindD #-}

b_patBindD :: ([HGRHS],[HDecl]) -> HPat -> HDecl
b_patBindD (grhss,decls) (dL->L l pat) =
  let bind = PatBind { pat_lhs = cL l pat
                     , pat_rhs = mkGRHSs grhss decls l
#if MIN_VERSION_ghc(8,6,0)
                     , pat_ext = NOEXT
#else
                     , pat_rhs_ty = placeHolderType
                     , bind_fvs = placeHolderNames
#endif
                     , pat_ticks = ([],[]) }
  in  L l (valD bind)
{-# INLINABLE b_patBindD #-}

b_tsigD :: [Code] -> ([HType], HType) -> Builder HDecl
b_tsigD names (ctxts,typ0) = do
  let typ' = mkLHsSigWcType qtyp
      qtyp = if null ctxts
                then typ1
                else L l (mkHsQualTy_compat (mkLocatedList ctxts) typ1)
      typ1 = unParTy typ0
      mkName form =
        case form of
          LForm (L l1 (Atom (ASymbol name))) -> return (L l1 (mkRdrName name))
          _                                  -> builderError
      l = getLoc (mkLocatedForm names)
      typeSig = TypeSig NOEXT
  names' <- mapM mkName names
  return (L l (sigD (typeSig names' typ')))
{-# INLINABLE b_tsigD #-}

b_inlineD :: InlineSpec -> Maybe Activation -> Code -> Builder HDecl
b_inlineD ispec mb_act (LForm (L l form)) =
  case form of
    Atom (ASymbol name) ->
      let inlineSig = InlineSig NOEXT
      in  return (L l (sigD (inlineSig (L l (mkRdrName name)) ipragma)))
    _ -> builderError
  where
    ipragma = mkInlinePragma (SourceText source) (ispec, FunLike) mb_act
    source = case ispec of
               NoInline  -> "{-# NOINLINE"
               Inlinable -> "{-# INLINABLE"
               _         -> "{-# INLINE"
{-# INLINABLE b_inlineD #-}

b_activation :: (SourceText -> PhaseNum -> Activation)
             -> Code -> Builder Activation
b_activation f code@(LForm (L _l atom))
  | Atom (AInteger il) <- atom = return (f source (fromIntegral (il_value il)))
  -- Supporting symbols in "~N" form, where "N" is an integer.
  | Atom (ASymbol s) <- atom
  , '~':rest <- unpackFS s
  , [(n,"")] <- reads rest = return (f source n)
  | otherwise = builderError
  where
    source = SourceText (show code)
{-# INLINABLE b_activation #-}

b_specializeD :: Code -> Maybe Activation -> (Code, HType) -> Builder HDecl
b_specializeD = specializeBuilder noUserInline "{-# SPECIALISE"
{-# INLINABLE b_specializeD #-}

b_specializeInlineD :: Code -> Maybe Activation -> (Code, HType)
                    -> Builder HDecl
b_specializeInlineD = specializeBuilder Inline "{-# SPECIALISE INLINE"
{-# INLINABLE b_specializeInlineD #-}

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
{-# INLINABLE specializeBuilder #-}

b_docnextD :: Code -> Builder HDecl
b_docnextD (LForm (L l form)) =
  case form of
    Atom (AString _ str) -> return $! L l (DocD NOEXT (docCommentNext str))
    _                    -> builderError
{-# INLINABLE b_docnextD #-}

b_docprevD :: Code -> Builder HDecl
b_docprevD (LForm (L l form)) =
  case form of
    Atom (AString _ str) ->
      return $! L l (DocD NOEXT (DocCommentPrev (hsDocString str)))
    _ -> builderError
{-# INLINABLE b_docprevD #-}

b_docGroupD :: Int -> Code -> Builder HDecl
b_docGroupD n form@(LForm (L l _))
  | List [_,doc_code] <- unCode form
  , Atom (AString _ doc) <- unCode doc_code
  = return $! L l (DocD NOEXT (DocGroup (fromIntegral n)
                                        (hsDocString doc)))
  | otherwise = setLastToken form >> failB "Invalid group doc"
{-# INLINABLE b_docGroupD #-}

b_docNamed :: Code -> Builder HDecl
b_docNamed form@(LForm (L l body))
  | List [_,name_code,doc_code] <- body
  , Atom (ASymbol name) <- unCode name_code
  , Atom (AString _ doc) <- unCode doc_code
  = let name' = unpackFS name
        doc' = hsDocString doc
    in return $! L l (DocD NOEXT (DocCommentNamed name' doc'))
  | otherwise
  = setLastToken form >> failB "Invalid named doc"
{-# INLINABLE b_docNamed #-}

docCommentNext :: FastString -> DocDecl
docCommentNext = DocCommentNext . hsDocString
{-# INLINABLE docCommentNext #-}

tyClD :: TyClDecl PARSED -> HsDecl PARSED
tyClD = TyClD NOEXT
{-# INLINABLE tyClD #-}

valD :: HsBind PARSED -> HsDecl PARSED
valD = ValD NOEXT
{-# INLINABLE valD #-}

sigD :: Sig PARSED -> HsDecl PARSED
sigD = SigD NOEXT
{-# INLINABLE sigD #-}

mkDataFamInstDecl :: Located RdrName
                  -> [HType]
                  -> HsDataDefn PARSED
                  -> DataFamInstDecl PARSED
mkDataFamInstDecl tycon pats rhs = dfid
  where
#if MIN_VERSION_ghc(8,4,0)
    dfid = DataFamInstDecl (mkHsImplicitBndrs (mkFamEqn tycon pats rhs))
#else
    dfid = DataFamInstDecl { dfid_tycon = tycon
                           , dfid_pats = mkHsImplicitBndrs pats
                           , dfid_fixity = Prefix
                           , dfid_defn = rhs
                           , dfid_fvs = placeHolderNames }
#endif
{-# INLINABLE mkDataFamInstDecl #-}

mkTyFamInstEqn :: Located RdrName
               -> [HType]
               -> HType
               -> TyFamInstEqn PARSED
mkTyFamInstEqn tycon pats rhs =
#if MIN_VERSION_ghc(8,4,0)
  mkHsImplicitBndrs (mkFamEqn tycon pats rhs)
#else
  TyFamEqn { tfe_tycon = tycon
           , tfe_pats = mkHsImplicitBndrs pats
           , tfe_fixity = Prefix
           , tfe_rhs = rhs }
#endif
{-# INLINABLE mkTyFamInstEqn #-}

#if MIN_VERSION_ghc(8,10,0)
type FAMEQN p rhs = FamEqn p rhs
#elif MIN_VERSION_ghc (8,4,0)
type FAMEQN p rhs = FamEqn p (HsTyPats p) rhs
#else
type FAMEQN p rhs = TyFamEqn p rhs
#endif

#if MIN_VERSION_ghc(8,4,0)
mkFamEqn :: Located RdrName -> [HType] -> rhs
         -> FAMEQN PARSED rhs
mkFamEqn tycon pats rhs =
  FamEqn { feqn_tycon = tycon
         , feqn_fixity = Prefix
         , feqn_rhs = rhs
         -- Type synonym "HsTyPats" for `feqn_pats' field changed from
         -- `HsTyPats' to `HsTypeArg' in 8.8.0.
#if   MIN_VERSION_ghc(8,8,0)
         , feqn_pats = map HsValArg pats
         , feqn_bndrs = Nothing
#else
         , feqn_pats = pats
#endif
#if   MIN_VERSION_ghc(8,6,0)
         , feqn_ext = NOEXT
#endif
         }
{-# INLINABLE mkFamEqn #-}
#endif

unParTy :: HType -> HType
unParTy t0 =
  case t0 of
    L _ (HsParTy _EXT t1) -> t1
    _                     -> t0
{-# INLINABLE unParTy #-}

noUserInline :: InlineSpec
#if MIN_VERSION_ghc(8,4,0)
noUserInline = NoUserInline
#else
noUserInline = EmptyInlineSpec
#endif

#if MIN_VERSION_ghc(8,10,0)
cd2atdefs :: CategorizedDecls -> Builder [LTyFamDefltDecl PARSED]
cd2atdefs = pure . cd_tfis
#else
cd2atdefs :: CategorizedDecls -> Builder [LTyFamDefltEqn PARSED]
cd2atdefs cd = mapM toATDef (cd_tfis cd)
  where
     toATDef d = case mkATDefault' d of
                   Right lty      -> return lty
                   Left (_, sdoc) -> failB (showSDocUnsafe sdoc)
#if MIN_VERSION_ghc(8,8,0)
     mkATDefault' = fmap fst . mkATDefault
#else
     mkATDefault' = mkATDefault
#endif
#endif
