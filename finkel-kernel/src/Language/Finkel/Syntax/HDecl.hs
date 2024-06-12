{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

-- | Syntax for declaration.
module Language.Finkel.Syntax.HDecl where

#include "Syntax.h"
#include "ghc_modules.h"

-- base
import Data.Maybe                       (fromMaybe)

-- ghc
import GHC_Core_DataCon                 (SrcStrictness (..))
import GHC_Data_FastString              (FastString, unpackFS)
import GHC_Data_OrdList                 (toOL)
import GHC_Hs_Binds                     (HsBind, Sig (..))
import GHC_Hs_Decls                     (ClsInstDecl (..), ConDecl (..),
                                         DataFamInstDecl (..), DefaultDecl (..),
                                         DerivDecl (..), DocDecl (..),
                                         FamilyDecl (..), FamilyInfo (..),
                                         FamilyResultSig (..), ForeignDecl (..),
                                         ForeignExport (..), HsDataDefn (..),
                                         HsDecl (..), HsDerivingClause (..),
                                         InstDecl (..), TyClDecl (..),
                                         TyFamInstDecl (..), TyFamInstEqn)
import GHC_Hs_Doc                       (LHsDocString)
import GHC_Hs_Expr                      (HsMatchContext (..), Match (..))
import GHC_Hs_Pat                       (Pat (..))
import GHC_Hs_Type                      (ConDeclField (..), HsConDetails (..),
                                         HsTyVarBndr (..), HsType (..),
                                         HsWildCardBndrs (..), mkFieldOcc,
                                         mkHsQTvs)
import GHC_Hs_Utils                     (mkClassOpSigs)
import GHC_Parser_PostProcess           (mkConDeclH98, mkGadtDecl,
                                         mkInlinePragma, parseCImport)
import GHC_Types_Basic                  (Activation (..), InlineSpec (..),
                                         OverlapMode (..), PhaseNum,
                                         RuleMatchInfo (..))
import GHC_Types_Fixity                 (Fixity (..), FixityDirection (..),
                                         LexicalFixity (..))
import GHC_Types_ForeignCall            (CCallConv (..), CExportSpec (..),
                                         Safety (..))
import GHC_Types_Name_Occurrence        (dataName, tcName)
import GHC_Types_Name_Reader            (RdrName, mkUnqual)
import GHC_Types_SrcLoc                 (GenLocated (..), Located, getLoc,
                                         noLoc, unLoc)

#if !MIN_VERSION_ghc(9,10,0) && MIN_VERSION_ghc(9,6,0)
import Language.Haskell.Syntax.Concrete (HsUniToken (..))
import GHC.Parser.Annotation            (noAnn)
import GHC.Parser.PostProcess           (mkTokenLocation)
#endif

#if !MIN_VERSION_ghc(9,10,0) && MIN_VERSION_ghc(9,6,0)
import Language.Haskell.Syntax.Concrete (LayoutInfo (..))
#elif !MIN_VERSION_ghc(9,10,0) && MIN_VERSION_ghc(9,0,0)
import GHC_Types_SrcLoc                 (LayoutInfo (..))
#endif

#if MIN_VERSION_ghc(9,8,0)
import GHC.Data.FastString              (fsLit)
import Language.Haskell.Syntax.Type     (HsBndrVis (..))
#endif

#if MIN_VERSION_ghc(9,6,0)
import Language.Haskell.Syntax.Decls    (DataDefnCons (..))
#endif

#if MIN_VERSION_ghc(9,6,0)
import Language.Haskell.Syntax.Decls    (NewOrData (..))
#else
import GHC_Hs_Decls                     (NewOrData (..))
#endif

#if MIN_VERSION_ghc(9,4,0)
import GHC.Parser.Annotation            (l2l)
#endif

#if MIN_VERSION_ghc(9,2,0)
import GHC_Hs_Type                      (mkHsOuterImplicit)
import GHC_Hs_Decls                     (DerivClauseTys (..),
                                         XViaStrategyPs (..))
import GHC_Hs_Utils                     (hsTypeToHsSigType, hsTypeToHsSigWcType)
import GHC_Parser_Annotation            (AnnSortKey (..))
import GHC_Types_Basic                  (TopLevelFlag (..))
#else
import GHC_Hs_Type                      (mkHsImplicitBndrs)
import GHC_Hs_Utils                     (mkLHsSigType, mkLHsSigWcType)
#endif


#if MIN_VERSION_ghc(9,0,0)
import GHC_Hs_Type                      (LHsTyVarBndr, hsLinear)
import GHC_Types_Var                    (Specificity (..))
#endif

#if MIN_VERSION_ghc(8,10,0)
import GHC_Parser_Lexer                 (P (..), ParseResult (..))
import GHC_Parser_PostProcess           (mkStandaloneKindSig)
#endif

#if MIN_VERSION_ghc(8,10,0)
import GHC_Hs_Decls                     (LTyFamDefltDecl)
#else
import GHC_Hs_Decls                     (LTyFamDefltEqn)
import Outputable                       (showSDocUnsafe)
import RdrHsSyn                         (mkATDefault)
#endif

#if MIN_VERSION_ghc(8,10,0)
import GHC_Hs_Decls                     (FamEqn (..))
#elif MIN_VERSION_ghc(8,4,0)
import GHC_Hs_Decls                     (FamEqn (..), HsTyPats)
#else
import GHC_Hs_Decls                     (TyFamEqn (..))
#endif

#if MIN_VERSION_ghc(8,8,0)
import GHC_Hs_Type                      (HsArg (..))
#endif

#if MIN_VERSION_ghc(8,6,0)
import GHC_Hs_Decls                     (DerivStrategy (..))
#else
import GHC_Hs_Decls                     (noForeignExportCoercionYet,
                                         noForeignImportCoercionYet)
import PlaceHolder                      (PlaceHolder (..), placeHolderNames)
#endif

-- Internal
import Language.Finkel.Builder
import Language.Finkel.Data.SourceText
import Language.Finkel.Form
import Language.Finkel.Syntax.HBind
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
        -> (FastString, [HTyVarBndrVis], Maybe HKind)
        -> (HDeriving, [HConDecl])
        -> HDecl
b_dataD = mkNewtypeOrDataD DataType
{-# INLINABLE b_dataD #-}

b_newtypeD :: Code
           -> (FastString, [HTyVarBndrVis], Maybe HKind)
           -> (HDeriving, [HConDecl])
           -> HDecl
b_newtypeD = mkNewtypeOrDataD NewType
{-# INLINABLE b_newtypeD #-}

mkNewtypeOrDataD :: NewOrData
                 -> Code
                 -> (FastString, [HTyVarBndrVis], Maybe HKind)
                 -> (HDeriving, [HConDecl])
                 -> HDecl
mkNewtypeOrDataD newOrData (LForm (L l _)) (name, tvs, ksig) (derivs, cs) =
  lA l (tyClD decl)
  where
    decl = DataDecl { tcdLName = lN l (mkUnqual tcName name)
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
    defn = HsDataDefn { dd_cType = Nothing
#if !MIN_VERSION_ghc(9,6,0)
                      , dd_ND = newOrData
#endif
#if MIN_VERSION_ghc(9,2,0)
                      , dd_ctxt = Nothing
#else
                      , dd_ctxt = noLoc []
#endif
                      , dd_kindSig = ksig
                      , dd_cons = condecls
                      , dd_derivs = derivs
#if MIN_VERSION_ghc(8,6,0)
                      , dd_ext = NOEXT
#endif
                      }
#if MIN_VERSION_ghc(9,6,0)
    condecls = case newOrData of
                 NewType | c:_ <- cs -> NewTypeCon c
                 DataType            -> DataTypeCons False cs
                 -- XXX: Not sure reaching below is possible.
                 _                   -> error "mkNewTypeOrDataD:condecls"
#else
    condecls = cs
#endif
{-# INLINABLE mkNewtypeOrDataD #-}

b_typeD :: Code
        -> (FastString, [HTyVarBndrVis], Maybe HKind)
        -> HType
        -> HDecl
b_typeD (LForm (L l _)) (name, tvs, _) ty = lA l (tyClD synonym)
  where
    synonym = SynDecl { tcdLName = lN l (mkUnqual tcName name)
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

b_standaloneKindSigD
  :: Code -> (FastString, [a], Maybe HKind) -> Builder HDecl
#if MIN_VERSION_ghc(8,10,0)
b_standaloneKindSigD (LForm (L l _)) (name, _tvs, mb_knd) =
  -- StandaloneKindSignature is not supported in ghc < 8.10.  Also the arguments
  -- of "mkStandaloneKindSig" differ from ghc 9.0.x to ghc 9.2.x.
#if   MIN_VERSION_ghc(9,2,0)
  do knd <- maybe builderError (pure . hsTypeToHsSigType) mb_knd
     let sigP = mkStandaloneKindSig l (L l [lN l (mkRdrName name)]) knd []
#else
  do knd <- maybe builderError pure mb_knd
     let sigP = mkStandaloneKindSig l (L l [lN l (mkRdrName name)]) knd
#endif
     ps <- fmap ghcPState getBState
     case unP sigP ps of
       POk _ sig -> pure (lA l (KindSigD NOEXT (unLoc sig)))
       PFailed _ -> builderError
#else
b_standaloneKindSigD _ _ = builderError
#endif

b_conD :: Code -> HConDeclH98Details -> Builder HConDecl
b_conD form@(LForm (L l _)) details = do
  name <- getConId form
  let name' = lN l (mkUnqual dataName name)
#if MIN_VERSION_ghc(8,6,0)
      cxt = Nothing
#else
      cxt = L l []
#endif
#if MIN_VERSION_ghc(9,2,0)
  pure (lA l (mkConDeclH98 NOEXT name' Nothing cxt details))
#else
  pure (L l (mkConDeclH98 name' Nothing cxt details))
#endif
{-# INLINABLE b_conD #-}

b_qtyconD :: (HConDecl, [HType]) -> HConDecl
b_qtyconD (whole@(L l decl), tys) =
  case tys of
    [] -> whole
#if MIN_VERSION_ghc(9,10,0)
    _  -> L l (decl { con_mb_cxt = Just (mkLocatedListA tys) })
#elif MIN_VERSION_ghc(8,6,0)
    _  -> L l (decl { con_mb_cxt = Just (la2la (mkLocatedListA tys)) })
#else
    _  -> L l (decl { con_cxt = Just (mkLocatedList tys) })
#endif
{-# INLINABLE b_qtyconD #-}

#if MIN_VERSION_ghc(9,0,0)
b_forallD
  :: [LHsTyVarBndr Specificity PARSED]
  -> (HConDecl, [HType])
  -> Builder HConDecl
#else
b_forallD :: [HTyVarBndr] -> (HConDecl, [HType]) -> Builder HConDecl
#endif
b_forallD vars (L l cdecl@ConDeclH98{}, cxts) = pure d
  where
#if MIN_VERSION_ghc(8,6,0)
    d = L l cdecl { con_ex_tvs = vars
#  if MIN_VERSION_ghc(9,2,0)
                  , con_forall = True
#  else
                  , con_forall = noLoc True
#  endif
#  if MIN_VERSION_ghc(9,10,0)
                  , con_mb_cxt = Just (mkLocatedListA cxts) }
#  else
                  , con_mb_cxt = Just (la2la (mkLocatedListA cxts)) }
#  endif
#else
    d = L l cdecl { con_qvars = Just (mkHsQTvs vars)
                  , con_cxt = Just (mkLocatedList cxts) }
#endif
b_forallD _ _ = builderError
{-# INLINABLE b_forallD #-}

b_gadtD :: Code -> ([HType], HType) -> Builder HConDecl
b_gadtD form@(LForm (L l1 _)) (ctxt, bodyty) = do
  name <- getConId form
  let name' = pure $ lN l1 (mkUnqual dataName name)
#if MIN_VERSION_ghc(9,0,0)
      -- Removing parentheses of the body type, so that the 'mkGadtDecl' can
      -- split the internal elements. Parentheses are added to the body of GADT
      -- when it is HsForAllTy, to support documentation string.
      ty = case qty of
             HsParTy _ unpar_ty -> unpar_ty
             _                  -> lA l1 qty

#else
      ty = lA l1 qty
#endif
#if MIN_VERSION_ghc(9,10,0)
      qty = mkHsQualTy' (mkLocatedListA ctxt) bodyty
#else
      qty = mkHsQualTy' (la2la (mkLocatedListA ctxt)) bodyty
#endif
#if MIN_VERSION_ghc(9,10,0)
  ldecl <- do
    ps <- fmap ghcPState getBState
    case unP (mkGadtDecl l1 name' unused (hsTypeToHsSigType ty)) ps of
      POk _ d -> pure d
      _       -> builderError
#elif MIN_VERSION_ghc(9,6,0)
  ldecl <- do
    ps <- fmap ghcPState getBState
    let dcolon = L (mkTokenLocation l1) HsNormalTok
    case unP (mkGadtDecl l1 name' dcolon (hsTypeToHsSigType ty)) ps of
      POk _ d -> pure d
      _       -> builderError
#elif MIN_VERSION_ghc(9,2,0)
  ldecl <- do
    ps <- fmap ghcPState getBState
    case unP (mkGadtDecl l1 name' (hsTypeToHsSigType ty) []) ps of
      POk _ d -> pure d
      _       -> builderError
#elif MIN_VERSION_ghc(9,0,0)
  ldecl <- do
    ps <- fmap ghcPState getBState
    case unP (mkGadtDecl name' ty) ps of
      POk _ d -> pure (L l1 (fst d))
      _       -> builderError
#elif MIN_VERSION_ghc(8,6,0)
  let ldecl = L l1 (fst (mkGadtDecl name' ty))
#else
  let ldecl = L l1 (mkGadtDecl name' (mkLHsSigType ty))
#endif
  return ldecl
{-# INLINABLE b_gadtD #-}

b_conOnlyD :: Code -> Builder HConDecl
b_conOnlyD name = b_conD name pcon
  where
#if MIN_VERSION_ghc(9,2,0)
    pcon = PrefixCon [] []
#else
    pcon = PrefixCon []
#endif
{-# INLINABLE b_conOnlyD #-}

-- XXX: Infix data constructor not supported.
-- XXX: Does not support liner types and unicode syntax (ghc >= 9.0)
b_conDeclDetails :: [HType] -> HConDeclH98Details
#if MIN_VERSION_ghc(9,2,0)
b_conDeclDetails = PrefixCon [] . map (hsLinear . parTyApp)
#elif MIN_VERSION_ghc(9,0,0)
b_conDeclDetails = PrefixCon . map (hsLinear . parTyApp)
#else
b_conDeclDetails = PrefixCon . map parTyApp
#endif
{-# INLINABLE b_conDeclDetails #-}

b_recFieldsD :: [HConDeclField] -> HConDeclH98Details
#if MIN_VERSION_ghc(9,10,0)
b_recFieldsD = RecCon . mkLocatedListA
#else
b_recFieldsD = RecCon . la2la . mkLocatedListA
#endif
{-# INLINABLE b_recFieldsD #-}

b_recFieldD :: Maybe LHsDocString -> ([Code], HType) -> Builder HConDeclField
b_recFieldD mb_doc (names, ty) = do
  let f (LForm (L l form)) =
        case form of
          Atom (ASymbol name) ->
#if MIN_VERSION_ghc(9,4,0)
            return (reLocA (L l (mkFieldOcc (lN l (mkRdrName name)))))
#else
            return (L l (mkFieldOcc (lN l (mkRdrName name))))
#endif
          _ -> builderError
  let mb_doc' = fmap lHsDocString2LHsDoc mb_doc
  names' <- mapM f names
  let field = ConDeclField { cd_fld_names = names'
#if MIN_VERSION_ghc(8,6,0)
                           , cd_fld_ext = NOEXT
#endif
                           , cd_fld_type = ty
                           , cd_fld_doc = mb_doc' }
      loc = getLoc (mkLocatedForm names)
  return (lA loc field)
{-# INLINABLE b_recFieldD #-}

b_derivD :: Maybe HDerivStrategy -> [HType] -> HDeriving
b_derivD mb_strat tys = hds
  where
#if MIN_VERSION_ghc(9,4,0)
    hds = [la2la (L l dc)]
    clauses = la2la (L l (DctMulti NOEXT (map hsTypeToHsSigType tys)))
#elif MIN_VERSION_ghc(9,2,0)
    hds = [reLoc (L l dc)]
    clauses = la2la (L l (DctMulti NOEXT (map hsTypeToHsSigType tys)))
#else
    hds = L l [L l dc]
    clauses = L l (map hsTypeToHsSigType tys)
#endif
    dc = HsDerivingClause NOEXT mb_strat clauses
    l = getLoc (mkLocatedListA' tys)
{-# INLINABLE b_derivD #-}

b_derivsD :: HDeriving -> HDeriving -> HDeriving
#if MIN_VERSION_ghc(9,2,0)
b_derivsD new acc                     = new ++ acc
#else
b_derivsD (dL->L _ new) (dL->L _ acc) = mkLocatedList (new ++ acc)
#endif
{-# INLINABLE b_derivsD #-}

b_emptyDeriving :: HDeriving
#if MIN_VERSION_ghc(9,2,0)
b_emptyDeriving = []
#else
b_emptyDeriving = noLoc []
#endif
{-# INLINABLE b_emptyDeriving #-}

b_viaD :: HType -> Builder (Maybe HDerivStrategy)
#if MIN_VERSION_ghc(9,4,0)
b_viaD ty@(L l _) =
  pure (Just (L (l2l l) (ViaStrategy (XViaStrategyPs NOEXT sig))))
  where
    sig = hsTypeToHsSigType ty
#elif MIN_VERSION_ghc(9,2,0)
b_viaD ty@(L l _) =
  pure (Just (reLoc (L l (ViaStrategy (XViaStrategyPs NOEXT sig)))))
  where
    sig = hsTypeToHsSigType ty
#elif MIN_VERSION_ghc(8,6,0)
b_viaD ty@(dL->L l _) =
  pure (Just (lA l (ViaStrategy (hsTypeToHsSigType ty))))
#else
b_viaD _              = builderError
#endif
{-# INLINABLE b_viaD #-}

b_standaloneD :: Maybe HDerivStrategy
              -> Maybe (Located OverlapMode)
              -> HType -> HDecl
b_standaloneD mb_strategy mb_overlap ty0@(dL-> L l _) = L l (DerivD NOEXT dd)
  where
#if MIN_VERSION_ghc(9,10,0)
    -- XXX: Does not support WarningTxt.
    dd = DerivDecl (Nothing, NOEXT) ty1 mb_strategy (fmap reLocA mb_overlap)
#else
    dd = DerivDecl NOEXT ty1 mb_strategy (fmap reLocA mb_overlap)
#endif
#if MIN_VERSION_ghc(8,6,0)
    ty1 = hsTypeToHsSigWcType ty0
#else
    ty1 = hsTypeToHsSigType ty0
#endif
{-# INCLUDE b_standaloneD #-}

b_classD :: ([HType],HType) -> [HDecl] -> Builder HDecl
b_classD (tys,ty) decls = do
    cd <- cvBindsAndSigs (toOL decls)
    let
#if MIN_VERSION_ghc(9,10,0)
        userTV = UserTyVar NOEXT (HsBndrRequired NOEXT)
        kindedTV = KindedTyVar NOEXT (HsBndrRequired NOEXT)
#elif MIN_VERSION_ghc(9,8,0)
        -- XXX: Does not support HsBndrInvisible
        userTV = UserTyVar NOEXT HsBndrRequired
        kindedTV = KindedTyVar NOEXT HsBndrRequired
#elif MIN_VERSION_ghc(9,0,0)
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
    bndrs' <- case reverse bndrs of
                []   -> builderError
                _:tl -> pure tl
    let cls = ClassDecl { tcdLName = name
#if !MIN_VERSION_ghc(9,10,0) && MIN_VERSION_ghc(9,6,0)
                        , tcdLayout = NoLayoutInfo
#endif
#if MIN_VERSION_ghc(9,2,0)
                        , tcdCtxt =
                          if null tys
                            then Nothing
#  if MIN_VERSION_ghc(9,10,0)
                            else Just (mkLocatedListA tys)
#  else
                            else Just (la2la (mkLocatedListA tys))
#  endif
#else
                        , tcdCtxt = mkLocatedList tys
#endif
                        , tcdFixity = Prefix
                        , tcdTyVars = mkHsQTvs bndrs'
                        , tcdFDs = []
                        , tcdSigs = mkClassOpSigs (cd_sigs cd)
                        , tcdMeths = cd_binds cd
                        , tcdATs = cd_fds cd
                        , tcdATDefs = atdefs
                        , tcdDocs = cd_docs cd
#if MIN_VERSION_ghc(9,10,0)
                        , tcdCExt = (unused, unused, NoAnnSortKey)
#elif MIN_VERSION_ghc(9,6,0)
                        , tcdCExt = (noAnn, NoAnnSortKey)
#elif MIN_VERSION_ghc(9,2,0)
                        , tcdCExt = (unused, NoAnnSortKey, NoLayoutInfo)
#elif MIN_VERSION_ghc(9,0,0)
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
b_instD mb_overlap (ctxts,ty@(L l _)) decls = do
  cd <- cvBindsAndSigs (toOL decls)
  let decl = ClsInstDecl { cid_poly_ty = hsTypeToHsSigType qty
                         , cid_binds = cd_binds cd
                         , cid_sigs = mkClassOpSigs (cd_sigs cd)
                         , cid_tyfam_insts = cd_tfis cd
                         , cid_datafam_insts = cd_dfis cd
                         , cid_overlap_mode = fmap reLocA mb_overlap
#if MIN_VERSION_ghc(9,10,0)
                           -- XXX: Does not support WarningTxt
                         , cid_ext = (Nothing, unused, NoAnnSortKey)
#elif MIN_VERSION_ghc(9,2,0)
                         , cid_ext = (unused, NoAnnSortKey)
#elif MIN_VERSION_ghc(8,6,0)
                         , cid_ext = NOEXT
#endif
                         }
#if MIN_VERSION_ghc(9,10,0)
      qty = L l (mkHsQualTy' (mkLocatedListA ctxts) ty)
#else
      qty = L l (mkHsQualTy' (la2la (mkLocatedListA ctxts)) ty)
#endif
      instD = InstD NOEXT
      clsInstD = ClsInstD NOEXT
  return (L l (instD (clsInstD decl)))
{-# INLINABLE b_instD #-}

b_datafamD :: Code -> (FastString, [HTyVarBndrVis], Maybe HType) -> HDecl
b_datafamD = mkFamilyDecl DataFamily
{-# INLINABLE b_datafamD #-}

b_tyfamD :: [(Located FastString, [HType], HType)]
         -> Code
         -> (FastString, [HTyVarBndrVis], Maybe HType)
         -> HDecl
b_tyfamD insts =
  if null insts
     then mkFamilyDecl OpenTypeFamily
     else mkFamilyDecl (ClosedTypeFamily (Just tfies))
  where
    tfies = map f insts
    f (L l name, argtys, ty) =
      let rname = L l (mkUnqual tcName name)
      in  lA l (mkTyFamInstEqn rname argtys ty)
{-# INLINABLE b_tyfamD #-}

-- See: "RdrHsSyn.mkFamDecl" and 'Convert.cvtDec'.
mkFamilyDecl :: FamilyInfo PARSED
             -> Code
             -> (FastString, [HTyVarBndrVis], Maybe HType)
             -> HDecl
mkFamilyDecl finfo (LForm (L l _)) (name, bndrs, mb_kind) =
  let fam = FamilyDecl
        { fdInfo = finfo
        , fdLName = lname
        , fdTyVars = hsqtyvars
        , fdFixity = Prefix
#if MIN_VERSION_ghc(9,4,0)
        , fdResultSig = reLocA (L l rsig)
#else
        , fdResultSig = L l rsig
#endif
        , fdInjectivityAnn = Nothing
#if MIN_VERSION_ghc(8,6,0)
        , fdExt = NOEXT
#endif
#if MIN_VERSION_ghc(9,2,0)
          -- XXX: When to use 'NotTopLevel'?
        , fdTopLevel = TopLevel
#endif
        }
      lname = lN l (mkUnqual tcName name)
      hsqtyvars = mkHsQTvs bndrs
      -- XXX: Does not support 'TyVarsig'.
      rsig = maybe (NoSig NOEXT) (KindSig NOEXT) mb_kind
  in  lA l (TyClD NOEXT (FamDecl NOEXT fam))
{-# INLINABLE mkFamilyDecl #-}

b_dfltSigD :: HDecl -> Builder HDecl
b_dfltSigD (dL->L l decl) =
  case decl of
    SigD _EXT (TypeSig _EXT ids ty) ->
     return (cL l (sigD (ClassOpSig NOEXT True ids (hswc_body ty))))
    _                               -> builderError
{-# INLINABLE b_dfltSigD #-}

-- See: "Convert.cvtDec".
b_datainstD
  :: Code
  -> (Located FastString, [HType], Maybe HType)
  -> (HDeriving, [HConDecl])
  -> HDecl
b_datainstD = mk_data_or_newtype_instD DataType
{-# INLINABLE b_datainstD #-}

b_newtypeinstD
  :: Code
  -> (Located FastString, [HType], Maybe HType)
  -> (HDeriving, [HConDecl])
  -> HDecl
b_newtypeinstD = mk_data_or_newtype_instD NewType
{-# INCLUDE b_newtypeinsD #-}

mk_data_or_newtype_instD
  :: NewOrData -> Code
  -> (Located FastString, [HType], Maybe HType)
  -> (HDeriving, [HConDecl]) -> HDecl
mk_data_or_newtype_instD new_or_data (LForm (L l _)) (L ln name, pats, mb_kind)
                         (deriv, condecls) =
  let faminst = DataFamInstD { dfid_inst = inst
#if MIN_VERSION_ghc(8,6,0)
                             , dfid_ext = NOEXT
#endif
                             }
      -- XXX: Contexts and kind signatures not supported.
      rhs = HsDataDefn { dd_cType = Nothing
#if !MIN_VERSION_ghc(9,6,0)
                       , dd_ND = new_or_data
#endif
#if MIN_VERSION_ghc(9,2,0)
                       , dd_ctxt = Nothing
#else
                       , dd_ctxt = L l []
#endif
                       , dd_kindSig = mb_kind
                       , dd_cons = condecls'
                       , dd_derivs = deriv
#if MIN_VERSION_ghc(8,6,0)
                       , dd_ext = NOEXT
#endif
                       }
      tycon = L ln (mkUnqual tcName name)
      inst = mkDataFamInstDecl tycon pats rhs
#if MIN_VERSION_ghc(9,6,0)
      condecls' = case new_or_data of
                    NewType | c:_ <- condecls -> NewTypeCon c
                    DataType                  -> DataTypeCons False condecls
                    -- XXX: Again, not sure reaching below is possible.
                    _ -> error "mk_data_or_newtype_instD:condecls'"
#else
      condecls' = condecls
#endif
  in  lA l (InstD NOEXT faminst)
{-# INLINABLE mk_data_or_newtype_instD #-}

b_tyinstD :: Code -> (Located FastString, [HType]) -> HType -> HDecl
b_tyinstD (LForm (L l _)) (L ln name, pats) rhs =
  let rname = L ln (mkUnqual tcName name)
      inst = mkTyFamInstEqn rname pats rhs
      tyfaminstD = TyFamInstD NOEXT tfid
#if MIN_VERSION_ghc(9,2,0)
      tfid = TyFamInstDecl NOEXT inst
#elif MIN_VERSION_ghc(8,4,0)
      tfid = TyFamInstDecl inst
#else
      tfid = TyFamInstDecl { tfid_eqn = L l inst
                           , tfid_fvs = placeHolderNames }
#endif
  in  lA l (InstD NOEXT tyfaminstD)
{-# INLINABLE b_tyinstD #-}

b_overlapP :: Code -> Builder (Maybe (Located OverlapMode))
b_overlapP (LForm (L _ (List [LForm (L l (Atom (ASymbol mode)))]))) =
  pure $ case mode of
    "OVERLAPPABLE" -> pragma Overlappable
    "OVERLAPPING"  -> pragma Overlapping
    "OVERLAPS"     -> pragma Overlaps
    "INCOHERENT"   -> pragma Incoherent
    _              -> Nothing
  where
    pragma con = Just (L l (con stxt))
    -- XXX: Adding extra pragma comment header to support translation to
    -- Haskell source code.
#if MIN_VERSION_ghc(9,8,0)
    stxt = SourceText (fsLit "{-# " <> mode)
#else
    stxt = SourceText ("{-# " ++ unpackFS mode)
#endif
b_overlapP _ = builderError
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
    l = getLoc (mkLocatedListA types)
    defD = DefD NOEXT
    defaultDecl = DefaultDecl NOEXT
{-# INLINABLE b_defaultD #-}

b_fixityD :: FixityDirection -> Code -> [Code] -> Builder HDecl
b_fixityD dir (LForm (L l form)) syms =
  case form of
    Atom (AInteger IL {il_value=n}) -> do
      let lname (LForm (L l0 x)) =
            case x of
              Atom (ASymbol name) -> return (lN l0 (mkRdrName name))
              _                   -> builderError
          fixity = Fixity dir' (fromIntegral n) dir
          dir' = case dir of
                   InfixL -> strToSourceText "infixl"
                   InfixR -> strToSourceText "infixr"
                   InfixN -> strToSourceText "infix"
      names <- mapM lname syms
      return (lA l (sigD (mkFixSig names fixity)))
    _ -> builderError
{-# INLINABLE b_fixityD #-}

b_ffiD :: Code -> Code -> HCCallConv -> Maybe (Located Safety)
       -> Code -> (Code, HType) -> Builder HDecl
b_ffiD (LForm (L l _)) imp_or_exp ccnv mb_safety ename (nm, ty)
  | LForm (L ln (Atom (ASymbol name))) <- nm
  , LForm (L _ls (Atom (AString _ ename'_fs))) <- ename =
  let lname = reLocA (L ln (mkRdrName name))
      tsig = hsTypeToHsSigType ty
      ename' = unpackFS ename'_fs
      source =
         case ename' of
            "" -> L l NoSourceText
            _  -> L l (toQuotedSourceText ename'_fs)
#if MIN_VERSION_ghc(9,10,0)
      safety = reLoc $ fromMaybe (noLoc PlaySafe) mb_safety
      ccnv' = reLoc ccnv
#else
      safety = fromMaybe (noLoc PlaySafe) mb_safety
      ccnv' = ccnv
#endif
      forD = ForD NOEXT
  in case unCode imp_or_exp of
    Atom (ASymbol ie)
      | ie == "import"
      , Just ispec <- parseCImport ccnv' safety name ename' source -> do
        let fi = ForeignImport { fd_name = lname
                               , fd_sig_ty = tsig
#if MIN_VERSION_ghc(8,6,0)
                               , fd_i_ext = NOEXT
#else
                               , fd_co = noForeignImportCoercionYet
#endif
                               , fd_fi = ispec}
        return (lA l (forD fi))
      | ie == "export" -> do
        let fe = ForeignExport { fd_name = lname
                               , fd_sig_ty = tsig
#if MIN_VERSION_ghc(8,6,0)
                               , fd_e_ext = NOEXT
#else
                               , fd_co = noForeignExportCoercionYet
#endif
                               , fd_fe = e }
            ces = CExportStatic stxt ename'_fs (unLoc ccnv)
#if MIN_VERSION_ghc(9,8,0)
            stxt = SourceText ename'_fs
#else
            stxt = SourceText ename'
#endif
#if MIN_VERSION_ghc(9,10,0)
            e = CExport (reLoc (L l stxt)) (reLoc (L l ces))
#elif MIN_VERSION_ghc(9,6,0)
            e = CExport (L l stxt) (L l ces)
#else
            e = CExport (L l ces) (L l stxt)
#endif
        return (lA l (forD fe))
    _ -> builderError
  | otherwise = builderError
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
    [] -> setLastToken eq_form >> failB "Empty binding"
    lpat@(dL->L l (BangPat _EXT pat)):pats' ->
      case pats' of
        [] -> return (b_patBindD gxd lpat)
        _  -> let name = reLoc (L l (mkRdrName "!"))
              in  b_funBindD name (pat:pats') grhss decls
    lpat@(dL->L _ pat):pats'
      | isVarPat pat -> do
        name <- varToName pat
        b_funBindD (reLoc name) pats' grhss decls
      | null pats'   -> return (b_patBindD gxd lpat)
      | otherwise    -> setLastToken eq_form >> failB "Malformed binding"
  where
    isVarPat VarPat {} = True
    isVarPat _         = False
    varToName (VarPat _EXT lname) = return lname
    varToName _                   = failB "Invalid name"
{-# INLINABLE b_funOrPatD #-}

b_funBindD :: Located RdrName -> [HPat] -> [HGRHS] -> [HDecl] -> Builder HDecl
b_funBindD lname0@(L l _) args grhss decls = do
  let body = mkGRHSs grhss decls l
      lname = reLocA lname0
#if MIN_VERSION_ghc(8,6,0)
      match = lA l (Match NOEXT ctxt args body)
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
      bind = mkFunBind_compat lname [match]
  return (lA l (ValD NOEXT bind))
{-# INLINABLE b_funBindD #-}

b_patBindD :: ([HGRHS],[HDecl]) -> HPat -> HDecl
b_patBindD (grhss,decls) lpat@(dL->L l _pat) =
  let bind = mkPatBind_compat lpat grhss decls
  in  L l (ValD NOEXT bind)
{-# INLINABLE b_patBindD #-}

b_tsigD :: [Code] -> ([HType], HType) -> Builder HDecl
b_tsigD names (ctxts,typ0) = do
  let typ' = hsTypeToHsSigWcType qtyp
      qtyp =
        if null ctxts
          then typ1
#if MIN_VERSION_ghc(9,10,0)
          else lA l (mkHsQualTy' (mkLocatedListA ctxts) typ1)
#else
          else lA l (mkHsQualTy' (la2la (mkLocatedListA ctxts)) typ1)
#endif
      typ1 = unParTy typ0
      mkName form =
        case form of
          LForm (L l1 (Atom (ASymbol name))) -> return (lN l1 (mkRdrName name))
          _                                  -> builderError
      l = getLoc (mkLocatedForm names)
      typeSig = TypeSig NOEXT
  names' <- mapM mkName names
  return (lA l (sigD (typeSig names' typ')))
{-# INLINABLE b_tsigD #-}

#if MIN_VERSION_ghc(9,4,0)
b_inlineD ::
  (SourceText -> InlineSpec) -> Maybe Activation -> Code -> Builder HDecl
#else
b_inlineD :: InlineSpec -> Maybe Activation -> Code -> Builder HDecl
#endif
b_inlineD ispec mb_act (LForm (L l form)) =
  case form of
    Atom (ASymbol name) ->
      let inlineSig = InlineSig NOEXT
      in  return (lA l (sigD (inlineSig (lN l (mkRdrName name)) ipragma)))
    _ -> builderError
  where
    ipragma = mkInlinePragma stxt (ispec', FunLike) mb_act
    source =
      case ispec'' (strToSourceText "") of
        NoInline {}  -> "{-# NOINLINE"
        Inlinable {} -> "{-# INLINABLE"
        _            -> "{-# INLINE"
    stxt = strToSourceText source
#if MIN_VERSION_ghc(9,4,0)
    ispec' = ispec stxt
    ispec'' = ispec
#else
    ispec' = ispec
    ispec'' = const ispec
#endif
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
    source = strToSourceText (show code)
{-# INLINABLE b_activation #-}

b_specializeD :: Code -> Maybe Activation -> (Code, HType) -> Builder HDecl
b_specializeD = specializeBuilder noUserInline "{-# SPECIALISE"
{-# INLINABLE b_specializeD #-}

b_specializeInlineD :: Code -> Maybe Activation -> (Code, HType)
                    -> Builder HDecl
#if MIN_VERSION_ghc(9,4,0)
b_specializeInlineD =
  let str = "{-# SPECIALISE INLINE"
  in  specializeBuilder (Inline (strToSourceText str)) str
#else
b_specializeInlineD = specializeBuilder Inline "{-# SPECIALISE INLINE"
#endif
{-# INLINABLE b_specializeInlineD #-}

specializeBuilder
  :: InlineSpec -> String -> Code -> Maybe Activation -> (Code, HType)
  -> Builder HDecl
specializeBuilder ispec txt (LForm (L l _)) mb_act (nsym, tsig)
  | LForm (L ln (Atom (ASymbol name))) <- nsym = do
  let lname = lN ln (mkRdrName name)
      ipragma = mkInlinePragma source (ispec, FunLike) mb_act
      source = strToSourceText txt
      specSig = SpecSig NOEXT lname [hsTypeToHsSigType tsig] ipragma
  return (lA l (sigD specSig))
  | otherwise = builderError
{-# INLINABLE specializeBuilder #-}

b_docnextD :: Code -> Builder HDecl
b_docnextD (LForm (L l form)) =
  case form of
    Atom (AString _ str) -> return $! lA l (DocD NOEXT (docCommentNext (L l str)))
    _                    -> builderError
{-# INLINABLE b_docnextD #-}

b_docprevD :: Code -> Builder HDecl
b_docprevD (LForm (L l form)) =
  case form of
    Atom (AString _ str) ->
      return $! lA l (DocD NOEXT (DocCommentPrev (mkLHsDoc l str)))
    _ -> builderError
{-# INLINABLE b_docprevD #-}

b_docGroupD :: Int -> Code -> Builder HDecl
b_docGroupD n form@(LForm (L l _))
  | List [_,doc_code] <- unCode form
  , Atom (AString _ doc) <- unCode doc_code
  = return $! lA l (DocD NOEXT (DocGroup (fromIntegral n) (mkLHsDoc l doc)))
  | otherwise = setLastToken form >> failB "Invalid group doc"
{-# INLINABLE b_docGroupD #-}

b_docNamed :: Code -> Builder HDecl
b_docNamed form@(LForm (L l body))
  | List [_,name_code,doc_code] <- body
  , Atom (ASymbol name) <- unCode name_code
  , Atom (AString _ doc) <- unCode doc_code
  = let name' = unpackFS name
    in return $! lA l (DocD NOEXT (DocCommentNamed name' (mkLHsDoc l doc)))
  | otherwise
  = setLastToken form >> failB "Invalid named doc"
{-# INLINABLE b_docNamed #-}

#if MIN_VERSION_ghc(9,4,0)
docCommentNext :: Located FastString -> DocDecl PARSED
#else
docCommentNext :: Located FastString -> DocDecl
#endif
docCommentNext (L l fs) = DocCommentNext . mkLHsDoc l $ fs
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
#if MIN_VERSION_ghc(9,2,0)
    dfid = DataFamInstDecl (mkFamEqn tycon pats rhs)
#elif MIN_VERSION_ghc(8,4,0)
    dfid = DataFamInstDecl (mkHsImplicitBndrs (mkFamEqn tycon pats rhs))
#else
    dfid = DataFamInstDecl { dfid_tycon = tycon
                           , dfid_pats = mkHsImplicitBndrs pats
                           , dfid_fixity = Prefix
                           , dfid_defn = rhs
                           , dfid_fvs = placeHolderNames }
#endif
{-# INLINABLE mkDataFamInstDecl #-}

mkTyFamInstEqn :: Located RdrName -> [HType] -> HType -> TyFamInstEqn PARSED
mkTyFamInstEqn tycon pats rhs =
#if MIN_VERSION_ghc(9,2,0)
  mkFamEqn tycon pats rhs
#elif MIN_VERSION_ghc(8,4,0)
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
#elif MIN_VERSION_ghc(8,4,0)
type FAMEQN p rhs = FamEqn p (HsTyPats p) rhs
#else
type FAMEQN p rhs = TyFamEqn p rhs
#endif

#if MIN_VERSION_ghc(8,4,0)
mkFamEqn :: Located RdrName -> [HType] -> rhs -> FAMEQN PARSED rhs
mkFamEqn tycon pats rhs =
  FamEqn { feqn_tycon = reLocA tycon
         , feqn_fixity = Prefix
         , feqn_rhs = rhs
#  if MIN_VERSION_ghc(9,10,0)
         , feqn_pats = map (HsValArg NOEXT) pats
         , feqn_bndrs = mkHsOuterImplicit
#  elif MIN_VERSION_ghc(8,8,0)
           -- Type synonym "HsTyPats" for `feqn_pats' field changed from
           -- `HsTyPats' to `HsTypeArg' in 8.8.0.
         , feqn_pats = map HsValArg pats
#    if MIN_VERSION_ghc(9,2,0)
         , feqn_bndrs = mkHsOuterImplicit
#    else
         , feqn_bndrs = Nothing
#    endif
#  else
         , feqn_pats = pats
#  endif
#  if   MIN_VERSION_ghc(8,6,0)
         , feqn_ext = NOEXT
#  endif
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
#if MIN_VERSION_ghc(9,2,0)
noUserInline = NoUserInlinePrag
#elif MIN_VERSION_ghc(8,4,0)
noUserInline = NoUserInline
#else
noUserInline = EmptyInlineSpec
#endif
{-# INLINABLE noUserInline #-}

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
#  if MIN_VERSION_ghc(8,8,0)
     mkATDefault' = fmap fst . mkATDefault
#  else
     mkATDefault' = mkATDefault
#  endif
#endif
{-# INLINABLE cd2atdefs #-}

#if !MIN_VERSION_ghc(9,2,0)
hsTypeToHsSigType :: HType -> HSigType
hsTypeToHsSigType = mkLHsSigType
{-# INLINABLE hsTypeToHsSigType #-}

hsTypeToHsSigWcType :: HType -> HSigWcType
hsTypeToHsSigWcType = mkLHsSigWcType
{-# INLINABLE hsTypeToHsSigWcType #-}
#endif
