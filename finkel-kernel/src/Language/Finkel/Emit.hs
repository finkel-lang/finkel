{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Emit Haskell source code from Haskell AST value.
--
-- This module contains types and functions for generating Haskell source code
-- from AST data types defined in ghc package.
--
-- The main purpose is to emit Haskell source code annotated with documentation
-- comments understood by hadddock, so the generated result could be messy.
--
-- Most of the implementations are defined with 'ppr' function from 'Outputable'
-- type class.
--
module Language.Finkel.Emit
  ( HsSrc(..)
  , Hsrc(..)
  , genHsSrc
  , putHsSrc
  ) where

#include "Syntax.h"
#include "ghc_modules.h"

-- base
import Control.Monad.IO.Class            (MonadIO (..))
import System.IO                         (Handle)

#if MIN_VERSION_ghc(9,6,0)
import Data.Foldable                     (toList)
#endif

#if !MIN_VERSION_ghc(9,2,0)
import Data.Maybe                        (fromMaybe)
#endif

#if MIN_VERSION_base(4,11,0)
import Prelude                           hiding ((<>))
#endif

-- ghc
import GHC                               (OutputableBndrId)
import GHC_Data_Bag                      (bagToList, isEmptyBag)
import GHC_Driver_Env                    (HscEnv (..))
import GHC_Driver_Monad                  (GhcMonad (..))
import GHC_Driver_Ppr                    (printForUser, showSDocForUser)
import GHC_Hs                            (HsModule (..))
import GHC_Hs_Binds                      (LHsBinds, LSig, Sig (..), pprDeclList)
import GHC_Hs_Decls                      (ConDecl (..), DocDecl (..),
                                          FamEqn (..), FamilyDecl (..),
                                          FamilyInfo (..), FamilyResultSig (..),
                                          HsDataDefn (..), HsDecl (..),
                                          InjectivityAnn (..), LConDecl,
                                          LDocDecl, LFamilyDecl,
                                          LTyFamDefltDecl, TyClDecl (..),
                                          TyFamInstEqn, pprHsFamInstLHS,
                                          pprTyFamInstDecl)
import GHC_Hs_Extension                  (GhcPass)
import GHC_Hs_ImpExp                     (IE (..), LIE)
import GHC_Hs_Type                       (ConDeclField (..), HsConDetails (..),
                                          HsContext, HsType (..),
                                          HsWildCardBndrs (..), LConDeclField,
                                          LHsContext, LHsQTyVars (..),
                                          pprHsForAll)
import GHC_Types_Basic                   (TopLevelFlag (..))
import GHC_Types_Fixity                  (LexicalFixity (..))
import GHC_Types_Name_Reader             (RdrName)
import GHC_Types_SrcLoc                  (GenLocated (..), sortLocated, unLoc)
import GHC_Utils_Outputable              (Outputable (..), OutputableBndr (..),
                                          SDoc, braces, char, comma, darrow,
                                          dcolon, dot, empty, equals, forAllLit,
                                          fsep, hang, hsep, interpp'SP,
                                          interppSP, lparen, nest, parens,
                                          pprWithCommas, punctuate, sep, text,
                                          vbar, vcat, ($$), ($+$), (<+>), (<>))

#if MIN_VERSION_ghc(9,6,0)
import GHC                               (getNamePprCtx)
#else
import GHC                               (getPrintUnqual)
#define getNamePprCtx getPrintUnqual
#endif

#if MIN_VERSION_ghc(9,6,0)
import GHC.Hs                            (XModulePs (..))
import GHC.Hs.Extension                  (GhcPs)
import Language.Haskell.Syntax.Decls     (DataDefnCons (..),
                                          dataDefnConsNewOrData)
#endif

#if MIN_VERSION_ghc(9,4,0)
import GHC.Hs.Doc                        (LHsDoc, hsDocString)
#else
import GHC_Hs_Doc                        (LHsDocString)
#endif

#if MIN_VERSION_ghc(9,2,0)
import GHC.Driver.Env                    (hsc_units)
import GHC.Hs                            (XRec)
import GHC.Hs.Decls                      (FunDep (..),
                                          HsConDeclGADTDetails (..))
import GHC.Hs.Type                       (HsSigType (..),
                                          pprHsOuterSigTyVarBndrs)
import GHC.Parser.Annotation             (noAnn)
#else
import GHC_Core_Class                    (pprFundeps)
import GHC_Hs_Type                       (HsImplicitBndrs (..), LHsType)
import GHC_Types_SrcLoc                  (Located, noLoc)
#endif

#if MIN_VERSION_ghc(9,2,0)
import GHC_Hs_Type                       (pprLHsContext)
#else
import GHC_Hs_Type                       (noLHsContext, pprLHsContext)
#endif

#if MIN_VERSION_ghc(9,2,0)
import GHC_Utils_Outputable              (arrow)
#else
import GHC_Utils_Outputable              (arrow, pprPanic)
#endif

#if MIN_VERSION_ghc(9,2,0)
import Language.Haskell.Syntax.Extension (IdP)
#else
import GHC_Hs_Extension                  (IdP)
#endif

#if MIN_VERSION_ghc(9,0,0)
import GHC_Hs_Type                       (HsForAllTelescope (..), HsScaled (..),
                                          hsScaledThing, mkHsForAllInvisTele)
import GHC_Utils_Outputable              (Depth (..))
#else
import GHC_Types_Var                     (ForallVisFlag (..))
#endif

#if !MIN_VERSION_ghc(9,0,0)
import GHC_Hs_Type                       (LHsTyVarBndr)
#endif

#if MIN_VERSION_ghc(9,4,0)
import GHC_Hs_Doc                        (HsDocString, renderHsDocString)
#else
import GHC_Hs_Doc                        (HsDocString, unpackHDS)
#endif

-- Internal
import Language.Finkel.Lexer
import Language.Finkel.Syntax.Location


-- ---------------------------------------------------------------------
--
-- Constraints for Outputable
--
-- ---------------------------------------------------------------------

type OUTPUTABLE a pr = (OutputableBndrId pr, a ~ GhcPass pr)


-- ---------------------------------------------------------------------
--
-- Annotation dictionary
--
-- ---------------------------------------------------------------------

{-
isDocComment :: Located AnnotationComment -> Bool
isDocComment x =
  case unLoc x of
    AnnDocCommentNext _  -> True
    AnnDocCommentPrev _  -> True
    AnnDocCommentNamed _ -> True
    AnnDocSection _ _    -> True
    _                    -> False

buildDocMap :: [Located AnnotationComment] -> DocMap
buildDocMap acs = go (Map.empty, Nothing, []) (sortLocated acs)
  where
    go :: ( Map.Map SrcSpan [AnnotationComment]
          , Maybe SrcSpan
          , [AnnotationComment] )
       -> [Located AnnotationComment]
       -> DocMap
    go (acc, keySpan, block) [] =
      case keySpan of
        Nothing -> acc
        Just k  -> Map.insert k (reverse block) acc
    go (acc, keySpan, block) (com:coms) =
      case keySpan of
        Just k ->
          case (k, getLoc com) of
            (RealSrcSpan k', RealSrcSpan com') ->
              if (srcSpanEndLine k' + 1) == srcSpanStartLine com'
                 then go ( acc
                         , Just (combineSrcSpans k (getLoc com))
                         , unLoc com:block )
                         coms
                 else
                   let acc' = Map.insert k (reverse block) acc
                       isDoc = isDocComment com
                       keySpan' | isDoc = Just (getLoc com)
                                | otherwise = Nothing
                       block' | isDoc = [unLoc com]
                              | otherwise = []
                   in  go (acc', keySpan', block') coms
            _ -> go (acc, Nothing, []) coms
        Nothing ->
          if isDocComment com
             then go (acc, Just (getLoc com), [unLoc com]) coms
             else go (acc, Nothing, block) coms

spanStartLine :: SrcSpan -> Int
spanStartLine l =
  case l of
    RealSrcSpan s -> srcSpanStartLine s
    _             -> -1

spanEndLine :: SrcSpan -> Int
spanEndLine l =
  case l of
    RealSrcSpan s -> srcSpanEndLine s
    _             -> -1

-- | Lookup previous documentation comment.
--
-- Here @previous@ means the end line of documentation comment matches
-- to the start line of reference span - offset.
--
lookupPrevDoc :: Int -> SrcSpan -> DocMap -> Maybe [AnnotationComment]
lookupPrevDoc offset l =
  let line = spanStartLine l
      f k a | spanEndLine k == line - offset = Just a
            | otherwise                      = Nothing
  in  Map.foldMapWithKey f

emitPrevDoc :: SPState -> Located a -> SDoc
emitPrevDoc = emitPrevDocWithOffset 1

emitPrevDocWithOffset :: Int -> SPState -> Located a -> SDoc
emitPrevDocWithOffset offset st ref =
  case lookupPrevDoc offset (getLoc ref) (docMap st) of
    Nothing -> empty
    Just as -> vcat (map f as)
  where
    f annotated = case annotated of
      AnnDocCommentNext doc -> case lines doc of
        c:cs -> vcat ((text "-- | " <> text c):
                      map (\ x -> text "--" <> text x) cs)
        []   -> empty
      AnnLineComment doc -> text "-- " <> text doc
      _                  -> ppr annotated

#if !MIN_VERSION_ghc(8,4,0)
-- | 'whenPprDebug' does not exist in ghc 8.2. Defining one with
-- 'ifPprDebug'. Also, number of arguments in 'ifPprDebug' changed in
-- ghc 8.4.
whenPprDebug :: SDoc -> SDoc
whenPprDebug d = ifPprDebug d
#endif

-}


-- ---------------------------------------------------------------------
--
-- HsSrc class
--
-- ---------------------------------------------------------------------

-- | Type class for generating textual source code.
class HsSrc a where
  toHsSrc :: SPState -> a -> SDoc

-- | A wrapper type to specify instance of 'HsSrc'.
newtype Hsrc a = Hsrc {unHsrc :: a}

-- | Generate textual source code from given data.
genHsSrc :: (GhcMonad m, HsSrc a) => SPState -> a -> m String
genHsSrc st0 x = do
  hsc_env <- getSession
  let dflags = hsc_dflags hsc_env
  unqual <- getNamePprCtx
#if MIN_VERSION_ghc(9,2,0)
  return (showSDocForUser dflags (hsc_units hsc_env) unqual (toHsSrc st0 x))
#else
  return (showSDocForUser dflags unqual (toHsSrc st0 x))
#endif

-- | Print textual source code of given data to given 'Handle'.
putHsSrc :: (GhcMonad m, HsSrc a, MonadIO m) => Handle -> SPState -> a -> m ()
putHsSrc hdl st0 x = do
  hsc_env <- getSession
  unqual <- getNamePprCtx
  let dflags = hsc_dflags hsc_env
#if MIN_VERSION_ghc(9,0,1)
      render = printForUser dflags hdl unqual AllTheWay
#else
      render = printForUser dflags hdl unqual
#endif
  liftIO (render (toHsSrc st0 x))


-- ---------------------------------------------------------------------
--
-- Instances
--
-- ---------------------------------------------------------------------

instance HsSrc RdrName where
  toHsSrc _ = ppr

instance (HsSrc b) => HsSrc (GenLocated a b) where
  toHsSrc st (L _ e) = toHsSrc st e

-- Some CPP macros to pattern match with constructors of HsModule.

#if MIN_VERSION_ghc(9,6,0)
#define HSMODEXT modExt
#define HSMODDEPREC {- no hsmodDeprecMessage -}
#define HSMODMBDOC {- no hsmodHaddockModHeader -}
#else
#define HSMODEXT {- no hsmodExt -}
#define HSMODDEPREC deprec
#define HSMODMBDOC mbDoc
#endif

#if !MIN_VERSION_ghc(9,2,0) || MIN_VERSION_ghc(9,6,0)
#define HSMODANN {- no hsmodAnn -}
#else
#define HSMODANN _hsmodAnn
#endif

#if !MIN_VERSION_ghc(9,0,0) || MIN_VERSION_ghc(9,6,0)
#define LAYOUTINFO {- no LayoutInfo -}
#else
#define LAYOUTINFO _layout_info
#endif

-- HsModule used had an argument until 9.0, and then the argument came back in
-- 9.6.
#if MIN_VERSION_ghc(9,6,0)
instance HsSrc (Hsrc (HsModule GhcPs)) where
#elif MIN_VERSION_ghc(9,0,0)
instance HsSrc (Hsrc HsModule) where
#else
instance OUTPUTABLE a pr => HsSrc (Hsrc (HsModule a)) where
#endif
  toHsSrc st a = case unHsrc a of
    HsModule HSMODEXT HSMODANN LAYOUTINFO mb_name exports imports decls
      HSMODDEPREC HSMODMBDOC ->
        vcat ([ pp_headerPragmas st
              , pp_mbdocn mbDoc ] ++
              mb_header ++
              [ pp_nonnull imports
              , hsSrc_nonnull st (map unLoc decls)
              , text "" ])
      where
        mb_header =
          case mb_name of
            Nothing -> []
            Just name -> [ case exports of
                             Nothing -> pp_header name (text "where")
                             Just es -> vcat [ pp_header name lparen
                                             , nest 8 (pp_lies st (unLoc es))
                                             , nest 4 (text ") where") ]]
        pp_header name rest =
          case deprec of
            Nothing -> pp_modname name <+> rest
            Just d  -> vcat [pp_modname name, ppr d, rest]
        pp_modname name =
          text "module" <+> ppr name
#if MIN_VERSION_ghc(9,6,0)
        mbDoc = hsmodHaddockModHeader modExt
        deprec = hsmodDeprecMessage modExt
#endif

instance OUTPUTABLE a pr => HsSrc (Hsrc (IE a)) where
  toHsSrc _st (Hsrc ie) =
    case ie of
      IEGroup _EXT n doc  -> commentWithHeader ("-- " ++ replicate n '*')
                                               (getHsDocString doc)
      IEDoc _EXT doc      -> commentWithHeader "-- |" (getHsDocString doc)
      IEDocNamed _EXT doc -> text ("-- $" ++ doc)
      _                   -> ppr ie


-- --------------------------------------------------------------------
--
-- Top level declarations
--
-----------------------------------------------------------------------

instance OUTPUTABLE a pr => HsSrc (HsDecl a) where
  toHsSrc st decl =
    case decl of
      SigD  _EXT sig   -> toHsSrc st sig
      TyClD _EXT tycld -> toHsSrc st tycld
      DocD _EXT doc    -> toHsSrc st doc
      _                -> ppr decl


-- --------------------------------------------------------------------
--
-- Type signature
--
-----------------------------------------------------------------------

instance OUTPUTABLE a pr => HsSrc (Sig a) where
  toHsSrc st sig = case sig of
    TypeSig _EXT vars ty -> pprVarSig (map unLoc vars)
                                      (toHsSrc st ty)
    ClassOpSig _EXT is_dflt vars ty
      | is_dflt   -> text "default" <+> pprVarSig (map unLoc vars)
                                                  (toHsSrc st ty)
      | otherwise -> pprVarSig (map unLoc vars) (toHsSrc st ty)
    _ -> ppr sig

#if MIN_VERSION_ghc(9,2,0)
instance (OUTPUTABLE a pr) => HsSrc (HsSigType a) where
  toHsSrc st (HsSig { sig_bndrs = outer_bndrs, sig_body = body}) =
    pprHsOuterSigTyVarBndrs outer_bndrs <+> toHsSrc st body
#endif

instance (OUTPUTABLE a pr, Outputable thing, HsSrc thing)
          => HsSrc (HsWildCardBndrs a thing) where
  toHsSrc st wc = case wc of
    HsWC { hswc_body = ty } -> toHsSrc st ty
#if !MIN_VERSION_ghc(9,0,0)
    _                       -> ppr wc
#endif

#if !MIN_VERSION_ghc(9,2,0)
instance (OUTPUTABLE a pr)
         => HsSrc (HsImplicitBndrs a (LHsType a)) where
  toHsSrc st ib =
    case ib of
      HsIB { hsib_body = ty } -> toHsSrc st ty
#  if !MIN_VERSION_ghc(9,0,0)
      _                       -> ppr ib
#  endif
#endif

instance (OUTPUTABLE a pr) => HsSrc (HsType a) where
  toHsSrc st ty = case ty of
#if MIN_VERSION_ghc(9,0,0)
    HsForAllTy {hst_tele=tele, hst_body=ty1} ->
      sep [pprHsTele tele, hsrc ty1]
#else
    HsForAllTy {hst_bndrs=tvs, hst_body=ty1} ->
      sep [pprHsForAllTvs tvs, hsrc ty1]
#endif
#if MIN_VERSION_ghc(9,4,0)
    HsQualTy {hst_ctxt=ctxt, hst_body=ty1} ->
      sep [ pprHsContextAlways (unLoc ctxt)
          , hsrc ty1 ]
#elif MIN_VERSION_ghc(9,2,0)
    HsQualTy {hst_ctxt=ctxt, hst_body=ty1} ->
      sep [ maybe (parens empty <+> darrow) (pprHsContextAlways . unLoc) ctxt
          , hsrc ty1]
#else
    HsQualTy {hst_ctxt=L _ ctxt, hst_body=ty1} ->
      sep [pprHsContextAlways ctxt, hsrc ty1]
#endif
#if MIN_VERSION_ghc(9,0,0)
    HsFunTy _EXT _arrow ty1 ty2 -> t_arr_t ty1 ty2
#else
    HsFunTy _EXT ty1 ty2 -> t_arr_t ty1 ty2
#endif
    HsDocTy _EXT ty1 (L _ docstr) ->
#if MIN_VERSION_ghc(9,4,0)
      ppr ty1 $+$ commentWithHeader "-- ^" (hsDocString docstr)
#else
      ppr ty1 $+$ commentWithHeader "-- ^" docstr
#endif
    HsParTy _EXT ty1 -> parens (hsrc ty1)
    _ -> ppr ty
    where
      t_arr_t t1 t2 = sep [hsrc t1, text "->", hsrc t2]
      hsrc :: HsSrc a => a -> SDoc
      hsrc = toHsSrc st

#if MIN_VERSION_ghc(9,0,0)
instance (HsSrc a) => HsSrc (HsScaled pass a) where
    toHsSrc st (HsScaled _cnt t) = toHsSrc st t
#endif

#if MIN_VERSION_ghc(9,0,0)
pprHsTele :: OutputableBndrId p => HsForAllTelescope (GhcPass p) -> SDoc
pprHsTele hsfat =
  case hsfat of
    HsForAllVis {hsf_vis_bndrs=_bndrs}->
      error "pprHsTele: HsForAllVis NYI"
    HsForAllInvis {hsf_invis_bndrs=bndrs} ->
      forAllLit <+> interppSP bndrs <> dot
#else
pprHsForAllTvs :: OUTPUTABLE n pr => [LHsTyVarBndr n] -> SDoc
pprHsForAllTvs qtvs
  | null qtvs = forAllLit <+> dot
  | otherwise = forAllLit <+> interppSP qtvs <> dot
#endif

-- From 'HsBinds.pprVarSig'.
pprVarSig :: OutputableBndr id => [id] -> SDoc -> SDoc
pprVarSig vars pp_ty = sep [pprvars <+> dcolon, nest 2 pp_ty]
  where
    pprvars = hsep $ punctuate comma (map pprPrefixOcc vars)

-- From 'HsTypes.pprHsContextAlways'.
pprHsContextAlways :: OUTPUTABLE n pr => HsContext n -> SDoc
pprHsContextAlways []       = parens empty <+> darrow
pprHsContextAlways [L _ ty] = ppr ty <+> darrow
pprHsContextAlways cxt      = parens (interpp'SP cxt) <+> darrow


-- --------------------------------------------------------------------
--
-- TyClDecl
--
-- --------------------------------------------------------------------

instance OUTPUTABLE a pr => HsSrc (TyClDecl a) where
  toHsSrc st tcd =
    case tcd of
      SynDecl { tcdLName = ltycon, tcdTyVars = tyvars
              , tcdFixity = fixity, tcdRhs = rhs } ->
        hang (text "type" <+>
#if MIN_VERSION_ghc(9,2,0)
              pp_vanilla_decl_head ltycon tyvars fixity Nothing <+>
#else
              pp_vanilla_decl_head ltycon tyvars fixity [] <+>
#endif
              equals)
           4 (toHsSrc st rhs)
      DataDecl { tcdLName = ltycon, tcdTyVars = tyvars
               , tcdFixity = fixity, tcdDataDefn = defn } ->
        pp_data_defn st (pp_vanilla_decl_head ltycon tyvars fixity) defn
      ClassDecl { tcdCtxt = context, tcdLName = lclas
                , tcdTyVars = tyvars, tcdFixity = fixity
                , tcdFDs = fds, tcdSigs = sigs, tcdMeths = methods
                , tcdATs = ats, tcdATDefs = at_defs
                , tcdDocs = docs }
        | null sigs && isEmptyBag methods && null ats && null at_defs
        -> top_matter
        | otherwise
        -> vcat
             [ top_matter <+> text "where"
             , nest 2
                    (pprDeclList
                      (ppr_cdecl_body st ats at_defs methods sigs docs))]
        where
          top_matter =
            text "class"
#if MIN_VERSION_ghc(9,2,0)
            <+> pp_vanilla_decl_head lclas tyvars fixity context
#else
            <+> pp_vanilla_decl_head lclas tyvars fixity (unLoc context)
#endif
            <+> pprFundeps (map unLoc fds)
      _ -> ppr tcd

#if MIN_VERSION_ghc(9,2,0)
-- Until ghc 9.0.2, GHC.Core.pprFundeps were reused.
pprFundeps :: OutputableBndrId p => [FunDep (GhcPass p)] -> SDoc
pprFundeps []  = empty
pprFundeps fds = hsep (vbar : punctuate comma (map pprFunDep fds))

pprFunDep :: OutputableBndrId p => FunDep (GhcPass p) -> SDoc
pprFunDep (FunDep _ us vs) = hsep [interppSP us, arrow, interppSP vs]

#endif

-- --------------------------------------------------------------------
--
-- For SynDecl and DataDecl
--
-- --------------------------------------------------------------------

pp_data_defn :: OUTPUTABLE n pr
             => SPState
#if MIN_VERSION_ghc(9,2,0)
             -> (Maybe (LHsContext n) -> SDoc)
#else
             -> (HsContext n -> SDoc)
#endif
             -> HsDataDefn n
             -> SDoc
pp_data_defn
  st pp_hdr HsDataDefn { dd_cType = mb_ct, dd_kindSig = mb_sig
#if !MIN_VERSION_ghc(9,6,0)
                       , dd_ND = new_or_data
#endif
#if MIN_VERSION_ghc(9,2,0)
                       , dd_ctxt = context
#else
                       , dd_ctxt = L _ context
#endif
                       , dd_cons = condecls
                       , dd_derivs = derivings }
  | null condecls
  = ppr new_or_data <+> pp_ct <+> pp_hdr context <+> pp_sig
    <+> pp_derivings derivings
  | otherwise
  = hang (ppr new_or_data <+> pp_ct <+> pp_hdr context <+> pp_sig)
       2 (pp_cds condecls $$ pp_derivings derivings)
  where
#if MIN_VERSION_ghc(9,6,0)
    new_or_data = dataDefnConsNewOrData condecls
    pp_cds cds =
      case cds of
        -- The _bool field in `DataTypeCOns' is for TypeData language extension
        -- introduced in ghc 9.6.1.
        NewTypeCon c          -> text "=" <+> pprConDecl st (unLoc c)
        DataTypeCons _bool cs -> pp_condecls st cs
#else
    pp_cds = pp_condecls st
#endif
    pp_ct = maybe empty ppr mb_ct
    pp_sig = maybe empty ((dcolon <+>) . ppr) mb_sig
#if MIN_VERSION_ghc(9,2,0)
    pp_derivings ds       = vcat (map ppr ds)
#else
    pp_derivings (L _ ds) = vcat (map ppr ds)
#endif
#if !MIN_VERSION_ghc(9,0,0)
pp_data_defn _ _ (XHsDataDefn x) = ppr x
#endif

-- Modified version of 'HsDecls.pp_condecls', no space in front of "|",
-- taking 'SPState' as first argument.
pp_condecls :: (OUTPUTABLE n pr) => SPState -> [LConDecl n] -> SDoc
pp_condecls st cs@(L _ ConDeclGADT {} : _) =
  hang (text "where") 2 (vcat (map (pprConDecl st . unLoc) cs))
pp_condecls st cs =
  equals <+> sep (punctuate (text " |") (map (pprConDecl st . unLoc) cs))

-- For pattern match of 'PrefixCon' in below 'pprConDecl' function.

#if MIN_VERSION_ghc(9,2,0)
#define _TYARGS _
#else
#define _TYARGS {- no [tyargs] -}
#endif

-- Modified version of 'HsDecls.pprConDecl'. This function does the pretty
-- printing of documentation for constructors.
--
-- Although the syntax parser for constructor documentation accepts ":doc^"
-- form, this function emit documentation before the constructor declaration, to
-- support documentation for constructor argument. This is because haddock may
-- concatenate the docstring for the last constructor argument and the docstring
-- for constructor itself.
pprConDecl :: OUTPUTABLE n pr => SPState -> ConDecl n -> SDoc
pprConDecl st condecl@ConDeclH98 {} =
  pp_mbdocn doc $+$ sep [hforall, ppr_details details]
  where
#if MIN_VERSION_ghc(9,2,0)
    hforall = pprHsForAll' (mkHsForAllInvisTele noAnn tvs) mcxt
#elif MIN_VERSION_ghc(9,0,0)
    hforall = pprHsForAll' (mkHsForAllInvisTele tvs) cxt
    cxt = fromMaybe (noLoc []) mcxt
#else
    hforall = pprHsForAll' tvs cxt
    cxt = fromMaybe (noLoc []) mcxt
#endif
    ConDeclH98 { con_name = L _ con
               , con_ex_tvs = tvs
               , con_mb_cxt = mcxt
               , con_args = details
               , con_doc = doc } = condecl
#if MIN_VERSION_ghc(9,0,0)
    ppr_details (InfixCon t1 t2) =
      hsep [hsrc (hsScaledThing t1), pprInfixOcc con, hsrc (hsScaledThing t2)]
    ppr_details (PrefixCon _TYARGS tys) =
      sep (pprPrefixOcc con : map (hsrc . unLoc . hsScaledThing) tys)
#else
    ppr_details (InfixCon t1 t2) =
      hsep [hsrc t1, pprInfixOcc con, hsrc t2]
    ppr_details (PrefixCon tys) =
      sep (pprPrefixOcc con : map (hsrc . unLoc) tys)
#endif
    ppr_details (RecCon fields) =
      pprPrefixOcc con <+> pprConDeclFields (unLoc fields)
    hsrc :: HsSrc a => a -> SDoc
    hsrc = toHsSrc st

pprConDecl st ConDeclGADT { con_names = cons
#if MIN_VERSION_ghc(9,2,0)
                          , con_bndrs = L _ outer_bndrs
                          , con_g_args = args
#else
                          , con_qvars = qvars
                          , con_args = args
#endif
                          , con_mb_cxt = mcxt
                          , con_res_ty = res_ty
                          , con_doc = doc }
  = pp_mbdocn doc $+$ ppr_con_names cons' <+> dcolon
    <+> sep [hforall -- pprHsForAll' (hsq_explicit qvars) cxt
            ,ppr_arrow_chain (get_args args ++ [hsrc res_ty])]
  where
#if MIN_VERSION_ghc(9,6,0)
    cons' = toList cons
#else
    cons' = cons
#endif
#if MIN_VERSION_ghc(9,2,0)
    hforall = pprHsOuterSigTyVarBndrs outer_bndrs <+> pprLHsContext mcxt
#elif MIN_VERSION_ghc(9,0,0)
    hforall = pprHsForAll' (mkHsForAllInvisTele qvars) cxt
    cxt = fromMaybe (noLoc []) mcxt
#else
    hforall = pprHsForAll' (hsq_explicit qvars) cxt
    cxt = fromMaybe (noLoc []) mcxt
#endif
#if MIN_VERSION_ghc(9,10,0)
    get_args (PrefixConGADT _x csts) = map hsrc csts
    get_args (RecConGADT _ fields)   = [pprConDeclFields (unLoc fields)]
#elif MIN_VERSION_ghc(9,4,0)
    get_args (PrefixConGADT csts)    = map hsrc csts
    get_args (RecConGADT fields _)   = [pprConDeclFields (unLoc fields)]
#elif MIN_VERSION_ghc(9,2,0)
    get_args (PrefixConGADT csts)    = map hsrc csts
    get_args (RecConGADT fields)     = [pprConDeclFields (unLoc fields)]
#else
    get_args (PrefixCon as)          = map hsrc as
    get_args (RecCon fields)         = [pprConDeclFields (unLoc fields)]
    get_args (InfixCon {})           = pprPanic "pprConDecl:GADT" (ppr cons)
#endif
    ppr_arrow_chain []     = empty
    ppr_arrow_chain (a:as) = sep (a : map (arrow <+>) as)
    hsrc :: HsSrc a => a -> SDoc
    hsrc = toHsSrc st

#if !MIN_VERSION_ghc(9,0,0)
pprConDecl _ con = ppr con
#endif

-- From 'HsDecls.ppr_con_names'.
#if MIN_VERSION_ghc(9,2,0)
ppr_con_names :: OutputableBndr a => [GenLocated l a] -> SDoc
#else
ppr_con_names :: OutputableBndr a => [Located a] -> SDoc
#endif
ppr_con_names = pprWithCommas (pprPrefixOcc . unLoc)

-- Modified version of 'HsTypes.pprConDeclFields', to emit documentation
-- comments of fields in record data type.
pprConDeclFields :: OUTPUTABLE n pr => [LConDeclField n] -> SDoc
pprConDeclFields fields =
  braces (sep (punctuate comma (map ppr_fld fields)))
  where
    ppr_fld (L _ ConDeclField { cd_fld_names = ns
                              , cd_fld_type = ty
                              , cd_fld_doc = mb_doc })
      = ppr_names ns <+> dcolon <+> ppr ty
        $+$ pp_mbdocp mb_doc $+$ text ""
    ppr_fld (L _ (XConDeclField x)) = ppr x
    ppr_names [n] = ppr n
    ppr_names ns  = sep (punctuate comma (map ppr ns))

-- From 'HsDecls.pp_vanilla_decl_head'.
pp_vanilla_decl_head :: (OUTPUTABLE n pr)
#if MIN_VERSION_ghc(9,2,0)
                     => XRec n (IdP n)
#else
                     => Located (IdP n)
#endif
                     -> LHsQTyVars n
                     -> LexicalFixity
#if MIN_VERSION_ghc(9,2,0)
                     -> Maybe (LHsContext n)
#else
                     -> HsContext n
#endif
                     -> SDoc
pp_vanilla_decl_head thing HsQTvs {hsq_explicit=tyvars} fixity context =
  hsep [pprHsContext context, pp_tyvars tyvars]
  where
    pp_tyvars (varl:varsr)
      | fixity == Infix, varsr_hd:varsr_tl <- varsr
      = hsep [ char '(', ppr (unLoc varl), pprInfixOcc (unLoc thing)
             , ppr (unLoc varsr_hd), char ')'
             , hsep (map (ppr . unLoc) varsr_tl) ]
      | fixity == Infix
      = hsep [ ppr (unLoc varl), pprInfixOcc (unLoc thing)
             , hsep (map (ppr . unLoc) varsr) ]
      | otherwise = hsep [ pprPrefixOcc (unLoc thing)
                         , hsep (map (ppr . unLoc) (varl: varsr)) ]
    pp_tyvars [] = pprPrefixOcc (unLoc thing)
#if !MIN_VERSION_ghc(9,0,0)
pp_vanilla_decl_head _ (XLHsQTyVars x) _ _ = ppr x
#endif


-- --------------------------------------------------------------------
--
-- For ClassDecl
--
-- --------------------------------------------------------------------

ppr_cdecl_body :: OUTPUTABLE n pr
               => SPState
               -> [LFamilyDecl n]
#if MIN_VERSION_ghc(8,10,1)
               -> [LTyFamDefltDecl n]
#else
               -> [LTyFamDefltEqn n]
#endif
               -> LHsBinds n
               -> [LSig n]
#if MIN_VERSION_ghc(9,2,0)
               -> [LDocDecl n]
#else
               -> [LDocDecl]
#endif
               -> [SDoc]
ppr_cdecl_body st ats at_defs methods sigs docs = body
  where
    body = map unLoc (sortLocated body0)
    body0 =
      map (reLoc . fmap (pprFamilyDecl NotTopLevel)) ats ++
#if MIN_VERSION_ghc(9,2,0)
      map (reLoc . fmap (pprTyFamInstDecl NotTopLevel)) at_defs ++
#else
      map (\d@(L l _) -> L l (ppr_fam_deflt_eqn d)) at_defs ++
#endif
      map (reLoc . fmap (toHsSrc st)) sigs ++
      map (reLoc . fmap ppr) (bagToList methods) ++
      map (reLoc . fmap (toHsSrc st)) docs

#if MIN_VERSION_ghc(9,2,0)
#define XCINJECTIVITYANN unused
#define _XCINJECTIVITYANN _
#else
#define XCINJECTIVITYANN {- no XCInjectivityAnn -}
#define _XCINJECTIVITYANN {- no XCInjectivityAnn -}
#endif

-- From 'HsDecls.pprFamilyDecl'. Used during pretty printing type class body
-- contents, with first argument set to 'NonTopLevel'.
pprFamilyDecl :: (OUTPUTABLE n pr)
              => TopLevelFlag -> FamilyDecl n -> SDoc
pprFamilyDecl top_level FamilyDecl { fdInfo = info
                                   , fdLName = ltycon
                                   , fdTyVars = tyvars
                                   , fdFixity = fixity
                                   , fdResultSig = L _ result
                                   , fdInjectivityAnn = mb_inj }
  = vcat [ pprFlavour info <+> pp_top_level <+>
#if MIN_VERSION_ghc(9,2,0)
           pp_vanilla_decl_head ltycon tyvars fixity Nothing <+>
#else
           pp_vanilla_decl_head ltycon tyvars fixity [] <+>
#endif
           pp_kind <+> pp_inj <+> pp_where
         , nest 2 pp_eqns ]
  where
    pp_top_level = case top_level of
                     TopLevel    -> text "family"
                     NotTopLevel -> empty

    pp_kind = case result of
                NoSig    _EXT         -> empty
                KindSig  _EXT kind    -> dcolon <+> ppr kind
                TyVarSig _EXT tv_bndr -> text "=" <+> ppr tv_bndr
#if !MIN_VERSION_ghc(9,0,0)
                XFamilyResultSig x    -> ppr x
#endif
    pp_inj = case mb_inj of
               Just (L _ (InjectivityAnn _XCINJECTIVITYANN lhs rhs)) ->
                 hsep [ vbar, ppr lhs, text "->", hsep (map ppr rhs) ]
               Nothing -> empty
    (pp_where, pp_eqns) = case info of
      ClosedTypeFamily mb_eqns ->
        ( text "where"
        , case mb_eqns of
            Nothing   -> text ".."
            Just eqns -> vcat $ map (ppr_fam_inst_eqn . unLoc) eqns )
      _ -> (empty, empty)
#if !MIN_VERSION_ghc(9,0,0)
pprFamilyDecl _ (XFamilyDecl x) = ppr x
#endif

-- From 'HsDecls.pprFlavour'.
pprFlavour :: FamilyInfo pass -> SDoc
pprFlavour DataFamily          = text "data"
pprFlavour OpenTypeFamily      = text "type"
pprFlavour ClosedTypeFamily {} = text "type"

-- From 'HsDecls.ppr_fam_inst_eqn'
ppr_fam_inst_eqn :: (OUTPUTABLE n pr) => TyFamInstEqn n -> SDoc
#if MIN_VERSION_ghc(9,2,0)
ppr_fam_inst_eqn (FamEqn { feqn_tycon = L _ tycon
                         , feqn_bndrs = bndrs
                         , feqn_pats = pats
                         , feqn_fixity = fixity
                         , feqn_rhs = rhs })
    = pprHsFamInstLHS tycon bndrs pats fixity Nothing <+> equals <+> ppr rhs
#else
ppr_fam_inst_eqn HsIB { hsib_body = FamEqn { feqn_tycon = L _ tycon
                                           , feqn_bndrs = bndrs
                                           , feqn_pats = pats
                                           , feqn_fixity = fixity
                                           , feqn_rhs = rhs }}
    = pprHsFamInstLHS tycon bndrs pats fixity noLHsContext <+>
      equals <+> ppr rhs
#  if !MIN_VERSION_ghc(9,0,0)
ppr_fam_inst_eqn (XHsImplicitBndrs x) = ppr x
ppr_fam_inst_eqn _ = error "ppr_fam_inst_eqn"
#  endif
#endif

-- From 'HsDecls.ppr_fam_deflt_eqn'
#if !MIN_VERSION_ghc(9,2,0)
ppr_fam_deflt_eqn :: OUTPUTABLE n pr => LTyFamDefltDecl n -> SDoc
ppr_fam_deflt_eqn (L _ tfdd) = pprTyFamInstDecl NotTopLevel tfdd
#endif


-- ---------------------------------------------------------------------
--
-- DocDecl
--
-- ---------------------------------------------------------------------

#if MIN_VERSION_ghc(9,4,0)
#define DOCDECL (DocDecl pass)
#else
#define DOCDECL DocDecl
#endif

instance HsSrc DOCDECL where
  toHsSrc _st d = case d of
    DocCommentNext ds       -> text ""
                               $+$ commentWithHeader "-- |" (getHsDocString ds)
    DocCommentPrev ds       -> text ""
                               $+$ commentWithHeader "-- ^" (getHsDocString ds)
                               $+$ text ""
    DocCommentNamed name ds -> namedDoc name (getHsDocString ds)
    DocGroup n ds           -> let stars = replicate n '*'
                               in  commentWithHeader ("-- " ++ stars)
                                     (getHsDocString ds)
    where
      namedDoc name doc =
        let body = map (\x -> text "--" <+> text x)
                       (lines (unpackHDS' doc))
        in  vcat (text "" : text ("-- $" ++ name) : text "--" : body)


-- -------------------------------------------------------------------
--
-- Auxiliary
--
-- -------------------------------------------------------------------

pp_nonnull :: Outputable t => [t] -> SDoc
pp_nonnull [] = empty
pp_nonnull xs = vcat (map ppr xs)

#if MIN_VERSION_ghc(9,4,0)
pp_mbdocn :: Maybe (LHsDoc pass) -> SDoc
pp_mbdocn = maybe empty (commentWithHeader "-- |" . getHsDocString)

pp_mbdocp :: Maybe (LHsDoc pass) -> SDoc
pp_mbdocp = maybe empty (commentWithHeader "-- ^" . getHsDocString)
#else
pp_mbdocn :: Maybe LHsDocString -> SDoc
pp_mbdocn = maybe empty (commentWithHeader "-- |" . unLoc)

pp_mbdocp :: Maybe LHsDocString -> SDoc
pp_mbdocp = maybe empty (commentWithHeader "-- ^" . unLoc)
#endif

pp_headerPragmas :: SPState -> SDoc
pp_headerPragmas sp = vcat sorted_pragmas
  where
    sorted_pragmas = map unLoc (sortLocated pragmas)
    pragmas = map lang (langExts sp) ++
              map ghc_opt (ghcOptions sp) ++
              map haddock_opt (haddockOptions sp)

    lang (L l e) = L l (gen "LANGUAGE" e)
    ghc_opt (L l o) = L l (gen "OPTIONS_GHC" o)
    haddock_opt (L l o) = L l (gen "OPTIONS_HADDOCK" o)
    gen label x = text "{-#" <+> text label <+> text x <+> text "#-}"

hsSrc_nonnull :: HsSrc a => SPState -> [a] -> SDoc
hsSrc_nonnull st xs =
  case xs of
    [] -> empty
    _  -> vcat (map (toHsSrc st) xs)

commentWithHeader :: String -> HsDocString -> SDoc
commentWithHeader header doc =
  case lines (unpackHDS' doc) of
    []   -> empty
    d:ds -> vcat ((text header <+> text d):
                  map (\ x -> text "--" <+> text x) ds)

-- | Format located export elements.
--
-- This function converts module export elements and comments to 'SDoc'.
-- Export elements are punctuated with commas, and newlines are inserted
-- between documentation comments.
pp_lies :: OUTPUTABLE a pr => SPState -> [LIE a] -> SDoc
pp_lies st = go
  where
    go [] = empty
    go ds =
      case break (isDocIE . unLoc) ds of
        (nondocs, rest) ->
          let sdoc = fsep (punctuate comma (map (toHsSrc st . Hsrc . unLoc)
                                                nondocs))
              sdoc' = case nondocs of
                        [] -> sdoc
                        _  -> sdoc <> comma
          in  case rest of
                []        -> sdoc
                doc:rest' -> sdoc'
                             $+$ toHsSrc st (Hsrc (unLoc doc))
                             $+$ go rest'

-- | 'True' when the argument is for documentation.
isDocIE :: IE a -> Bool
isDocIE ie =
  case ie of
    IEGroup {}    -> True
    IEDoc {}      -> True
    IEDocNamed {} -> True
    _             -> False

-- | GHC version compatible function for unpacking 'HsDocString'.
unpackHDS' :: HsDocString -> String
#if MIN_VERSION_ghc(9,4,0)
-- XXX: Consider using 'exactPrintHsDocString' or 'pprHsDocString'.
unpackHDS' = renderHsDocString
#else
unpackHDS' = unpackHDS
#endif

-- | GHC version compatible function for pretty printing 'HsContext'.
#if MIN_VERSION_ghc(9,2,0)
pprHsContext :: OUTPUTABLE a pr => Maybe (LHsContext a) -> SDoc
pprHsContext = pprLHsContext
#else
pprHsContext :: OUTPUTABLE n a => HsContext (GhcPass a) -> SDoc
pprHsContext = pprLHsContext . noLoc
#endif

-- | GHC version compatible function for pretty printing @forall@.
#if MIN_VERSION_ghc(9,2,0)
pprHsForAll'
  :: OUTPUTABLE a pr => HsForAllTelescope a -> Maybe (LHsContext a) -> SDoc
pprHsForAll' = pprHsForAll
#elif MIN_VERSION_ghc(9,0,0)
pprHsForAll'
  :: OUTPUTABLE a pr => HsForAllTelescope a -> LHsContext a -> SDoc
pprHsForAll' = pprHsForAll
#else
pprHsForAll' :: OUTPUTABLE a pr => [LHsTyVarBndr a] -> LHsContext a -> SDoc
pprHsForAll' = pprHsForAll ForallInvis
#endif

#if MIN_VERSION_ghc(9,4,0)
getHsDocString :: LHsDoc pass -> HsDocString
getHsDocString = hsDocString . unLoc
#else
getHsDocString :: HsDocString -> HsDocString
getHsDocString = id
#endif
{-# INLINABLE getHsDocString #-}
