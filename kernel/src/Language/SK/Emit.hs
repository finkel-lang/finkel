{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Emit Haskell source code from Haskell syntax data type.
--
-- This module contains types and functions for generating Haskell
-- source code from AST data types defined in ghc package.
--
-- The main purpose is to emit Haskell source code annotated with
-- documentation comments understood by hadddock, so the generated
-- result could be messy.
--
-- Most of the implementations are defined with 'ppr' function from
-- 'Outputable' type class.
--
module Language.SK.Emit
  ( HsSrc(..)
  , Hsrc(..)
  , genHsSrc
  ) where

-- base
#if MIN_VERSION_base(4,11,0)
import Prelude hiding ((<>))
#endif
import Data.Maybe (fromMaybe)

-- ghc
import BasicTypes (LexicalFixity(..))
import GHC (OutputableBndrId, getPrintUnqual)
import GhcMonad (GhcMonad(..), getSessionDynFlags)
import HsBinds (Sig(..))
import HsDecls ( ConDecl(..), DocDecl(..), HsDataDefn(..), HsDecl(..)
               , LConDecl, TyClDecl(..) )
import HsDoc (LHsDocString)
import HsImpExp (IE(..), LIE)
import HsSyn (HsModule(..))
import HsTypes ( ConDeclField(..), HsConDetails(..), HsContext
               , HsImplicitBndrs(..), HsWildCardBndrs(..), HsType(..)
               , LConDeclField, LHsQTyVars(..), LHsType, LHsTyVarBndr
               , pprHsContext, pprHsForAll )
import Outputable ( (<+>), (<>), ($$), ($+$), Outputable(..)
                  , OutputableBndr(..), SDoc
                  , braces, char, comma, dcolon, darrow, dot, empty
                  , equals, forAllLit, fsep, hang, hsep, interppSP
                  , interpp'SP, lparen, nest, parens, punctuate
                  , showSDocForUser, sep, text, vcat )
import RdrName (RdrName)
import SrcLoc ( GenLocated(..), Located, noLoc, unLoc )

#if MIN_VERSION_ghc(8,6,0)
import HsDoc (HsDocString, unpackHDS)
import HsExtension (GhcPass, IdP)
#elif MIN_VERSION_ghc(8,4,0)
import HsDoc (HsDocString(..))
import HsExtension (SourceTextX, IdP)
import FastString (unpackFS)
#else
#define IdP {- empty -}
import HsDoc (HsDocString(..))
import OccName (HasOccName(..))
import FastString (unpackFS)
#endif

-- Internal
import Language.SK.Lexer

#include "Syntax.h"


-- ---------------------------------------------------------------------
--
-- Constraints for Outputable
--
-- ---------------------------------------------------------------------

#if MIN_VERSION_ghc(8,6,0)
type OUTPUTABLE a pr = (OutputableBndrId a, a ~ GhcPass pr)
#elif MIN_VERSION_ghc(8,4,0)
type OUTPUTABLE a pr = (OutputableBndrId a, SourceTextX a)
#else
type OUTPUTABLE a pr = (OutputableBndrId a, HsSrc a)
#endif


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
  flags <- getSessionDynFlags
  unqual <- getPrintUnqual
  return (showSDocForUser flags unqual (toHsSrc st0 x))


-- ---------------------------------------------------------------------
--
-- Instances
--
-- ---------------------------------------------------------------------

instance HsSrc RdrName where
  toHsSrc _ = ppr

instance (HsSrc b) => HsSrc (GenLocated a b) where
  toHsSrc st (L _ e) = toHsSrc st e

#if MIN_VERSION_ghc(8,4,0)
instance OUTPUTABLE a pr
#else
instance (OUTPUTABLE a pr, HasOccName a)
#endif
         => HsSrc (Hsrc (HsModule a)) where
  toHsSrc st a = case unHsrc a of
    HsModule Nothing _ imports decls _ mbDoc ->
      vcat [ pp_langExts st
           , pp_mbdocn mbDoc
           , pp_nonnull imports
           , hsSrc_nonnull st (map unLoc decls)
           , text "" ]
    HsModule (Just name) exports imports decls deprec mbDoc ->
      vcat [ pp_langExts st
           , pp_mbdocn mbDoc
           , case exports of
               Nothing ->
                 pp_header (text "where")
               Just es ->
                 vcat [ pp_header lparen
                      , nest 8 (pp_lies st (unLoc es))
                      , nest 4 (text ") where")]
           , pp_nonnull imports
           , hsSrc_nonnull st (map unLoc decls)
           , text "" ]
      where
        pp_header rest =
          case deprec of
            Nothing -> pp_modname <+> rest
            Just d  -> vcat [pp_modname, ppr d, rest]
        pp_modname = text "module" <+> ppr name

#if MIN_VERSION_ghc(8,4,0)
instance OUTPUTABLE a pr
#else
instance (OUTPUTABLE a pr, HasOccName a)
#endif
         => HsSrc (Hsrc (IE a)) where
  toHsSrc _st (Hsrc ie) =
    case ie of
      IEGroup _EXT n doc  -> commentWithHeader ("-- " ++ replicate n '*')
                                                doc
      IEDoc _EXT doc      -> commentWithHeader ("-- |") doc
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
      decl'            -> ppr decl'


-- --------------------------------------------------------------------
--
-- Type signature
--
-----------------------------------------------------------------------

instance OUTPUTABLE a pr => HsSrc (Sig a) where
  toHsSrc st sig = case sig of
    TypeSig _EXT vars ty -> pprVarSig (map unLoc vars)
                                      (toHsSrc st ty)
    _ -> ppr sig

instance (OUTPUTABLE a pr, Outputable thing, HsSrc thing)
          => HsSrc (HsWildCardBndrs a thing) where
  toHsSrc st wc = case wc of
    HsWC { hswc_body = ty } -> toHsSrc st ty
#if MIN_VERSION_ghc(8,6,0)
    _ -> ppr wc
#endif

instance (OUTPUTABLE a pr)
         => HsSrc (HsImplicitBndrs a (LHsType a)) where
  toHsSrc st ib =
    case ib of
      HsIB { hsib_body = ty } -> toHsSrc st ty
#if MIN_VERSION_ghc(8,6,0)
      _ -> ppr ib
#endif

instance (OUTPUTABLE a pr) => HsSrc (HsType a) where
  toHsSrc st ty = case ty of
    HsForAllTy {hst_bndrs=tvs, hst_body=ty1} ->
      sep [pprHsForAllTvs tvs, hsrc ty1]
    HsQualTy {hst_ctxt=L _ ctxt, hst_body=ty1} ->
      sep [pprHsContextAlways ctxt, hsrc ty1]
    HsFunTy _EXT ty1 ty2 ->
      sep [hsrc ty1, text "->", hsrc ty2]
    HsDocTy _EXT ty' (L _ docstr) ->
      ppr ty' <+> commentWithHeader "-- ^" docstr
    HsParTy _EXT ty1 -> parens (hsrc ty1)
    _ -> ppr ty
    where
      hsrc :: HsSrc a => a -> SDoc
      hsrc = toHsSrc st

-- Taken from 'HsBinds.pprVarSig'.
pprVarSig :: OutputableBndr id => [id] -> SDoc -> SDoc
pprVarSig vars pp_ty = sep [pprvars <+> dcolon, nest 2 pp_ty]
  where
    pprvars = hsep $ punctuate comma (map pprPrefixOcc vars)

-- Taken from 'HsTypes.pprHsForAllTvs'.
pprHsForAllTvs :: OUTPUTABLE n pr => [LHsTyVarBndr n] -> SDoc
pprHsForAllTvs qtvs
  | null qtvs = forAllLit <+> dot
  | otherwise = forAllLit <+> interppSP qtvs <> dot

-- Taken from 'HsTypes.pprHsContextAlways'.
pprHsContextAlways :: OUTPUTABLE n pr => HsContext n -> SDoc
pprHsContextAlways [] = parens empty <+> darrow
pprHsContextAlways [L _ ty] = ppr ty <+> darrow
pprHsContextAlways cxt = parens (interpp'SP cxt) <+> darrow


-- --------------------------------------------------------------------
--
-- Data type
--
-----------------------------------------------------------------------

instance OUTPUTABLE a pr => HsSrc (TyClDecl a) where
  toHsSrc st tcd =
    case tcd of
      DataDecl { tcdLName = ltycon, tcdTyVars = tyvars
               , tcdFixity = fixity, tcdDataDefn = defn } ->
        pp_data_defn (pp_vanilla_decl_head ltycon tyvars fixity) defn
      SynDecl { tcdLName = ltycon, tcdTyVars = tyvars
              , tcdFixity = fixity, tcdRhs = rhs } ->
        hang (text "type" <+>
              pp_vanilla_decl_head ltycon tyvars fixity [] <+> equals)
           4 (toHsSrc st rhs)
      _ -> ppr tcd

-- From 'HsDecls.pp_data_defn'.
pp_data_defn :: (OUTPUTABLE n pr)
             => (HsContext n -> SDoc)
             -> HsDataDefn n
             -> SDoc
pp_data_defn
  pp_hdr (HsDataDefn { dd_ND = new_or_data, dd_ctxt = L _ context
                     , dd_cType = mb_ct, dd_kindSig = mb_sig
                     , dd_cons = condecls, dd_derivs = derivings })
  | null condecls
  = ppr new_or_data <+> pp_ct <+> pp_hdr context <+> pp_sig
    <+> pp_derivings derivings
  | otherwise
  = hang (ppr new_or_data <+> pp_ct <+> pp_hdr context <+> pp_sig)
       2 (pp_condecls condecls $$ pp_derivings derivings)
  where
    pp_ct = case mb_ct of
              Nothing -> empty
              Just ct -> ppr ct
    pp_sig = case mb_sig of
               Nothing   -> empty
               Just kind -> dcolon <+> ppr kind
    pp_derivings (L _ ds) = vcat (map ppr ds)
#if MIN_VERSION_ghc(8,6,0)
pp_data_defn _ (XHsDataDefn x) = ppr x
#endif

-- Modified version of 'HsDecls.pp_condecls', no space in front of "|".
pp_condecls :: (OUTPUTABLE n pr) => [LConDecl n] -> SDoc
pp_condecls cs@(L _ ConDeclGADT {} : _) =
  hang (text "where") 2 (vcat (map ppr cs))
pp_condecls cs =
  equals <+> sep (punctuate (text "|") (map (pprConDecl . unLoc) cs))

-- Modified version of 'HsDecls.pprConDecl'. This function does the
-- pretty printing of documentation for constructors.
pprConDecl :: OUTPUTABLE n pr => ConDecl n -> SDoc
#if MIN_VERSION_ghc(8,6,0)
pprConDecl (ConDeclH98 { con_name = L _ con
                       , con_ex_tvs = ex_tvs
                       , con_mb_cxt = mcxt
                       , con_args = args
                       , con_doc = doc})
  = sep [pprHsForAll ex_tvs cxt, ppr_details args] $+$
    pp_mbdocp doc $$ text ""
  where
#else
pprConDecl (ConDeclH98 { con_name = L _ con
                       , con_qvars = mtvs
                       , con_cxt = mcxt
                       , con_details = details
                       , con_doc = doc})
  = sep [pprHsForAll tvs cxt, ppr_details details] $+$
    pp_mbdocp doc $$ text ""
  where
    tvs = maybe [] hsq_explicit mtvs
#endif
    ppr_details (InfixCon t1 t2) =
      hsep [ppr t1, pprInfixOcc con, ppr t2]
    ppr_details (PrefixCon tys) =
      hsep (pprPrefixOcc con : map (ppr . unLoc) tys)
    ppr_details (RecCon fields) =
      pprPrefixOcc con <+> pprConDeclFields (unLoc fields)
    cxt = fromMaybe (noLoc []) mcxt
pprConDecl con = ppr con

-- Modified version of 'HsTypes.pprConDeclFields', to emit documentation
-- comments of fields in record data type.
pprConDeclFields :: OUTPUTABLE n pr
                  => [LConDeclField n] -> SDoc
pprConDeclFields fields =
  braces (sep (punctuate comma (map ppr_fld fields)))
  where
    ppr_fld (L _ (ConDeclField { cd_fld_names = ns
                               , cd_fld_type = ty
                               , cd_fld_doc = doc }))
      = ppr_names ns <+> dcolon <+> ppr ty
        $+$ pp_mbdocp doc $+$ text ""
#if MIN_VERSION_ghc (8,6,0)
    ppr_fld (L _ (XConDeclField x)) = ppr x
#endif
    ppr_names [n] = ppr n
    ppr_names ns = sep (punctuate comma (map ppr ns))

-- From 'HsDecls.pp_vanilla_decl_head'.
pp_vanilla_decl_head :: (OUTPUTABLE n pr)
                     => Located (IdP n)
                     -> LHsQTyVars n
                     -> LexicalFixity
                     -> HsContext n
                     -> SDoc
pp_vanilla_decl_head thing (HsQTvs {hsq_explicit=tyvars}) fixity context
  = hsep [pprHsContext context, pp_tyvars tyvars]
  where
    pp_tyvars (varl:varsr)
      | fixity == Infix && length varsr > 1
      = hsep [ char '(', ppr (unLoc varl), pprInfixOcc (unLoc thing)
             , ppr (unLoc (head varsr)), char ')'
             , hsep (map (ppr . unLoc) (tail varsr)) ]
      | fixity == Infix
      = hsep [ ppr (unLoc varl), pprInfixOcc (unLoc thing)
             , hsep (map (ppr . unLoc) varsr) ]
      | otherwise = hsep [ pprPrefixOcc (unLoc thing)
                         , hsep (map (ppr . unLoc) (varl: varsr)) ]
    pp_tyvars [] = pprPrefixOcc (unLoc thing)
#if MIN_VERSION_ghc(8,6,0)
pp_vanilla_decl_head _ (XLHsQTyVars x) _ _ = ppr x
#endif

-- ---------------------------------------------------------------------
--
-- DocDecl
--
-- ---------------------------------------------------------------------

instance HsSrc DocDecl where
  toHsSrc _st d = case d of
    DocCommentNext ds       -> text "" $+$ commentWithHeader "-- |" ds
    DocCommentPrev ds       -> text "" $+$ commentWithHeader "-- ^" ds
                               $+$ text ""
    DocCommentNamed name ds -> namedDoc name ds
    DocGroup n ds           -> let stars = replicate n '*'
                               in  commentWithHeader ("-- " ++ stars) ds
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

pp_mbdocn :: Maybe LHsDocString -> SDoc
pp_mbdocn = maybe empty (commentWithHeader "-- |" . unLoc)

pp_mbdocp :: Maybe LHsDocString -> SDoc
pp_mbdocp = maybe empty (commentWithHeader "-- ^" . unLoc)

pp_langExts :: SPState -> SDoc
pp_langExts sp = vcat (map f (langExts sp))
  where
    f (L _ e) = text "{-# LANGUAGE" <+> text e <+> text "#-}"

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
pp_lies ::
#if MIN_VERSION_ghc (8,4,0)
  OUTPUTABLE a pr
#else
  (OUTPUTABLE a pr, HasOccName a)
#endif
  => SPState -> [LIE a] -> SDoc
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
#if MIN_VERSION_ghc(8,6,0)
unpackHDS' = unpackHDS
#else
unpackHDS' (HsDocString fs) = unpackFS fs
#endif
