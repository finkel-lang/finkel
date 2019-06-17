{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
-- | Syntax for type.
module Language.SK.Syntax.HType where

-- ghc
import BasicTypes (Boxity(..), SourceText(..))
import FastString (headFS, lengthFS, nullFS, tailFS)
import HsDoc (LHsDocString)
import HsTypes ( HsSrcBang(..), HsType(..), HsTupleSort(..)
               , LHsTyVarBndr, Promoted(..), SrcStrictness(..)
               , SrcUnpackedness(..), mkAnonWildCardTy
               , mkHsAppTy, mkHsAppTys, mkHsOpTy )
import Lexeme (isLexCon, isLexConSym)
import OccName (tcName, tvName)
import PrelNames (eqTyCon_RDR)
import RdrName (getRdrName, mkQual, mkUnqual)
import SrcLoc (GenLocated(..), Located, getLoc)
import TysPrim (funTyCon)
import TysWiredIn (listTyCon, tupleTyCon)

#if MIN_VERSION_ghc(8,6,0)
import BasicTypes (PprPrec, funPrec)
import HsExtension (IdP, noExt)
import qualified HsTypes (parenthesizeHsType)
#elif MIN_VERSION_ghc(8,4,0)
import HsExtension (IdP)
import TysWiredIn (starKindTyCon)
#else
#define IdP {- empty -}
import TysWiredIn (starKindTyCon)
#endif

-- Internal
import Language.SK.Builder
import Language.SK.Form
import Language.SK.Syntax.SynUtils

#include "Syntax.h"


-- ---------------------------------------------------------------------
--
-- Types
--
-- ---------------------------------------------------------------------

b_anonWildT :: Code -> HType
b_anonWildT (LForm (L l _)) = L l mkAnonWildCardTy
{-# INLINE b_anonWildT #-}

b_symT :: Code -> Builder HType
b_symT whole@(LForm (L l form))
  | Atom (ASymbol name) <- form =
    let ty =
          case splitQualName name of
            Nothing
              | ',' == x  -> tv (getRdrName (tupleTyCon Boxed arity))
              | '!' == x  ->
                bang (tv (mkUnqual (namespace xs) xs))
              -- XXX: Handle "StarIsType" language extension. Name of
              -- the type kind could be obtained from
              -- "TysWiredIn.liftedTypeKindTyCon".
              | '*' == x && nullFS xs ->
#if MIN_VERSION_ghc(8,6,0)
                L l (HsStarTy NOEXT False)
#else
                tv (getRdrName starKindTyCon)
#endif
              | otherwise -> tv (mkUnqual (namespace name) name)
            Just qual -> tv (mkQual (namespace name) qual)
        namespace ns
          | isLexCon ns = tcName
          | otherwise   = tvName
        x = headFS name
        xs = tailFS name
        arity = 1 + lengthFS name
        tv t = L l (hsTyVar NotPromoted (L l t))
        bang = b_bangT whole
    in  return ty
  | otherwise = builderError
{-# INLINE b_symT #-}

b_unitT :: Code -> HType
b_unitT (LForm (L l _)) = L l (hsTupleTy HsBoxedTuple [])
{-# INLINE b_unitT #-}

b_tildeT :: Code -> HType
b_tildeT (LForm (L l _)) = L l (hsTyVar NotPromoted (L l eqTyCon_RDR))
{-# INLINE b_tildeT #-}

b_funT :: Code -> [HType] -> Builder HType
b_funT (LForm (L l _)) ts =
  -- For single argument, making HsAppTy with '(->)' instead of HsFunTy.
  case ts of
    []           -> return funty
#if MIN_VERSION_ghc(8,4,0)
    [t]          -> return (mkHsAppTy funty t)
#else
    [t@(L l0 _)] -> return (L l0 (hsParTy (mkHsAppTy funty t)))
#endif
    _            -> return (foldr1 f ts)
  where
    f a@(L l1 _) b = L l1 (hsFunTy (parenthesizeHsType funPrec a) b)
    hsFunTy = HsFunTy NOEXT
    funty = L l (hsTyVar NotPromoted (L l (getRdrName funTyCon)))
{-# INLINE b_funT #-}

b_opOrAppT :: Code -> [HType] -> Builder HType
b_opOrAppT form@(LForm (L l ty)) typs
  | null typs = b_symT form
  | Atom (ASymbol name) <- ty
  , isLexConSym name =
    let lrname = L l (mkUnqual tcName name)
        f lhs rhs = L l (mkHsOpTy lhs lrname rhs)
    in  return (L l (hsParTy (foldr1 f typs)))
  | otherwise =
    do op <- b_symT form
       b_appT (op:typs)
{-# INLINE b_opOrAppT #-}

b_appT :: [HType] -> Builder HType
b_appT []           = builderError
b_appT whole@(x:xs) =
  case xs of
    [] -> return x
    _  -> return (L l0 (hsParTy (mkHsAppTys x xs)))
  where
    l0 = getLoc (mkLocatedList whole)
{-# INLINE b_appT #-}

b_listT :: HType -> HType
b_listT ty@(L l _) = L l (HsListTy NOEXT ty)
{-# INLINE b_listT #-}

b_nilT :: Code -> HType
b_nilT (LForm (L l _)) =
  L l (hsTyVar NotPromoted (L l (getRdrName listTyCon)))
{-# INLINE b_nilT #-}

b_tupT :: Code -> [HType] -> HType
b_tupT (LForm (L l _)) ts =
  case ts of
   [] -> L l (hsTyVar NotPromoted (L l tup))
     where
       tup = getRdrName (tupleTyCon Boxed 2)
   _  -> L l (hsTupleTy HsBoxedTuple ts)
{-# INLINE b_tupT #-}

b_bangT :: Code -> HType -> HType
b_bangT (LForm (L l _)) t = L l (hsBangTy srcBang t)
  where
    srcBang = HsSrcBang (SourceText "b_bangT") NoSrcUnpack SrcStrict
{-# INLINE b_bangT #-}

b_forallT :: Code -> ([HTyVarBndr], ([HType], HType)) -> HType
b_forallT (LForm (L l0 _)) (bndrs, (ctxts, body)) =
  let ty0 = L l0 (mkHsQualTy_compat (mkLocatedList ctxts) body)
      ty1 = hsParTy (L l0 (forAllTy bndrs ty0))
  in  L l0 ty1
{-# INLINE b_forallT #-}

b_kindedType :: Code -> HType -> HType -> HType
b_kindedType (LForm (L l _)) ty kind = L l (HsKindSig NOEXT ty kind)
{-# INLINE b_kindedType #-}

b_docT :: HType -> LHsDocString -> HType
b_docT ty doc = let l = getLoc ty in L l (HsDocTy NOEXT ty doc)
{-# INLINE b_docT #-}

b_unpackT :: Code -> HType -> HType
b_unpackT (LForm (L l _)) t = L l (hsBangTy bang t')
  where
    bang = HsSrcBang (SourceText "b_unpackT") SrcUnpack strictness
    (strictness, t') =
      case t of
        L _ (HsBangTy _EXT (HsSrcBang _ _ st) t0) -> (st, t0)
        _                                         -> (NoSrcStrict, t)
{-# INLINE b_unpackT #-}

hsTupleTy :: HsTupleSort -> [HType] -> HsType PARSED
hsTupleTy = HsTupleTy NOEXT
{-# INLINE hsTupleTy #-}

hsBangTy :: HsSrcBang -> HType -> HsType PARSED
hsBangTy = HsBangTy NOEXT
{-# INLINE hsBangTy #-}

forAllTy :: [LHsTyVarBndr PARSED] -> HType -> HsType PARSED
forAllTy bndrs body =
  HsForAllTy { hst_bndrs = bndrs
#if MIN_VERSION_ghc(8,6,0)
             , hst_xforall = noExt
#endif
             , hst_body = body }
{-# INLINE forAllTy #-}

hsParTy :: HType -> HsType PARSED
hsParTy = HsParTy NOEXT
{-# INLINE hsParTy #-}

hsTyVar :: Promoted -> Located (IdP PARSED) -> HsType PARSED
hsTyVar = HsTyVar NOEXT
{-# INLINE hsTyVar #-}


-- ---------------------------------------------------------------------
--
-- Parenthesizing
--
-- ---------------------------------------------------------------------

#if MIN_VERSION_ghc(8,6,0)

-- Unlike "parenthesizeHsType" defined in "HsTypes" module in ghc 8.6.x,
-- does not parenthesize "HsBangTy" constructor, because
-- "parenthesizeHsType" is used for parenthesizing argument in HsFunTy.

parenthesizeHsType :: PprPrec -> HType -> HType
parenthesizeHsType p lty@(L _ ty)
  | HsBangTy {} <- ty = lty
  | otherwise         = HsTypes.parenthesizeHsType p lty

#else

-- Ppr precedence, arranged and back ported from ghc 8.6.x.
--
-- "PprPrec" and xxxPrec are defined in "basicTypes/BasicTypes.hs",
-- "hsTypeNeedsParens" and "parenthesizeHsType" are defined in
-- "hsSyn/HsTypes.hs".

parenthesizeHsType :: PprPrec -> HType -> HType
parenthesizeHsType p lty@(L loc ty)
  | hsTypeNeedsParens p ty = L loc (HsParTy NOEXT lty)
  | otherwise              = lty

newtype PprPrec = PprPrec Int deriving (Eq, Ord, Show)

topPrec, sigPrec, funPrec, opPrec, appPrec :: PprPrec
topPrec = PprPrec 0
sigPrec = PprPrec 1
funPrec = PprPrec 2
opPrec  = PprPrec 2
appPrec = PprPrec 3

hsTypeNeedsParens :: PprPrec -> HsType pass -> Bool
hsTypeNeedsParens p = go
  where
    go (HsForAllTy{})        = p >= funPrec
    go (HsQualTy{})          = p >= funPrec
    -- No parenthesis for Bang types
    -- go (HsBangTy{})          = p > topPrec
    go (HsRecTy{})           = False
    go (HsTyVar{})           = False
    go (HsFunTy{})           = p >= funPrec
    go (HsTupleTy{})         = False
    go (HsSumTy{})           = False
    go (HsKindSig{})         = False
    go (HsListTy{})          = False
    go (HsIParamTy{})        = p > topPrec
    go (HsSpliceTy{})        = False
    go (HsExplicitListTy{})  = False
    go (HsExplicitTupleTy{}) = False
    go (HsTyLit{})           = False
    go (HsWildCardTy{})      = False
    go (HsAppTy{})           = p >= appPrec
    go (HsOpTy{})            = p >= opPrec
    go (HsParTy{})           = False
    go (HsDocTy (L _ t) _)   = go t
    go _                     = False
#endif
