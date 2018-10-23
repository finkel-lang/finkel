{-# LANGUAGE CPP #-}
-- | Syntax for type.
module Language.SK.Syntax.HType where

-- base
import Data.Char (isUpper)

-- ghc
import BasicTypes (Boxity(..), SourceText(..))
import FastString (headFS, lengthFS)
import HsTypes (HsSrcBang(..), HsType(..), HsTupleSort(..), LHsTyVarBndr, Promoted(..), SrcStrictness(..), SrcUnpackedness(..), mkHsAppTys)
import OccName (tcName, tvName)
import RdrName (getRdrName, mkQual, mkUnqual)
import SrcLoc (GenLocated(..), Located)
import TysWiredIn (listTyCon, tupleTyCon)


#if MIN_VERSION_ghc(8,6,0)
import HsExtension (IdP, noExt)
#elif MIN_VERSION_ghc(8,4,0)
import HsExtension (IdP)
#else
#define IdP {- empty -}
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
b_symT _ = error "b_symT"
{-# INLINE b_symT #-}

b_unitT :: Code -> HType
b_unitT (LForm (L l _)) = L l (mkHsTupleTy_compat HsBoxedTuple [])
{-# INLINE b_unitT #-}

b_funT :: [HType] -> Builder HType
b_funT ts =
  case ts of
    []          -> builderError
    (L l0 _):_  -> return (L l0 (mkHsParTy_compat (foldr1 f ts)))
  where
    f a@(L l1 _) b = L l1 (hsFunTy a b)
    hsFunTy = HsFunTy NOEXT
{-# INLINE b_funT #-}

b_appT :: [HType] -> HType
b_appT []           = error "b_appT"
b_appT whole@(x:xs) =
  case xs of
    [] -> x
    _  -> L l0 (mkHsParTy_compat (mkHsAppTys x xs))
  where
    l0 = getLoc (mkLocatedList whole)
{-# INLINE b_appT #-}

b_listT :: HType -> HType
b_listT ty@(L l _) = L l lty
  where
    lty = HsListTy NOEXT ty
{-# INLINE b_listT #-}

b_nilT :: Code -> HType
b_nilT (LForm (L l _)) =
  L l (mkHsTyVar_compat NotPromoted (L l (getRdrName listTyCon)))
{-# INLINE b_nilT #-}

b_tupT :: Code -> [HType] -> HType
b_tupT (LForm (L l _)) ts =
  case ts of
   [] -> L l (mkHsTyVar_compat NotPromoted (L l tup))
     where
       tup = getRdrName (tupleTyCon Boxed 2)
   _  -> L l (mkHsTupleTy_compat HsBoxedTuple ts)
{-# INLINE b_tupT #-}

b_bangT :: Code -> HType -> HType
b_bangT (LForm (L l _)) t = L l (mkHsBangTy srcBang t)
  where
    srcBang = HsSrcBang (SourceText "b_bangT") NoSrcUnpack SrcStrict
{-# INLINE b_bangT #-}

b_forallT :: (Code, [Code]) -> ([HType], HType) -> HType
b_forallT ((LForm (L l0 _)), vars) (ctxts, body) = L l0 forAllTy
  where
    forAllTy = mkHsParTy_compat (L l0 forAllTy')
    forAllTy' = mkForAllTy_compat bndrs ty
    ty = L l0 (mkHsQualTy_compat (mkLocatedList ctxts) body)
    bndrs = map codeToUserTyVar vars
{-# INLINE b_forallT #-}

b_unpackT :: Code -> HType -> HType
b_unpackT (LForm (L l _)) t = L l (mkHsBangTy bang t')
  where
    bang = HsSrcBang (SourceText "b_unpackT") SrcUnpack strictness
    (strictness, t') =
      case t of
#if MIN_VERSION_ghc(8,6,0)
        L _ (HsBangTy _ (HsSrcBang _ _ st) t0) -> (st, t0)
#else
        L _ (HsBangTy   (HsSrcBang _ _ st) t0) -> (st, t0)
#endif
        _                                      -> (NoSrcStrict, t)
{-# INLINE b_unpackT #-}

mkHsTupleTy_compat :: HsTupleSort -> [HType] -> HsType PARSED
mkHsTupleTy_compat = HsTupleTy NOEXT
{-# INLINE mkHsTupleTy_compat #-}

mkHsBangTy :: HsSrcBang -> HType -> HsType PARSED
mkHsBangTy = HsBangTy NOEXT
{-# INLINE mkHsBangTy #-}

mkForAllTy_compat :: [LHsTyVarBndr PARSED] -> HType -> HsType PARSED
mkForAllTy_compat bndrs body =
  HsForAllTy { hst_bndrs = bndrs
#if MIN_VERSION_ghc(8,6,0)
             , hst_xforall = noExt
#endif
             , hst_body = body }
{-# INLINE mkForAllTy_compat #-}

mkHsParTy_compat :: HType -> HsType PARSED
mkHsParTy_compat = HsParTy NOEXT
{-# INLINE mkHsParTy_compat #-}

-- #if MIN_VERSION_ghc(8,4,0)
-- mkHsTyVar_compat :: Promoted -> Located (IdP PARSED) -> HsType PARSED
-- #else
-- mkHsTyVar_compat :: Promoted -> Located name -> HsType name
-- #endif

mkHsTyVar_compat :: Promoted -> Located (IdP PARSED) -> HsType PARSED
mkHsTyVar_compat = HsTyVar NOEXT
{-# INLINE mkHsTyVar_compat #-}
