{-# LANGUAGE CPP #-}
-- | Syntax for type.
module Language.SK.Syntax.HType where

-- base
import Data.Char (isUpper)

-- ghc
import BasicTypes (Boxity(..), SourceText(..))
import FastString (headFS, lengthFS, tailFS)
import HsTypes ( HsSrcBang(..), HsType(..), HsTupleSort(..)
               , LHsTyVarBndr, Promoted(..), SrcStrictness(..)
               , SrcUnpackedness(..), mkHsAppTys )
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

b_symT :: Code -> Builder HType
b_symT whole@(LForm (L l form))
  | Atom (ASymbol name) <- form =
    let (ty, bang) =
           case splitQualName name of
             Nothing
               | ',' == x  ->
                 (getRdrName (tupleTyCon Boxed arity), False)
               | '!' == x  ->
                 (mkUnqual (namespace (headFS xs)) xs, True)
               | otherwise ->
                 (mkUnqual (namespace x) name, False)
             Just qual -> (mkQual (namespace x) qual, False)
        namespace chr
          | isUpper chr || ':' == chr = tcName
          | otherwise                 = tvName
        x = headFS name
        xs = tailFS name
        arity = 1 + lengthFS name
        tyvar = hsTyVar NotPromoted (L l ty)
        hty   = L l tyvar
        hty' | bang      = b_bangT whole hty
             | otherwise = hty
    in  return hty'
  | otherwise = builderError
{-# INLINE b_symT #-}

b_unitT :: Code -> HType
b_unitT (LForm (L l _)) = L l (hsTupleTy HsBoxedTuple [])
{-# INLINE b_unitT #-}

b_funT :: [HType] -> Builder HType
b_funT ts =
  case ts of
    []          -> builderError
    (L l0 _):_  -> return (L l0 (hsParTy (foldr1 f ts)))
  where
    f a@(L l1 _) b = L l1 (hsFunTy a b)
    hsFunTy = HsFunTy NOEXT
{-# INLINE b_funT #-}

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

b_forallT :: (Code, [Code]) -> ([HType], HType) -> HType
b_forallT ((LForm (L l0 _)), vars) (ctxts, body) = L l0 fat
  where
    fat = hsParTy (L l0 forAllTy')
    forAllTy' = forAllTy bndrs ty
    ty = L l0 (mkHsQualTy_compat (mkLocatedList ctxts) body)
    bndrs = map codeToUserTyVar vars
{-# INLINE b_forallT #-}

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
