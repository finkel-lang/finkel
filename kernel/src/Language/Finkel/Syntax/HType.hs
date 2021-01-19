{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
-- | Syntax for type.
module Language.Finkel.Syntax.HType where

#include "Syntax.h"


-- base
import Data.List                       (foldl')

-- ghc
import BasicTypes                      (Boxity (..), SourceText (..))
import FastString                      (headFS, lengthFS, nullFS, tailFS)
import GHC_Hs_Doc                      (LHsDocString)
import GHC_Hs_Types                    (HsSrcBang (..), HsTupleSort (..),
                                        HsTyLit (..), HsType (..), LHsTyVarBndr,
                                        SrcStrictness (..),
                                        SrcUnpackedness (..), mkAnonWildCardTy,
                                        mkHsAppTy, mkHsOpTy)
import Lexeme                          (isLexCon, isLexConSym, isLexVarSym)
import OccName                         (dataName, tcName, tvName)
import RdrName                         (getRdrName, mkQual, mkUnqual)
import SrcLoc                          (GenLocated (..), Located, addCLoc,
                                        getLoc)
import TysPrim                         (funTyCon)
import TysWiredIn                      (consDataCon, listTyCon_RDR, tupleTyCon)

#if MIN_VERSION_ghc(8,10,0)
import Var                             (ForallVisFlag (..))
#endif

#if MIN_VERSION_ghc(8,8,0)
import BasicTypes                      (PromotionFlag (..))
#else
import GHC_Hs_Types                    (Promoted (..))
#endif

#if MIN_VERSION_ghc(8,8,0)
import TysWiredIn                      (eqTyCon_RDR)
#else
import PrelNames                       (eqTyCon_RDR)
#endif

#if MIN_VERSION_ghc(8,6,0)
import GHC_Hs_Types                    (parenthesizeHsType)
#endif

#if MIN_VERSION_ghc(8,10,0)
import GHC_Hs_Extension                (noExtField)
#elif MIN_VERSION_ghc(8,6,0)
import GHC_Hs_Extension                (noExt)
#else
import PlaceHolder                     (placeHolderKind)
#endif

#if !MIN_VERSION_ghc(8,6,0)
import TysWiredIn                      (starKindTyCon)
#endif

#if MIN_VERSION_ghc(8,4,0)
import GHC_Hs_Extension                (IdP)
#else
#define IdP {- empty -}
#endif

-- Internal
import Language.Finkel.Builder
import Language.Finkel.Form
import Language.Finkel.Syntax.SynUtils

-- ---------------------------------------------------------------------
--
-- Promotion
--
-- ---------------------------------------------------------------------

#if MIN_VERSION_ghc(8,8,0)
type PROMOTIONFLAG = PromotionFlag

iSPROMOTED :: PROMOTIONFLAG
iSPROMOTED = IsPromoted
#else
type PROMOTIONFLAG = Promoted

iSPROMOTED :: PROMOTIONFLAG
iSPROMOTED = Promoted
#endif
{-# INLINE iSPROMOTED #-}

nOTPROMOTED :: PROMOTIONFLAG
nOTPROMOTED = NotPromoted
{-# INLINE nOTPROMOTED #-}

unPromoteTyVar :: HType -> HType
unPromoteTyVar ty =
  case ty of
    (dL->L l (HsTyVar _EXT _ (L ln name))) ->
      cL l (hsTyVar nOTPROMOTED (L ln name))
    _ -> ty
{-# INLINE unPromoteTyVar #-}


-- ---------------------------------------------------------------------
--
-- Types
--
-- ---------------------------------------------------------------------

b_anonWildT :: Code -> HType
b_anonWildT (LForm (L l _)) = L l mkAnonWildCardTy
{-# INLINE b_anonWildT #-}

b_symT :: Code -> Builder HType
b_symT whole@(LForm (L l form)) =
  case form of
    Atom (ASymbol name) -> return $! ty name
    _                   -> builderError
  where
    ty name =
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
      where
        x = headFS name
        xs = tailFS name
        arity = 1 + lengthFS name
    namespace ns
      -- Using "isLexVarSym" for "TypeOperator" extension.
      | isLexCon ns || isLexVarSym ns = tcName
      | otherwise                     = tvName
    tv t = L l (hsTyVar NotPromoted (L l t))
    bang = b_bangT whole
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
    f a b = addCLoc a b (hsFunTy (parenthesizeHsType' funPrec a) b)
    hsFunTy = HsFunTy NOEXT
    funty = L l (hsTyVar NotPromoted (L l (getRdrName funTyCon)))
{-# INLINE b_funT #-}

b_tyLitT :: Code -> Builder HType
b_tyLitT (LForm (L l form))
  | Atom (AString _ str) <- form =
    return (mkLit l (HsStrTy (SourceText (show str)) str))
  | Atom (AInteger (IL {il_value=n})) <- form =
    return (mkLit l (HsNumTy (SourceText (show n)) n))
  | otherwise = builderError
  where
    mkLit loc lit = cL loc (HsTyLit NOEXT lit)
{-# INLINE b_tyLitT #-}

b_opOrAppT :: Code -> [HType] -> Builder HType
b_opOrAppT form@(LForm (L l ty)) typs
  -- Perhaps empty list
  | null typs = b_symT form
  -- Constructor application (not promoted)
  | Atom (ASymbol name) <- ty
  , isLexConSym name =
    let lrname = L l (mkUnqual tcName name)
        f lhs rhs = L l (mkHsOpTy lhs lrname rhs)
    in  return (foldr1 f (map (parenthesizeHsType' opPrec) typs))
  -- Var type application
  | otherwise =
    do op <- b_symT form
       b_appT (op:typs)
{-# INLINE b_opOrAppT #-}

b_prmConT :: Code -> Builder HType
b_prmConT (LForm (L l form)) =
  case form of
    Atom (ASymbol name) -> return $! ty name
    _                   -> builderError
  where
    ty name = L l (hsTyVar iSPROMOTED (cL l (rname name)))
    rname name =
      case name of
       ":" -> getRdrName consDataCon
       _   -> maybe (mkUnqual (namespace name) name)
                    (mkQual tcName)
                    (splitQualName name)
    namespace n
      | isLexCon n    = dataName
      | isLexVarSym n = tcName
      | otherwise     = tvName
{-# INLINE b_prmConT #-}

b_appT :: [HType] -> Builder HType
b_appT []     = builderError
b_appT (x:xs) =
  case xs of
    [] -> return x
    _  -> let f t1 t2 = addCLoc t1 t2 (HsAppTy NOEXT t1 (parTyApp t2))
          in  return (foldl' f x xs)
{-# INLINE b_appT #-}

b_listT :: HType -> HType
b_listT ty@(L l _) = L l (HsListTy NOEXT ty)
{-# INLINE b_listT #-}

b_nilT :: Code -> HType
b_nilT (LForm (L l _)) =
  L l (hsTyVar NotPromoted (L l (listTyCon_RDR)))
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
b_bangT (LForm (L l _)) t = L l (hsBangTy srcBang (parTyApp t))
  where
    srcBang = HsSrcBang (SourceText "b_bangT") NoSrcUnpack SrcStrict
{-# INLINE b_bangT #-}

b_forallT :: Code -> ([HTyVarBndr], ([HType], HType)) -> HType
b_forallT (LForm (L l0 _)) (bndrs, (ctxts, body)) =
  let ty0 = cL l0 (mkHsQualTy_compat (mkLocatedList ctxts) body)
#if MIN_VERSION_ghc(8,4,0)
      ty1 = hsParTy (cL l0 (forAllTy bndrs ty0))
#else
      ty1 = forAllTy bndrs ty0
#endif
  in  cL l0 ty1
{-# INLINE b_forallT #-}

b_qualT :: Code -> ([HType], HType) -> HType
b_qualT (LForm (L l _)) (ctxts, body) =
  cL l (mkHsQualTy_compat (mkLocatedList ctxts) body)
{-# INLINE b_qualT #-}

b_kindedType :: Code -> HType -> HType -> HType
b_kindedType (LForm (L l _)) ty kind =
#if MIN_VERSION_ghc(8,8,0)
   -- Parens for kind signature were removed in ghc 8.8.1. To show
   -- parens in generated Haskell code, explicitly adding at this point.
   L l (hsParTy (L l (HsKindSig NOEXT ty kind)))
#else
   L l (HsKindSig NOEXT ty kind)
#endif
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

b_prmListT :: ([Code] -> Builder [HType]) -> Code -> Builder HType
b_prmListT prsr typs =
  case typs of
    LForm (L l (HsList xs))
      | null xs   -> return (cL l (hsExplicitListTy []))
      | otherwise -> do
          tys <- prsr xs
          return $! cL l (hsExplicitListTy tys)
    _ -> builderError
{-# INLINE b_prmListT #-}

b_prmTupT :: ([Code] -> Builder [HType]) -> [Code] -> Builder HType
b_prmTupT prsr typs =
  case typs of
    hd:tl
      | isCommaSymbol hd -> do
        tys <- prsr tl
        let tys' = map unPromoteTyVar tys
            l = getLoc (mkLocatedList (map unLForm typs))
        return (cL l (hsExplicitTupleTy tys'))
    _ -> builderError
{-# INLINE b_prmTupT #-}

isCommaSymbol :: Code -> Bool
isCommaSymbol (LForm (L _ form)) =
  case form of
    Atom (ASymbol ",") -> True
    _                  -> False
{-# INLINE isCommaSymbol #-}

hsTupleTy :: HsTupleSort -> [HType] -> HsType PARSED
hsTupleTy = HsTupleTy NOEXT
{-# INLINE hsTupleTy #-}

hsBangTy :: HsSrcBang -> HType -> HsType PARSED
hsBangTy = HsBangTy NOEXT
{-# INLINE hsBangTy #-}

forAllTy :: [LHsTyVarBndr PARSED] -> HType -> HsType PARSED
forAllTy bndrs body =
  HsForAllTy { hst_bndrs = bndrs
#if MIN_VERSION_ghc(8,10,0)
             , hst_fvf = ForallInvis
#endif
#if MIN_VERSION_ghc(8,6,0)
             , hst_xforall = NOEXT
#endif
             , hst_body = body }
{-# INLINE forAllTy #-}

hsParTy :: HType -> HsType PARSED
hsParTy = HsParTy NOEXT
{-# INLINE hsParTy #-}

hsTyVar :: PROMOTIONFLAG -> Located (IdP PARSED) -> HsType PARSED
hsTyVar = HsTyVar NOEXT
{-# INLINE hsTyVar #-}

hsExplicitListTy :: [HType] -> HsType PARSED
hsExplicitListTy tys =
#if MIN_VERSION_ghc(8,6,0)
  HsExplicitListTy NOEXT iSPROMOTED tys
#else
  HsExplicitListTy iSPROMOTED placeHolderKind tys
#endif

hsExplicitTupleTy :: [HType] -> HsType PARSED
hsExplicitTupleTy tys =
#if MIN_VERSION_ghc(8,6,0)
  HsExplicitTupleTy NOEXT tys
#else
  HsExplicitTupleTy [] tys
#endif

-- ---------------------------------------------------------------------
--
-- Parenthesizing
--
-- ---------------------------------------------------------------------

-- Unlike "HsTypes.parenthesizeHsType" in ghc 8.6.x, does not
-- parenthesize "HsBangTy" constructor, because
-- "HsTypes.parenthesizeHsType" is used for parenthesizing argument in
-- HsFunTy.

-- | Parenthesize given 'HType' with 'appPrec'.
parTyApp :: HType -> HType
parTyApp = parenthesizeHsType' appPrec
{-# INLINE parTyApp #-}

#if MIN_VERSION_ghc(8,6,0)
parenthesizeHsType' :: PprPrec -> HType -> HType
parenthesizeHsType' p lty@(L _ ty)
  | HsBangTy {} <- ty = lty
  | otherwise         = parenthesizeHsType p lty

#else

-- Ppr precedence, arranged and back ported from ghc 8.6.x.
--
-- "PprPrec" and xxxPrec are defined in "basicTypes/BasicTypes.hs",
-- "hsTypeNeedsParens" and "parenthesizeHsType" are defined in
-- "hsSyn/HsTypes.hs".

parenthesizeHsType' :: PprPrec -> HType -> HType
parenthesizeHsType' p lty@(L loc ty)
  | hsTypeNeedsParens p ty = L loc (HsParTy NOEXT lty)
  | otherwise              = lty

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
