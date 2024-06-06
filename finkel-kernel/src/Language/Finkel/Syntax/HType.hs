{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

-- | Syntax for type.
module Language.Finkel.Syntax.HType where

#include "Syntax.h"
#include "ghc_modules.h"

-- base
import Data.List                       (foldl')

-- ghc
import GHC_Builtin_Types               (consDataCon, listTyCon_RDR, tupleTyCon)
import GHC_Hs_Doc                      (LHsDocString)
import GHC_Hs_Type                     (HsSrcBang (..), HsTupleSort (..),
                                        HsTyLit (..), HsType (..),
                                        SrcStrictness (..),
                                        SrcUnpackedness (..), mkAnonWildCardTy,
                                        mkHsAppTy, mkHsOpTy)
import GHC_Types_Basic                 (Boxity (..))
import GHC_Types_Name_Occurrence       (dataName, tcName, tvName)
import GHC_Types_Name_Reader           (getRdrName, mkQual, mkUnqual)
import GHC_Types_SrcLoc                (GenLocated (..), getLoc)
import GHC_Utils_Lexeme                (isLexCon, isLexConSym, isLexVarSym)

#if MIN_VERSION_ghc(9,4,0)
import GHC.Hs.Extension                (noHsUniTok)
import GHC.Parser.Annotation           (NoEpAnns (..))
#elif MIN_VERSION_ghc(9,2,0)
import GHC_Parser_Annotation           (EpaLocation (..), TrailingAnn (..))
#endif

#if !MIN_VERSION_ghc(9,4,0) && MIN_VERSION_ghc(9,0,0)
import GHC_Parser_Annotation           (IsUnicodeSyntax (..))
#endif

#if MIN_VERSION_ghc(9,2,0)
import GHC_Parser_Annotation           (Anchor (..), AnchorOperation (..),
                                        EpAnn (..))
import GHC_Types_SrcLoc                (srcSpanToRealSrcSpan)
#endif

#if MIN_VERSION_ghc(9,0,0)
import GHC_Builtin_Types               (unrestrictedFunTyCon)
#else
import GHC_Builtin_Types_Prim          (funTyCon)
#endif

#if MIN_VERSION_ghc(9,0,0)
import GHC_Hs_Type                     (HsArrow (..), mkHsForAllInvisTele)
#elif MIN_VERSION_ghc(8,10,0)
import GHC_Types_Var                   (ForallVisFlag (..))
#endif

#if MIN_VERSION_ghc(8,8,0)
import GHC_Builtin_Types               (eqTyCon_RDR)
import GHC_Types_Basic                 (PromotionFlag (..))
#else
import GHC_Hs_Type                     (Promoted (..))
import PrelNames                       (eqTyCon_RDR)
#endif

#if MIN_VERSION_ghc(8,6,0)
import GHC_Hs_Type                     (parenthesizeHsType)
#else
import PlaceHolder                     (placeHolderKind)
import TysWiredIn                      (starKindTyCon)
#endif

-- Internal
import Language.Finkel.Builder
import Language.Finkel.Data.FastString (lengthFS, nullFS, unconsFS)
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
{-# INLINABLE iSPROMOTED #-}

nOTPROMOTED :: PROMOTIONFLAG
nOTPROMOTED = NotPromoted
{-# INLINABLE nOTPROMOTED #-}

unPromoteTyVar :: HType -> HType
unPromoteTyVar ty =
  case ty of
    (dL->L l (HsTyVar _EXT _ (L ln name))) ->
      L l (hsTyVar nOTPROMOTED (L ln name))
    _ -> ty
{-# INLINABLE unPromoteTyVar #-}


-- ---------------------------------------------------------------------
--
-- Types
--
-- ---------------------------------------------------------------------

b_anonWildT :: Code -> HType
b_anonWildT (LForm (L l _)) = lA l mkAnonWildCardTy
{-# INLINABLE b_anonWildT #-}

b_symT :: Code -> Builder HType
b_symT whole@(LForm (L l form)) =
  case form of
    Atom (ASymbol name) -> return $! ty name
    _                   -> builderError
  where
    ty name =
      case splitQualName name of
        Just qual -> tv (mkQual (namespace name) qual)
        Nothing -> case unconsFS name of
          -- XXX: Handle "StarIsType" language extension. Name of the type kind
          -- could be obtained from "TysWiredIn.liftedTypeKindTyCon".
          Just (x, xs)
            | ',' == x -> tv (getRdrName (tupleTyCon Boxed arity))
            | '!' == x -> bang (tv (mkUnqual (namespace xs) xs))
            | '*' == x, nullFS xs ->
#if MIN_VERSION_ghc(8,6,0)
               lA l (HsStarTy NOEXT False)
#else
               tv (getRdrName starKindTyCon)
#endif
          _ -> tv (mkUnqual (namespace name) name)
      where
        arity = 1 + lengthFS name
    namespace ns
      -- Using "isLexVarSym" for "TypeOperator" extension.
      | isLexCon ns || isLexVarSym ns = tcName
      | otherwise                     = tvName
    tv rname = lA l (hsTyVar NotPromoted (lN l rname))
    bang = b_bangT whole
{-# INLINABLE b_symT #-}

b_unitT :: Code -> HType
b_unitT (LForm (L l _)) = lA l (hsTupleTy hsBoxedTuple [])
{-# INLINABLE b_unitT #-}

b_tildeT :: Code -> HType
b_tildeT (LForm (L l _)) = lA l (hsTyVar NotPromoted (lN l eqTyCon_RDR))
{-# INLINABLE b_tildeT #-}

b_funT :: Code -> [HType] -> Builder HType
b_funT (LForm (L l _)) ts =
  -- For single argument, making HsAppTy with '(->)' instead of HsFunTy.
  case ts of
    []           -> return funty
#if MIN_VERSION_ghc(8,4,0)
    [t]          -> return (mkHsAppTy funty t)
#else
    [t@(L l0 _)] -> return (lA l0 (hsParTy (mkHsAppTy funty t)))
#endif
    _            -> return (foldr1 f ts)
  where
    f a b = addCLocAA a b (hsFunTy (parenthesizeHsType' funPrec a) b)
    -- XXX: Does not support linear type and unicode syntax.
#if MIN_VERSION_ghc(9,2,0)
    -- XXX: As of ghc 9.2.1, the 'GHC.Hs.Type.splitHsFunType' function in the
    -- ghc package is ignoring "EpAnnNotUsed" constructor in the pattern match
    -- during recursion. Using "EpAnn" to make a dummy EpAnn typed value with
    -- "mkDummyAnn". Without the dummy value, GADT constructors will show
    -- compilation errors.
#  if MIN_VERSION_ghc(9,4,0)
    hsFunTy = HsFunTy ann (HsUnrestrictedArrow noHsUniTok)
#  else
    hsFunTy = HsFunTy ann (HsUnrestrictedArrow NormalSyntax)
#  endif
    ann = maybe NOEXT mkDummyAnn (srcSpanToRealSrcSpan l)
    mkDummyAnn real_span =
      let dummy_anchor = Anchor real_span UnchangedAnchor
#  if MIN_VERSION_ghc(9,4,0)
          dummy_anns = NoEpAnns
#  else
          dummy_anns = AddRarrowAnn (EpaSpan real_span)
#  endif
          dummy_comments = NOEXT
      in  EpAnn dummy_anchor dummy_anns dummy_comments
#elif MIN_VERSION_ghc(9,0,0)
    hsFunTy = HsFunTy NOEXT (HsUnrestrictedArrow NormalSyntax)
#else
    hsFunTy = HsFunTy NOEXT
#endif
#if MIN_VERSION_ghc(9,0,0)
    funty = lA l (hsTyVar NotPromoted (lN l (getRdrName unrestrictedFunTyCon)))
#else
    funty = lA l (hsTyVar NotPromoted (lN l (getRdrName funTyCon)))
#endif
{-# INLINABLE b_funT #-}

b_tyLitT :: Code -> Builder HType
b_tyLitT (LForm (L l form))
  | Atom (AString stxt str) <- form =
    return (mkLit l (HsStrTy stxt str))
  | Atom (AInteger IL {il_value=n, il_text=stxt}) <- form =
    return (mkLit l (HsNumTy stxt n))
  | otherwise = builderError
  where
    mkLit loc lit = lA loc (HsTyLit NOEXT lit)
{-# INLINABLE b_tyLitT #-}

b_opOrAppT :: Code -> [HType] -> Builder HType
b_opOrAppT form@(LForm (L l ty)) typs
  -- Perhaps empty list
  | null typs = b_symT form
  -- Constructor application (not promoted)
  | Atom (ASymbol name) <- ty
  , isLexConSym name =
    let lrname = lN l (mkUnqual tcName name)
#if MIN_VERSION_ghc(9,4,0)
        f lhs rhs = lA l (mkHsOpTy NotPromoted lhs lrname rhs)
#else
        f lhs rhs = lA l (mkHsOpTy lhs lrname rhs)
#endif
    in  return (foldr1 f (map (parenthesizeHsType' opPrec) typs))
  -- Var type application
  | otherwise =
    do op <- b_symT form
       b_appT (op:typs)
{-# INLINABLE b_opOrAppT #-}

b_prmConT :: Code -> Builder HType
b_prmConT (LForm (L l form)) =
  case form of
    Atom (ASymbol name) -> return $! ty name
    _                   -> builderError
  where
    ty name = lA l (hsTyVar iSPROMOTED (lN l (rname name)))
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
{-# INLINABLE b_prmConT #-}

b_appT :: [HType] -> Builder HType
b_appT []     = builderError
b_appT (x:xs) =
  case xs of
    [] -> return x
    _  -> let f t1 t2 = addCLocAA t1 t2 (HsAppTy NOEXT t1 (parTyApp t2))
          in  pure (foldl' f x xs)
{-# INLINABLE b_appT #-}

b_listT :: HType -> HType
b_listT ty@(L l _) = L l (HsListTy NOEXT ty)
{-# INLINABLE b_listT #-}

b_nilT :: Code -> HType
b_nilT (LForm (L l _)) =
  lA l (hsTyVar NotPromoted (lN l listTyCon_RDR))
{-# INLINABLE b_nilT #-}

b_tupT :: Code -> [HType] -> HType
b_tupT (LForm (L l _)) ts =
  case ts of
   [] -> lA l (hsTyVar NotPromoted (lN l tup))
     where
       tup = getRdrName (tupleTyCon Boxed 2)
   _  -> lA l (hsTupleTy hsBoxedTuple ts)
{-# INLINABLE b_tupT #-}

b_bangT :: Code -> HType -> HType
b_bangT (LForm (L l _)) t = lA l (hsBangTy srcBang (parTyApp t))
  where
    srcBang = HsSrcBang (SourceText "b_bangT") NoSrcUnpack SrcStrict
{-# INLINABLE b_bangT #-}

b_forallT :: Code -> ([HTyVarBndrSpecific], ([HType], HType)) -> HType
b_forallT (LForm (L l0 _)) (bndrs, (ctxts, body)) =
  let ty0 = lA l0 (mkHsQualTy_compat ctxts' body)
      ctxts' = la2la (mkLocatedListA ctxts)
#if MIN_VERSION_ghc(8,4,0)
      ty1 = hsParTy (lA l0 (forAllTy bndrs ty0))
#else
      ty1 = forAllTy bndrs ty0
#endif
  in  lA l0 ty1
{-# INLINABLE b_forallT #-}

b_qualT :: Code -> ([HType], HType) -> HType
b_qualT (LForm (L l _)) (ctxts, body) =
  lA l (mkHsQualTy_compat (la2la (mkLocatedListA ctxts)) body)

{-# INLINABLE b_qualT #-}

b_kindedType :: Code -> HType -> HType -> HType
b_kindedType (LForm (L l _)) ty kind =
#if MIN_VERSION_ghc(8,8,0)
   -- Parens for kind signature were removed in ghc 8.8.1. To show
   -- parens in generated Haskell code, explicitly adding at this point.
   lA l (hsParTy (lA l (HsKindSig NOEXT ty kind)))
#else
   L l (HsKindSig NOEXT ty kind)
#endif
{-# INLINABLE b_kindedType #-}

b_docT :: HType -> LHsDocString -> HType
b_docT ty doc = let l = getLocA ty in lA l (HsDocTy NOEXT ty doc')
  where
    doc' = lHsDocString2LHsDoc doc
{-# INLINABLE b_docT #-}

b_unpackT :: Code -> HType -> HType
b_unpackT (LForm (L l _)) t = lA l (hsBangTy bang t')
  where
    bang = HsSrcBang (SourceText "b_unpackT") SrcUnpack strictness
    (strictness, t') =
      case t of
        L _ (HsBangTy _EXT (HsSrcBang _ _ st) t0) -> (st, t0)
        _                                         -> (NoSrcStrict, t)
{-# INLINABLE b_unpackT #-}

b_prmListT :: ([Code] -> Builder [HType]) -> Code -> Builder HType
b_prmListT prsr typs =
  case typs of
    LForm (L l (HsList xs))
      | null xs   -> return (lA l (hsExplicitListTy []))
      | otherwise -> do
          tys <- prsr xs
          return $! lA l (hsExplicitListTy tys)
    _ -> builderError
{-# INLINABLE b_prmListT #-}

b_prmTupT :: ([Code] -> Builder [HType]) -> [Code] -> Builder HType
b_prmTupT prsr typs =
  case typs of
    hd:tl
      | isCommaSymbol hd -> do
        tys <- prsr tl
        let tys' = map unPromoteTyVar tys
            l = getLoc (mkLocatedList (map unLForm typs))
        return (lA l (hsExplicitTupleTy tys'))
    _ -> builderError
{-# INLINABLE b_prmTupT #-}

isCommaSymbol :: Code -> Bool
isCommaSymbol (LForm (L _ form)) =
  case form of
    Atom (ASymbol ",") -> True
    _                  -> False
{-# INLINABLE isCommaSymbol #-}

hsTupleTy :: HsTupleSort -> [HType] -> HsType PARSED
hsTupleTy = HsTupleTy NOEXT
{-# INLINABLE hsTupleTy #-}

hsBangTy :: HsSrcBang -> HType -> HsType PARSED
hsBangTy = HsBangTy NOEXT
{-# INLINABLE hsBangTy #-}

forAllTy :: [HTyVarBndrSpecific] -> HType -> HsType PARSED
forAllTy bndrs body =
  HsForAllTy { hst_body = body
#if MIN_VERSION_ghc(9,2,0)
             , hst_tele = mkHsForAllInvisTele NOEXT bndrs
#elif MIN_VERSION_ghc(9,0,0)
             , hst_tele = mkHsForAllInvisTele bndrs
#else
             , hst_bndrs = bndrs
#  if MIN_VERSION_ghc(8,10,0)
             , hst_fvf = ForallInvis
#  endif
#endif

#if MIN_VERSION_ghc(8,6,0)
             , hst_xforall = NOEXT
#endif
             }
{-# INLINABLE forAllTy #-}

hsParTy :: HType -> HsType PARSED
hsParTy = HsParTy NOEXT
{-# INLINABLE hsParTy #-}

hsTyVar :: PROMOTIONFLAG -> LIdP PARSED -> HsType PARSED
hsTyVar = HsTyVar NOEXT
{-# INLINABLE hsTyVar #-}

hsExplicitListTy :: [HType] -> HsType PARSED
hsExplicitListTy =
#if MIN_VERSION_ghc(8,6,0)
  HsExplicitListTy NOEXT iSPROMOTED
#else
  HsExplicitListTy iSPROMOTED placeHolderKind
#endif

hsExplicitTupleTy :: [HType] -> HsType PARSED
hsExplicitTupleTy =
#if MIN_VERSION_ghc(8,6,0)
  HsExplicitTupleTy NOEXT
#else
  HsExplicitTupleTy []
#endif

hsBoxedTuple :: HsTupleSort
#if MIN_VERSION_ghc(9,2,0)
hsBoxedTuple = HsBoxedOrConstraintTuple
#else
hsBoxedTuple = HsBoxedTuple
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
{-# INLINABLE parTyApp #-}

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
