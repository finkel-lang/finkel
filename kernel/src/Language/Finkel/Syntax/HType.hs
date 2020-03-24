{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
-- | Syntax for type.
module Language.Finkel.Syntax.HType where

-- ghc
import BasicTypes                      (Boxity (..), SourceText (..))
import FastString                      (headFS, lengthFS, nullFS, tailFS)
import HsDoc                           (LHsDocString)
import HsTypes                         (HsSrcBang (..), HsTupleSort (..),
                                        HsTyLit (..), HsType (..), LHsTyVarBndr,
                                        SrcStrictness (..),
                                        SrcUnpackedness (..), mkAnonWildCardTy,
                                        mkHsAppTy, mkHsAppTys, mkHsOpTy)
import Lexeme                          (isLexCon, isLexConSym, isLexVarSym)
import OccName                         (NameSpace, dataName, tcName, tvName)
import RdrHsSyn                        (setRdrNameSpace)
import RdrName                         (getRdrName, mkQual, mkUnqual)
import SrcLoc                          (GenLocated (..), Located, getLoc)
import TysPrim                         (funTyCon)
import TysWiredIn                      (consDataCon, listTyCon_RDR, tupleTyCon)

#if MIN_VERSION_ghc(8,8,0)
import BasicTypes                      (PromotionFlag (..))
#else
import HsTypes                         (Promoted (..))
#endif

#if MIN_VERSION_ghc(8,8,0)
import TysWiredIn                      (eqTyCon_RDR)
#else
import PrelNames                       (eqTyCon_RDR)
#endif

#if MIN_VERSION_ghc(8,6,0)
import BasicTypes                      (PprPrec, funPrec)
import HsTypes                         (parenthesizeHsType)
#endif

#if MIN_VERSION_ghc(8,6,0)
import HsExtension                     (noExt)
#else
import PlaceHolder                     (placeHolderKind)
#endif

#if !MIN_VERSION_ghc(8,6,0)
import TysWiredIn                      (starKindTyCon)
#endif

#if MIN_VERSION_ghc(8,4,0)
import HsExtension                     (IdP)
#else
#define IdP {- empty -}
#endif

-- Internal
import Language.Finkel.Builder
import Language.Finkel.Form
import Language.Finkel.Syntax.SynUtils

#include "Syntax.h"

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

unPromoteTyVar :: Maybe NameSpace -> HType -> HType
unPromoteTyVar mb_ns (dL->L l (HsTyVar _EXT _ (L ln name))) =
  let name' = maybe name (setRdrNameSpace name) mb_ns
  in  cL l (hsTyVar nOTPROMOTED (L ln name'))
unPromoteTyVar _     other = other
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
          -- Using "isLexVarSym" for "TypeOperator" extension.
          | isLexCon ns || isLexVarSym ns = tcName
          | otherwise                     = tvName
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
    f a@(L l1 _) b = L l1 (hsFunTy (parenthesizeHsType' funPrec a) b)
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
    in  return (L l (hsParTy (foldr1 f typs)))
  -- Var type application
  | otherwise =
    do op <- b_symT form
       b_appT (op:typs)
{-# INLINE b_opOrAppT #-}

b_prmConT :: Code -> Builder HType
b_prmConT (LForm (L l form))
  | Atom (AString _ str) <- form =
    let name = str
        rname =
          case name of
            ":" -> getRdrName consDataCon
            _   -> maybe (mkUnqual (namespace name) name)
                         (mkQual dataName)
                         (splitQualName name)
        namespace n
          | isLexCon n    = dataName
          | isLexVarSym n = dataName
          | otherwise     = tvName
    in  return (L l (hsTyVar iSPROMOTED (cL l rname)))
  | otherwise = builderError

-- | 'True' when given form is for qSymbol. There are two situations:
-- "qSymbol" (when compiling files) and "Language.Finkel.qSymbol" (from
-- REPL) are used for quoted names after macro expansion.
isQSymbol :: Form Atom -> Bool
isQSymbol aform
  | Atom (ASymbol qsym) <- aform
  = qsym == "qSymbol" || qsym == "Language.Finkel.qSymbol"
  | otherwise = False
{-# INLINE isQSymbol #-}

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
b_bangT (LForm (L l _)) t = L l (hsBangTy srcBang t)
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
          -- let tys' = map promoteHType tys
          let tys' = map (unPromoteTyVar (Just tcName)) tys
          return (cL l (hsExplicitListTy tys'))
    _ -> builderError
{-# INLINE b_prmListT #-}

b_prmTupT :: ([Code] -> Builder [HType]) -> Code -> Builder HType
b_prmTupT prsr typs =
  case typs of
    LForm (L l (HsList (hd:tl)))
      | isCommaSymbol hd -> do
        tys <- prsr tl
        -- let tys' = map unPromoteHType tys
        let tys' = map (unPromoteTyVar Nothing) tys
        return (cL l (hsExplicitTupleTy tys'))
    _ -> builderError
{-# INLINE b_prmTupT #-}

isCommaSymbol :: Code -> Bool
isCommaSymbol form
 | LForm (L _ (List [LForm (L _ qsym)
                    ,LForm (L _ (Atom (AString _ ",")))])) <- form
 = isQSymbol qsym
 | otherwise = False
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
#if MIN_VERSION_ghc(8,6,0)
             , hst_xforall = noExt
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

#if MIN_VERSION_ghc(8,6,0)

-- Unlike "HsTypes.parenthesizeHsType" in ghc 8.6.x, does not
-- parenthesize "HsBangTy" constructor, because
-- "HsTypes.parenthesizeHsType" is used for parenthesizing argument in
-- HsFunTy.

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
