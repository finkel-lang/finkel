{-# LANGUAGE CPP #-}
-- | Utility codes for syntax.
module Language.SK.Syntax.SynUtils where

-- ghc
import Bag (consBag, emptyBag, listToBag)
import BasicTypes ( SourceText(..)
#if MIN_VERSION_ghc (8,4,0)
                  , IntegralLit(..)
#endif
                  )
import FastString (FastString)
import HsBinds ( HsBindLR(..), HsLocalBindsLR(..), HsValBindsLR(..)
               , emptyLocalBinds )
import HsDecls (HsDecl(..))
import HsExpr ( GRHSs(..), LGRHS, LHsExpr, LMatch, Match(..)
              , MatchGroup(..) )
import HsLit (HsOverLit(..))
import HsPat ( HsRecField'(..), LHsRecField
             , LHsRecUpdField )
import HsTypes ( AmbiguousFieldOcc(..), FieldOcc(..), LHsContext
               , HsTyVarBndr(..), HsType(..), mkFieldOcc )
import HsUtils (mkFunBind, mkHsIntegral)
import OccName (tvName)
import OrdList (OrdList, fromOL, toOL)
import RdrHsSyn (cvTopDecls)
import RdrName (mkUnqual)
import SrcLoc ( GenLocated(..), Located, SrcSpan, combineLocs
              , combineSrcSpans, noLoc )

#if MIN_VERSION_ghc(8,6,0)
import HsExtension (noExt)
#else
import PlaceHolder (PlaceHolder(..), placeHolderType)
#endif

-- Internal
import Language.SK.Builder
import Language.SK.Form

#include "Syntax.h"

-- ---------------------------------------------------------------------
--
-- Auxiliary
--
-- ---------------------------------------------------------------------

-- | Build 'HLocalBinds' from list of 'HDecl's.
declsToBinds :: SrcSpan -> [HDecl] -> HLocalBinds
declsToBinds l decls = L l binds'
  where
    binds' = case decls of
      [] -> emptyLocalBinds
      _  -> mkHsValBinds_compat (listToBag binds) sigs
    -- Using 'RdrHsSyn.cvTopDecls' to group same names in where
    -- clause. Perhaps better to do similar things done in
    -- 'RdrHsSyn.cvBindGroup', which is dedicated for 'P' monad ...
    decls' = cvTopDecls (toOL decls)
    (binds, sigs) = go ([],[]) decls'
    go (bs,ss) ds =
      case ds of
        []    -> (bs, ss)
        d:ds' -> case d of
#if MIN_VERSION_ghc(8,6,0)
          L ld (ValD _ b) -> go (L ld b:bs,ss) ds'
          L ld (SigD _ s) -> go (bs,L ld s:ss) ds'
#else
          L ld (ValD   b) -> go (L ld b:bs,ss) ds'
          L ld (SigD   s) -> go (bs,L ld s:ss) ds'
#endif
          -- XXX: Ignoring.
          _               -> go (bs,ss) ds'

-- Function defined in 'HsUtils', not exported.
mkLocatedList ::  [Located a] -> Located [Located a]
mkLocatedList [] = noLoc []
mkLocatedList ms = L (combineLocs (head ms) (last ms)) ms
{-# INLINE mkLocatedList #-}

-- | Convert record field constructor expression to record field update
-- expression.
cfld2ufld :: LHsRecField PARSED HExpr
          -> LHsRecUpdField PARSED
-- Almost same as 'mk_rec_upd_field' in 'RdrHsSyn'
#if MIN_VERSION_ghc(8,6,0)
cfld2ufld (L l0 (HsRecField (L l1 (FieldOcc _ rdr)) arg pun)) =
  L l0 (HsRecField (L l1 unambiguous) arg pun)
  where
    unambiguous = Unambiguous noExt rdr
cfld2ufld _ = error "cfld2ufld"
#else
cfld2ufld (L l0 (HsRecField (L l1 (FieldOcc rdr _)) arg pun)) =
  L l0 (HsRecField (L l1 unambiguous) arg pun)
  where
    unambiguous = Unambiguous rdr PlaceHolder
#endif
{-# INLINE cfld2ufld #-}

-- | Make 'HsRecField' with given name and located data.
mkcfld :: (FastString, Located a) -> LHsRecField PARSED (Located a)
mkcfld (name, e@(L fl _)) =
  L fl HsRecField { hsRecFieldLbl = mkfname fl name
                  , hsRecFieldArg = e
                  , hsRecPun = False }
  where
    mkfname nl n = L nl (mkFieldOcc (L nl (mkRdrName n)))
{-# INLINE mkcfld #-}

quotedSourceText :: String -> SourceText
quotedSourceText s = SourceText $ "\"" ++ s ++ "\""
{-# INLINE quotedSourceText #-}

-- Following `cvBindsAndSigs`, `getMonoBind`, `has_args`, and
-- `makeFunBind` functions are based on resembling functions defined in
-- `RdrHsSyn` module in ghc package, since these functions were not
-- exported.
--
-- Unlike the original version, `cvBindsAndSigs` has pattern matches
-- for 'ValD' and 'SigD' only, and `getMonoBind` ignores 'DocD'
-- declarations.

cvBindsAndSigs :: OrdList HDecl -> (HBinds, [HSig])
cvBindsAndSigs fb = go (fromOL fb)
  where
    go [] = (emptyBag, [])
#if MIN_VERSION_ghc(8,6,0)
    go (L l (ValD _ d) : ds)
#else
    go (L l (ValD d) : ds)
#endif
      = let (b', ds') = getMonoBind (L l d) ds
            (bs, ss) = go ds'
        in  (b' `consBag` bs, ss)
#if MIN_VERSION_ghc(8,6,0)
    go (L l (SigD _ s) : ds)
#else
    go (L l (SigD s) : ds)
#endif
      = let (bs, ss) = go ds
        in  (bs, L l s : ss)

    -- XXX: Ignoring  other constructors.
    go (_ : ds) = go ds

getMonoBind :: HBind -> [HDecl] -> (HBind, [HDecl])
getMonoBind (L loc1 (FunBind { fun_id = fun_id1@(L _ f1),
                               fun_matches
                                 = MG { mg_alts = L _ mtchs1 }}))
            binds
  | has_args mtchs1 = go mtchs1 loc1 binds
  where
    go mtchs loc
#if MIN_VERSION_ghc(8,6,0)
       (L loc2 (ValD _ (FunBind { fun_id = L _ f2,
                                  fun_matches
                                    = MG { mg_alts = L _ mtchs2 }}))

#else
       (L loc2 (ValD (FunBind { fun_id = L _ f2,
                                fun_matches
                                  = MG { mg_alts = L _ mtchs2 }}))
#endif
                : binds2)
      | f1 == f2 = go (mtchs2 ++ mtchs)
                      (combineSrcSpans loc loc2) binds2
    go mtchs loc binds2
      = (L loc (mkFunBind fun_id1 (reverse mtchs)), binds2)
      -- Reverse the final matches, to get it back in the right order

getMonoBind bind binds = (bind, binds)

-- Don't group together FunBinds if they have no arguments.  This is
-- necessary that variable bindings with no arguments are now treated as
-- FunBinds rather than pattern bindings.
has_args :: [LMatch PARSED (LHsExpr PARSED)] -> Bool
has_args (L _ mtch:_) = not (null (m_pats mtch))
has_args []                             =
  error "Language.SK.Syntax.Internal:has_args"

codeToUserTyVar :: Code -> HTyVarBndr
codeToUserTyVar code =
  case code of
    LForm (L l (Atom (ASymbol name))) ->
      L l (UserTyVar NOEXT (L l (mkUnqual tvName name)))
    _ -> error "Language.SK.Syntax.Internal:codeToUserTyVar"
{-# INLINE codeToUserTyVar #-}

-- | Auxiliary function to absorb version compatibiity of
-- 'mkHsIntegral'.
mkHsIntegral_compat :: Integer -> HsOverLit PARSED
mkHsIntegral_compat n =
#if MIN_VERSION_ghc(8,6,0)
  let il = IL { il_text = SourceText (show n)
              , il_neg = n < 0
              , il_value = n }
  in  mkHsIntegral il
#elif MIN_VERSION_ghc(8,4,0)
  let il = IL { il_text = SourceText (show n)
              , il_neg = n < 0
              , il_value = n }
  in  mkHsIntegral il placeHolderType
#else
  mkHsIntegral (SourceText (show n)) n placeHolderType
#endif
{-# INLINE mkHsIntegral_compat #-}

mkGRHSs :: [LGRHS PARSED t] -> [HDecl] -> SrcSpan -> GRHSs PARSED t
mkGRHSs grhss decls l = gRHSs grhss (declsToBinds l decls)
  where
    gRHSs = GRHSs NOEXT
{-# INLINE mkGRHSs #-}

mkHsValBinds_compat :: HBinds -> [HSig] -> HsLocalBindsLR PARSED PARSED
mkHsValBinds_compat binds sigs =
#if MIN_VERSION_ghc(8,6,0)
  HsValBinds noExt (ValBinds noExt binds sigs)
#else
  HsValBinds (ValBindsIn binds sigs)
#endif

mkHsQualTy_compat :: LHsContext PARSED -> HType -> HsType PARSED
mkHsQualTy_compat ctxt body =
  HsQualTy { hst_ctxt = ctxt
#if MIN_VERSION_ghc(8,6,0)
           , hst_xqual = noExt
#endif
           , hst_body = body }
