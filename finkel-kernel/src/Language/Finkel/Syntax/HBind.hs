{-# LANGUAGE CPP          #-}
{-# LANGUAGE ViewPatterns #-}

-- | Syntax for binds.
module Language.Finkel.Syntax.HBind where

#include "Syntax.h"
#include "ghc_modules.h"

-- ghc
import           GHC_Data_Bag                    (listToBag)
import           GHC_Data_OrdList                (toOL)
import           GHC_Hs_Binds                    (FixitySig (..), HsBind,
                                                  HsBindLR (..),
                                                  HsLocalBindsLR (..),
                                                  HsValBindsLR (..), Sig (..),
                                                  emptyLocalBinds)
import           GHC_Hs_Decls                    (HsDecl (..))
import           GHC_Hs_Expr                     (GRHSs (..), LGRHS)
import           GHC_Hs_Utils                    (mkFunBind)
import qualified GHC_Parser_PostProcess          as PostProcess
import           GHC_Types_Fixity                (Fixity)
import           GHC_Types_Name_Reader           (RdrName)
import           GHC_Types_SrcLoc                (GenLocated (..))

#if !MIN_VERSION_ghc(9,2,0)
import           GHC_Types_SrcLoc                (SrcSpan)
#endif

#if MIN_VERSION_ghc(8,10,0)
import           GHC_Types_Basic                 (Origin (..))
#endif

#if !MIN_VERSION_ghc(8,6,0)
import           PlaceHolder                     (placeHolderNames,
                                                  placeHolderType)
#endif

-- Internal
import           Language.Finkel.Builder
import           Language.Finkel.Syntax.SynUtils

mkFunBind_compat :: LocatedN RdrName -> [HMatch] -> HsBind PARSED
#if MIN_VERSION_ghc(8,10,0)
mkFunBind_compat = mkFunBind FromSource
#else
mkFunBind_compat = mkFunBind
#endif
{-# INLINABLE mkFunBind_compat #-}

mkPatBind_compat :: HPat -> [HGRHS] -> [HDecl] -> HsBind PARSED
mkPatBind_compat (dL->L l pat) grhss decls =
  PatBind { pat_lhs = cL l pat
          , pat_rhs = mkGRHSs grhss decls l
#if MIN_VERSION_ghc(8,6,0)
          , pat_ext = NOEXT
#else
          , pat_rhs_ty = placeHolderType
          , bind_fvs = placeHolderNames
#endif
          , pat_ticks = ([], []) }
{-# INLINABLE mkPatBind_compat #-}

mkHsValBinds_compat :: HBinds -> [HSig] -> HsLocalBindsLR PARSED PARSED
mkHsValBinds_compat binds sigs =
#if MIN_VERSION_ghc(8,6,0)
  HsValBinds NOEXT (ValBinds NOEXT binds sigs)
#else
  HsValBinds (ValBindsIn binds sigs)
#endif
{-# INLINABLE mkHsValBinds_compat #-}

#if MIN_VERSION_ghc(9,2,0)
mkGRHSs :: [LGRHS PARSED t] -> [HDecl] -> a -> GRHSs PARSED t
#else
mkGRHSs :: [LGRHS PARSED t] -> [HDecl] -> SrcSpan -> GRHSs PARSED t
#endif
mkGRHSs grhss decls l = GRHSs NOEXT grhss (declsToBinds l decls)
{-# INLINABLE mkGRHSs #-}

-- | Build 'HLocalBinds' from list of 'HDecl's.
#if MIN_VERSION_ghc(9,2,0)
declsToBinds :: a -> [HDecl] -> HLocalBinds
declsToBinds _ decls = binds'
#else
declsToBinds :: SrcSpan -> [HDecl] -> HLocalBinds
declsToBinds l decls = L l binds'
#endif
  where
    binds' = case decls of
      [] -> emptyLocalBinds
      _  -> mkHsValBinds_compat (listToBag binds) sigs
    -- Using 'PostProcess.cvTopDecls' to group same names in where
    -- clause. Perhaps better to do similar things done in
    -- 'PostProcess.cvBindGroup', which is dedicated for 'P' monad ...
    decls' = PostProcess.cvTopDecls (toOL decls)
    (binds, sigs) = go ([],[]) decls'
    go (bs,ss) ds =
      case ds of
        []    -> (bs, ss)
        d:ds' -> case d of
          L ld (ValD _EXT b) -> go (L ld b:bs,ss) ds'
          L ld (SigD _EXT s) -> go (bs,L ld s:ss) ds'
          -- XXX: Ignoring.
          _                  -> go (bs,ss) ds'
{-# INLINABLE declsToBinds #-}

mkFixSig :: [LocatedN RdrName] -> Fixity -> Sig PARSED
mkFixSig lnames fixity = FixSig NOEXT (FixitySig NOEXT lnames fixity)
{-# INLINABLE mkFixSig #-}
