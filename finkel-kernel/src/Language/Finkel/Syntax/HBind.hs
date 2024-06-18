{-# LANGUAGE CPP          #-}
{-# LANGUAGE ViewPatterns #-}

-- | Syntax for binds.
module Language.Finkel.Syntax.HBind where

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
import qualified GHC_Parser_PostProcess          as PostProcess
import           GHC_Types_Fixity                (Fixity)
import           GHC_Types_Name_Reader           (RdrName)
import           GHC_Types_SrcLoc                (GenLocated (..))

#if MIN_VERSION_ghc(9,10,0)
import           GHC_Hs_Binds                    (HsMultAnn (..))
#endif

#if !MIN_VERSION_ghc(9,2,0)
import           GHC_Types_SrcLoc                (SrcSpan)
#endif

-- Internal
import           Language.Finkel.Builder
import           Language.Finkel.Syntax.SynUtils


mkPatBind_compat :: HPat -> [HGRHS] -> [HDecl] -> HsBind PARSED
mkPatBind_compat (dL->L l pat) grhss decls =
  PatBind { pat_lhs = cL l pat
          , pat_rhs = mkGRHSs grhss decls l
#if MIN_VERSION_ghc(9,10,0)
            -- XXX: Does not support HsMultAnn.
          , pat_mult = HsNoMultAnn unused
#endif
            -- XXX: From ghc 9.6 (8.6?), the `pat_ext' field is used for holding
            -- former `pat_ticks' information.
          , pat_ext = unused
#if !MIN_VERSION_ghc(9,6,0)
          , pat_ticks = ([], [])
#endif
          }
{-# INLINABLE mkPatBind_compat #-}

mkHsValBinds :: HBinds -> [HSig] -> HsLocalBindsLR PARSED PARSED
mkHsValBinds binds sigs = HsValBinds unused (ValBinds unused binds sigs)
{-# INLINABLE mkHsValBinds #-}

#if MIN_VERSION_ghc(9,2,0)
mkGRHSs :: [LGRHS PARSED t] -> [HDecl] -> a -> GRHSs PARSED t
#else
mkGRHSs :: [LGRHS PARSED t] -> [HDecl] -> SrcSpan -> GRHSs PARSED t
#endif
mkGRHSs grhss decls l = GRHSs unused grhss (declsToBinds l decls)
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
      _  -> mkHsValBinds (listToBag binds) sigs
    -- Using 'PostProcess.cvTopDecls' to group same names in where
    -- clause. Perhaps better to do similar things done in
    -- 'PostProcess.cvBindGroup', which is dedicated for 'P' monad ...
    decls' = PostProcess.cvTopDecls (toOL decls)
    (binds, sigs) = go ([],[]) decls'
    go (bs,ss) ds =
      case ds of
        []    -> (bs, ss)
        d:ds' -> case d of
          L ld (ValD _ b) -> go (L ld b:bs,ss) ds'
          L ld (SigD _ s) -> go (bs,L ld s:ss) ds'
          -- XXX: Ignoring.
          _               -> go (bs,ss) ds'
{-# INLINABLE declsToBinds #-}

mkFixSig :: [LocatedN RdrName] -> Fixity -> Sig PARSED
-- XXX: Does not support NamespaceSpecifier.
mkFixSig lnames fixity = FixSig unused (FixitySig unused lnames fixity)
{-# INLINABLE mkFixSig #-}
