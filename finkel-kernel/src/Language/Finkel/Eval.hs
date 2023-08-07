{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
-- | Module containing functions for code evaluation.
module Language.Finkel.Eval
  ( evalDecls
  , evalExpr
  , evalExprType
  , evalTypeKind
  ) where

#include "ghc_modules.h"

-- base
import Control.Monad.IO.Class  (MonadIO (..))

#if MIN_VERSION_ghc(8,4,0) && !MIN_VERSION_ghc(8,8,0)
import Data.IORef              (readIORef)
#endif

-- ghc
import GHC_Core_TyCo_Rep       (Kind, Type (..))
import GHC_Driver_Env_Types    (HscEnv (..))
import GHC_Driver_Monad        (GhcMonad (..))
import GHC_Runtime_Context     (InteractiveContext (..))
import GHC_Runtime_Eval        (compileParsedExprRemote)
import GHC_Tc_Module           (TcRnExprMode (..), tcRnExpr, tcRnType)
import GHC_Types_TyThing       (TyThing (..))
import GHC_Types_Var_Env       (emptyTidyEnv)
import GHC_Utils_Error         (Messages)

#if MIN_VERSION_ghc(9,4,0)
import GHC.Driver.Errors.Types (GhcMessage, hoistTcRnMessage)
import GHC.Tc.Errors.Types     (TcRnMessage)
#elif MIN_VERSION_ghc(9,2,0)
import GHC_Types_Error         (DecoratedSDoc)
#endif

#if MIN_VERSION_ghc(9,2,0)
import GHC_Types_Error         (partitionMessages)
import GHC_Types_SourceError   (throwErrors)
#else
import Control.Exception       (throwIO)
import GHC_Types_SourceError   (mkSrcErr)
#endif

#if MIN_VERSION_ghc(8,10,0)
import GHC_Core_TyCo_Tidy      (tidyType)
import GHC_Tc_Utils_Zonk       (ZonkFlexi (..))
#else
import TyCoRep                 (tidyType)
#endif

#if MIN_VERSION_ghc(8,8,0)
import GHC_Driver_Main         (hscParsedDecls)
import GHC_Driver_Monad        (withSession)
#else
import ByteCodeGen             (byteCodeGen)
import ConLike                 (ConLike (..))
import CorePrep                (corePrepPgm)
import CoreSyn                 (CoreProgram, bindersOfBinds)
import Desugar                 (deSugar)
import FastString              (fsLit)
import HscMain                 (hscAddSptEntries, hscSimplify)
import HscTypes                (CgGuts (..), ModDetails (..), ModGuts (..),
                                extendInteractiveContext)
import Id                      (idName, isDFunId, isImplicitId)
import Linker                  (linkDecls)
import Module                  (ModLocation (..), Module)
import Name                    (isExternalName)
import SrcLoc                  (SrcLoc (..), srcLocSpan)
import TcRnDriver              (tcRnDeclsi)
import TcRnTypes               (TcGblEnv (..))
import TidyPgm                 (tidyProgram)
import TyCon                   (TyCon, isDataTyCon, isImplicitTyCon)
import Util                    (filterOut)
#endif

-- ghci
import GHCi.RemoteTypes        (HValue, localRef, withForeignRef)

-- internal
import Language.Finkel.Builder (HDecl, HExpr, HType)


-- ---------------------------------------------------------------------
--
-- Eval functions
--
-- ---------------------------------------------------------------------

-- | Evaluate given expression to haskell value.
evalExpr :: GhcMonad m => HExpr -> m HValue
evalExpr expr = do
  fhv <- compileParsedExprRemote expr
  liftIO (withForeignRef fhv localRef)
{-# INLINABLE evalExpr #-}

-- | Evaluate the type of given expression.
evalExprType :: GhcMonad m => HExpr -> m Type
evalExprType expr = do
  -- See `InteractiveEval.exprType' and `HscMain.hscTcExpr'. As in `evalDecls',
  -- taking HExpr instead of Haskell source code String.
  --
  -- XXX: Currently, `TcRnExprMode' is hard coded as `TM_Inst' in below call to
  -- `tcRnExpr'. In ghci, user can type in and specify the mode from REPL
  -- session.
  --
  hsc_env <- getSession
  ty <- ioMsgMaybe $ hoistTcRnMessage' $ tcRnExpr hsc_env TM_Inst expr
  return $ tidyType emptyTidyEnv ty
{-# INLINABLE evalExprType #-}

-- | Evaluate the kind of given type.  Returned values is a pair of the
-- argument type and the kind of that type.
evalTypeKind :: GhcMonad m => HType -> m (Type, Kind)
evalTypeKind ty = do
  -- See `InteractiveEval.typeKind' and `HscMain.hscKcType'.
  --
  -- XXX: The second argument of `tcRnType' is hard coded as `True' in below
  -- code.
  --
  hsc_env <- getSession
  ioMsgMaybe $ hoistTcRnMessage' $ tcRnType' hsc_env True ty
{-# INLINABLE evalTypeKind #-}

-- | Evaluate given declarations. The returned value is resulting 'TyThing's of
-- declarations and updated interactive context.
evalDecls :: GhcMonad m => [HDecl] -> m ([TyThing], InteractiveContext)
#if MIN_VERSION_ghc(8,8,0)
evalDecls decls =
  withSession (\hsc_env -> liftIO (hscParsedDecls hsc_env decls))
#else
evalDecls decls = do
  -- Mostly doing similar works done in `HscMain.hscDeclsWithLocation', but this
  -- function is wrapped with 'Fnk' instead of 'Hsc'. And takes list of 'HDecl'
  -- instead of 'String' of declarations codes as argument.
  hsc_env <- getSession
  tc_gblenv <- ioMsgMaybe (tcRnDeclsi hsc_env decls)
  let defaults = tcg_default tc_gblenv
      interactive_loc =
         ModLocation { ml_hs_file = Nothing
                     , ml_hi_file = error "ewc:ml_hi_file"
                     , ml_obj_file = error "ewc:ml_obj_file" }
  ds_result <- fnkcDesugar' interactive_loc tc_gblenv
  simpl_mg <- liftIO (hscSimplify' hsc_env ds_result tc_gblenv)
  (tidy_cg, mod_details) <- liftIO (tidyProgram hsc_env simpl_mg)
  let !CgGuts { cg_module = this_mod
              , cg_binds = core_binds
              , cg_tycons = tycons
              , cg_modBreaks = mod_breaks } = tidy_cg
      !ModDetails { md_insts = cls_insts
                  , md_fam_insts = fam_insts } = mod_details
      data_tycons = filter isDataTyCon tycons
  prepd_binds <-
    liftIO (corePrepPgm' hsc_env this_mod interactive_loc
                         core_binds data_tycons)
  cbc <- liftIO (byteCodeGen hsc_env this_mod prepd_binds
                             data_tycons mod_breaks)
  let evalDeclsSrcLoc =
        UnhelpfulLoc (fsLit "<Language.Finkel.Eval.evalDecls>")
      src_span = srcLocSpan evalDeclsSrcLoc
  liftIO (linkDecls hsc_env src_span cbc)
  liftIO (hscAddSptEntries hsc_env (cg_spt_entries tidy_cg))

  let tcs = filterOut isImplicitTyCon (mg_tcs simpl_mg)
      patsyns = mg_patsyns simpl_mg
      ext_ids = [ id' | id' <- bindersOfBinds core_binds
                      , isExternalName (idName id')
                      , not (isDFunId id' || isImplicitId id')]
      new_tythings = map AnId ext_ids ++ map ATyCon tcs ++
                     map (AConLike . PatSynCon) patsyns
      ictxt = hsc_IC hsc_env
      fix_env = tcg_fix_env tc_gblenv
      new_ictxt = extendInteractiveContext ictxt new_tythings
                                           cls_insts fam_insts
                                           defaults fix_env

  return (new_tythings, new_ictxt)

-- | Like 'HscMain.hscDesugar'', but for 'Fnk'.
fnkcDesugar' :: GhcMonad m => ModLocation -> TcGblEnv -> m ModGuts
fnkcDesugar' mod_location tc_result = do
  hsc_env <- getSession
  r <- ioMsgMaybe (deSugar hsc_env mod_location tc_result)

  -- In `Hsc', `handleWarning' is called at this point. But currently Fnk does
  -- not keep tracks of warning messages, so does nothing ...
  --
  -- handleWarnings

  return r

-- | GHC version compatibility helper for combining 'hscSimplify'
-- and 'tcg_th_coreplugins'.
hscSimplify' :: HscEnv -> ModGuts -> TcGblEnv -> IO ModGuts
#  if MIN_VERSION_ghc(8,4,0)
hscSimplify' hsc_env modguts tc_gblenv = do
  plugins <- readIORef (tcg_th_coreplugins tc_gblenv)
  hscSimplify hsc_env plugins modguts
#  else
hscSimplify' hsc_env modguts _tc_gblenv =
  hscSimplify hsc_env modguts
#  endif

-- | GHC version compatibility helper for 'corePrepPgm'.
corePrepPgm' :: HscEnv -> Module -> ModLocation -> CoreProgram
             -> [TyCon] -> IO CoreProgram

#  if MIN_VERSION_ghc(8,4,0)
corePrepPgm' hsc_env this_mod mod_loc binds data_tycons =
  fmap fst (corePrepPgm hsc_env this_mod mod_loc binds data_tycons)
#  else
corePrepPgm' = corePrepPgm
#  endif

#endif

-- ---------------------------------------------------------------------
--
-- Auxiliary
--
-- ---------------------------------------------------------------------

-- Separation of TcRnMessage and GhcMessage was introduced in ghc 9.4.
#if MIN_VERSION_ghc(9,4,0)
hoistTcRnMessage' ::
  Monad m => m (Messages TcRnMessage, a) -> m (Messages GhcMessage, a)
hoistTcRnMessage' = hoistTcRnMessage
#else
hoistTcRnMessage' :: a -> a
hoistTcRnMessage' = id
#endif
{-# INLINABLE hoistTcRnMessage' #-}

-- | Like 'HscMain.ioMsgMaybe', but for 'Fnk'.
#if MIN_VERSION_ghc(9,4,0)
ioMsgMaybe :: MonadIO m => IO (Messages GhcMessage, Maybe a) -> m a
#elif MIN_VERSION_ghc(9,2,0)
ioMsgMaybe :: MonadIO m => IO (Messages DecoratedSDoc, Maybe a) -> m a
#else
ioMsgMaybe :: MonadIO m => IO (Messages, Maybe a) -> m a
#endif

#if MIN_VERSION_ghc(9,2,0)
ioMsgMaybe ioA = do
  -- XXX: Log warning messages.
  (msgs, mb_r) <- liftIO ioA
  let (_warns, errs) = partitionMessages msgs
  maybe (throwErrors errs) pure mb_r
#else
ioMsgMaybe ioA = do
  ((_warns, errs), mb_r) <- liftIO ioA
  maybe (liftIO (throwIO (mkSrcErr errs))) return mb_r
#endif
{-# INLINABLE ioMsgMaybe #-}

-- | GHC version compatibility helper for 'tcRnType'.
#if MIN_VERSION_ghc(9,4,0)
tcRnType'
  :: HscEnv -> Bool -> HType -> IO (Messages TcRnMessage, Maybe (Type, Kind))
#elif MIN_VERSION_ghc(9,2,0)
tcRnType'
  :: HscEnv -> Bool -> HType -> IO (Messages DecoratedSDoc, Maybe (Type, Kind))
#else
tcRnType'
  :: HscEnv -> Bool -> HType -> IO (Messages, Maybe (Type, Kind))
#endif

#if MIN_VERSION_ghc(8,10,0)
tcRnType' hsc_env = tcRnType hsc_env DefaultFlexi
#else
tcRnType' = tcRnType
#endif
{-# INLINABLE tcRnType' #-}
