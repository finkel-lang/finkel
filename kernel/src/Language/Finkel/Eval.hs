{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
-- | Module containing functions for code evaluation.
module Language.Finkel.Eval
  ( evalDecls
  , evalExpr
  , evalExprType
  , evalTypeKind
  ) where

-- base
import Control.Monad.IO.Class  (liftIO)

#if MIN_VERSION_ghc(8,4,0) && !MIN_VERSION_ghc(8,8,0)
import Data.IORef              (readIORef)
#endif

-- ghc
import ErrUtils                (Messages)
import Exception               (throwIO)
import GhcMonad                (GhcMonad (..))
import HscTypes                (HscEnv (..), InteractiveContext (..),
                                TyThing (..), mkSrcErr)
import InteractiveEval         (compileParsedExprRemote)
import TcRnDriver              (TcRnExprMode (..), tcRnExpr, tcRnType)
import TyCoRep                 (Kind, Type (..))
import VarEnv                  (emptyTidyEnv)

#if MIN_VERSION_ghc(8,10,0)
import TcHsSyn                 (ZonkFlexi (..))
import TyCoTidy                (tidyType)
#else
import TyCoRep                 (tidyType)
#endif

#if MIN_VERSION_ghc(8,8,0)
import GhcMonad                (withSession)
import HscMain                 (hscParsedDecls)
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
import Module                  (ModLocation (..), Module, moduleName,
                                moduleNameString)
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
import Language.Finkel.Builder
import Language.Finkel.Fnk


-- ---------------------------------------------------------------------
--
-- Eval functions
--
-- ---------------------------------------------------------------------

-- | Evaluate given expression to haskell value.
evalExpr :: HExpr -> Fnk HValue
evalExpr expr = do
  fhv <- compileParsedExprRemote expr
  liftIO (withForeignRef fhv localRef)
{-# INLINE evalExpr #-}

-- | Evaluate the type of given expression.
evalExprType :: HExpr -> Fnk Type
evalExprType expr = do
  -- See `InteractiveEval.exprType' and `HscMain.hscTcExpr'. As in
  -- `evalDecls', taking HExpr instead of Haskell source code String.
  --
  -- XXX: Currently, `TcRnExprMode' is hard coded as `TM_Inst' in below
  -- call to `tcRnExpr'. In ghci, user can type in and specify the mode
  -- from REPL session.
  --
  hsc_env <- getSession
  ty <- ioMsgMaybe $ tcRnExpr hsc_env TM_Inst expr
  return $ tidyType emptyTidyEnv ty

-- | Evaluate the kind of given type.  Returned values is a pair of the
-- argument type and the kind of that type.
evalTypeKind :: HType -> Fnk (Type, Kind)
evalTypeKind ty = do
  -- See `InteractiveEval.typeKind' and `HscMain.hscKcType'.
  --
  -- XXX: The second argument of `tcRnType' is hard coded as `True' in below
  -- code.
  --
  hsc_env <- getSession
  ioMsgMaybe $ tcRnType' hsc_env True ty

-- | Evaluate given declarations. The returned value is resulting
-- 'TyThing's of declarations and updated interactive context.
evalDecls :: [HDecl] -> Fnk ([TyThing], InteractiveContext)
#if MIN_VERSION_ghc(8,8,0)
evalDecls decls =
  withSession (\hsc_env -> liftIO (hscParsedDecls hsc_env decls))
#else
evalDecls decls = do
  -- Mostly doing similar works done in `HscMain.hscDeclsWithLocation',
  -- but this function is wrapped with 'Fnk' instead of 'Hsc'. And takes
  -- list of 'HDecl' instead of 'String' of declarations codes as
  -- argument.
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
  debugFnk ("[Language.Finkel.Eval.envDecls] this_mod=" ++
            moduleNameString (moduleName this_mod))
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
fnkcDesugar' :: ModLocation -> TcGblEnv -> Fnk ModGuts
fnkcDesugar' mod_location tc_result = do
  hsc_env <- getSession
  r <- ioMsgMaybe (deSugar hsc_env mod_location tc_result)

  -- In `Hsc', `handleWarning' is called at this point. But currently
  -- Fnk does not keep tracks of warning messages, so does nothing ...
  --
  -- handleWarnings

  return r

-- | GHC version compatibility helper for combining 'hscSimplify'
-- and 'tcg_th_coreplugins'.
hscSimplify' :: HscEnv -> ModGuts -> TcGblEnv -> IO ModGuts
#if MIN_VERSION_ghc(8,4,0)
hscSimplify' hsc_env modguts tc_gblenv = do
  plugins <- readIORef (tcg_th_coreplugins tc_gblenv)
  hscSimplify hsc_env plugins modguts
#else
hscSimplify' hsc_env modguts _tc_gblenv =
  hscSimplify hsc_env modguts
#endif

-- | GHC version compatibility helper for 'corePrepPgm'.
corePrepPgm' :: HscEnv -> Module -> ModLocation -> CoreProgram
             -> [TyCon] -> IO CoreProgram
#if MIN_VERSION_ghc(8,4,0)
corePrepPgm' hsc_env this_mod mod_loc binds data_tycons =
  fmap fst (corePrepPgm hsc_env this_mod mod_loc binds data_tycons)
#else
corePrepPgm' hsc_env this_mod mod_loc binds data_tycons =
  corePrepPgm hsc_env this_mod mod_loc binds data_tycons
#endif

#endif

-- ---------------------------------------------------------------------
--
-- Auxiliary
--
-- ---------------------------------------------------------------------

-- | Like 'HscMain.ioMsgMaybe', but for 'Fnk'.
ioMsgMaybe :: IO (Messages, Maybe a) -> Fnk a
ioMsgMaybe ioA = do
  -- XXX: Show warning messages with DynFlags settings.
  ((_warns, errs), mb_r) <- liftIO ioA
  case mb_r of
    Nothing -> liftIO (throwIO (mkSrcErr errs))
    Just r  -> return r

-- | GHC version compatibility helper for 'tcRnType'.
tcRnType' :: HscEnv -> Bool -> HType -> IO (Messages, Maybe (Type, Kind))
#if MIN_VERSION_ghc(8,10,0)
tcRnType' hsc_env tidy typ = tcRnType hsc_env DefaultFlexi tidy typ
#else
tcRnType' = tcRnType
#endif
