{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
-- | Module for code evaluation.
module Language.SK.Eval
  ( evalDecls
  , evalExpr
  , evalExprType
  , evalTypeKind
  ) where

-- base
import Control.Monad.IO.Class (liftIO)
#if MIN_VERSION_ghc(8,4,0)
import Data.IORef (readIORef)
#endif

-- ghc
import ByteCodeGen (byteCodeGen)
import ConLike (ConLike(..))
import CorePrep (corePrepPgm)
import CoreSyn (CoreProgram, bindersOfBinds)
import Desugar (deSugar)
import ErrUtils (Messages)
import Exception (throwIO)
import FastString (fsLit)
import GhcMonad (GhcMonad(..))
import InteractiveEval (compileParsedExprRemote)
import HscMain (hscAddSptEntries, hscSimplify)
import HscTypes ( CgGuts(..), ModDetails(..), HscEnv(..), ModGuts(..)
                , InteractiveContext(..), TyThing(..)
                , extendInteractiveContext, mkSrcErr )
import Id (idName, isDFunId, isImplicitId)
import Linker (linkDecls)
import Module (Module, ModLocation(..), moduleName, moduleNameString)
import Name (isExternalName)
import SrcLoc (SrcLoc(..), srcLocSpan)
import TcRnDriver (TcRnExprMode(..), tcRnDeclsi, tcRnExpr, tcRnType)
import TcRnTypes (TcGblEnv(..))
import TidyPgm (tidyProgram)
import TyCoRep (Type(..), Kind, tidyType)
import TyCon (TyCon, isDataTyCon, isImplicitTyCon)
import Util (filterOut)
import VarEnv (emptyTidyEnv)

-- ghci
import GHCi.RemoteTypes (HValue, localRef, withForeignRef)

-- internal
import Language.SK.Builder
import Language.SK.SKC


-- ---------------------------------------------------------------------
--
-- Eval functions
--
-- ---------------------------------------------------------------------

-- | Evaluate given expression to haskell value.
evalExpr :: HExpr -> Skc HValue
evalExpr expr = do
  fhv <- compileParsedExprRemote expr
  liftIO (withForeignRef fhv localRef)
{-# INLINE evalExpr #-}

-- | Evaluate type of given expression.
evalExprType :: HExpr -> Skc Type
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

evalTypeKind :: HType -> Skc (Type, Kind)
evalTypeKind ty = do
  -- See `InteractiveEval.typeKind' and `HscMain.hscKcType'.
  --
  -- XXX: The second argument passed to `tcRnType' is hard coded as
  -- `True' in below code.
  --
  hsc_env <- getSession
  ioMsgMaybe $ tcRnType hsc_env True ty

-- | Evaluate given declarations.
evalDecls :: [HDecl] -> Skc ([TyThing], InteractiveContext)
evalDecls decls = do
  -- Mostly doing similar works done in `HscMain.hscDeclsWithLocation',
  -- but this function is wrapped with 'Skc' instead of 'Hsc'. And takes
  -- list of 'HDecl' instead of 'String' of declarations codes as
  -- argument.
  hsc_env <- getSession
  tc_gblenv <- ioMsgMaybe (tcRnDeclsi hsc_env decls)
  let defaults = tcg_default tc_gblenv
      interactive_loc =
         ModLocation { ml_hs_file = Nothing
                     , ml_hi_file = error "ewc:ml_hi_file"
                     , ml_obj_file = error "ewc:ml_obj_file"}
  ds_result <- skcDesugar' interactive_loc tc_gblenv
  simpl_mg <- liftIO (hscSimplify_compat hsc_env ds_result tc_gblenv)
  (tidy_cg, mod_details) <- liftIO (tidyProgram hsc_env simpl_mg)
  let !CgGuts { cg_module = this_mod
              , cg_binds = core_binds
              , cg_tycons = tycons
              , cg_modBreaks = mod_breaks } = tidy_cg
      !ModDetails { md_insts = cls_insts
                  , md_fam_insts = fam_insts } = mod_details
      data_tycons = filter isDataTyCon tycons
  debugIO (putStrLn
             ("[Language.SK.Eval.envDecls] this_mod=" ++
              moduleNameString (moduleName this_mod)))
  prepd_binds <-
    liftIO (corePrepPgm_compat hsc_env this_mod interactive_loc
                               core_binds data_tycons)
  cbc <- liftIO (byteCodeGen hsc_env this_mod prepd_binds
                             data_tycons mod_breaks)
  let evalDeclsSrcLoc =
        UnhelpfulLoc (fsLit "<Language.SK.Eval.evalDecls>")
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
  setSession (hsc_env {hsc_IC = new_ictxt})
  return (new_tythings, new_ictxt)

-- ---------------------------------------------------------------------
--
-- Auxiliary
--
-- ---------------------------------------------------------------------

-- | Like 'HscMain.hscDesugar'', but for 'Skc'.
skcDesugar' :: ModLocation -> TcGblEnv -> Skc ModGuts
skcDesugar' mod_location tc_result = do
  hsc_env <- getSession
  r <- ioMsgMaybe (deSugar hsc_env mod_location tc_result)

  -- In `Hsc', `handleWarning' is called at this point. But currently
  -- Skc does not keep tracks of warning messages, so does nothing ...
  --
  -- handleWarnings

  return r

-- | Like 'HscMain.ioMsgMaybe', but for 'Skc'.
ioMsgMaybe :: IO (Messages, Maybe a) -> Skc a
ioMsgMaybe ioA = do
  -- XXX: Show warning messages with DynFlags settings.
  ((_warns, errs), mb_r) <- liftIO ioA
  case mb_r of
    Nothing -> liftIO (throwIO (mkSrcErr errs))
    Just r  -> return r

-- | GHC version compatibility helper for combining 'hscSimplify'
-- and 'tcg_th_coreplugins'.
hscSimplify_compat :: HscEnv -> ModGuts -> TcGblEnv -> IO ModGuts
#if !MIN_VERSION_ghc(8,4,0)
hscSimplify_compat hsc_env modguts _tc_gblenv =
  hscSimplify hsc_env modguts
#else
hscSimplify_compat hsc_env modguts tc_gblenv = do
  plugins <- readIORef (tcg_th_coreplugins tc_gblenv)
  hscSimplify hsc_env plugins modguts
#endif

-- | GHC version compatibility helper for 'corePrepPgm'.
corePrepPgm_compat :: HscEnv -> Module -> ModLocation -> CoreProgram
                   -> [TyCon] -> IO CoreProgram
#if !MIN_VERSION_ghc(8,4,0)
corePrepPgm_compat hsc_env this_mod mod_loc binds data_tycons =
  corePrepPgm hsc_env this_mod mod_loc binds data_tycons
#else
corePrepPgm_compat hsc_env this_mod mod_loc binds data_tycons =
  fmap fst (corePrepPgm hsc_env this_mod mod_loc binds data_tycons)
#endif
