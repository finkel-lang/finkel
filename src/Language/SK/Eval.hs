{-# LANGUAGE BangPatterns #-}
-- | Module for code evaluation.
module Language.SK.Eval
  ( evalDecls
  , evalExpr
  ) where

-- base
import Control.Monad.IO.Class (liftIO)

-- ghc
import ByteCodeGen (byteCodeGen)
import ConLike (ConLike(..))
import CorePrep (corePrepPgm)
import CoreSyn (bindersOfBinds)
import Desugar (deSugar)
import ErrUtils (Messages)
import InteractiveEval (compileParsedExprRemote)
import HscMain (hscSimplify)
import HscTypes ( CgGuts(..), ModDetails(..), ModGuts(..)
                , InteractiveContext(..), extendInteractiveContext )
import Id (idName, isDFunId, isImplicitId)
import Linker (linkDecls)
import Name (isExternalName)
import SrcLoc (srcLocSpan)
import TcRnDriver (tcRnDeclsi)
import TcRnTypes (TcGblEnv(..))
import TidyPgm (tidyProgram)
import TyCon (isDataTyCon, isImplicitTyCon)
import Util (filterOut)

-- internal
import Language.SK.Builder
import Language.SK.GHC
import Language.SK.SKC


-- | Evaluate given expression to haskell value.
evalExpr :: HExpr -> Skc HValue
evalExpr expr = do
  fhv <- compileParsedExprRemote expr
  liftIO (withForeignRef fhv localRef)
{-# INLINE evalExpr #-}

-- | Evaluate given declarations.
evalDecls :: [HDecl] -> Skc ([TyThing], InteractiveContext)
evalDecls decls = do
  -- Mostly doing similar works done in `HscMain.hscDeclsWithLocation',
  -- but this function is wrapped with 'Skc' instead of 'Hsc'. Also,
  -- 'hscDeclsWithlocation' is not exported, and takes 'String' of
  -- declarations codes as argument.
  hsc_env <- getSession
  tc_gblenv <- ioMsgMaybe (tcRnDeclsi hsc_env decls)
  let defaults = tcg_default tc_gblenv
      interactive_loc =
         ModLocation { ml_hs_file = Nothing
                     , ml_hi_file = error "ewc:ml_hi_file"
                     , ml_obj_file = error "ewc:ml_obj_file"}
  ds_result <- skcDesugar' interactive_loc tc_gblenv
  simpl_mg <- liftIO (hscSimplify hsc_env ds_result)
  (tidy_cg, mod_details) <- liftIO (tidyProgram hsc_env simpl_mg)
  let !CgGuts { cg_module = this_mod
              , cg_binds = core_binds
              , cg_tycons = tycons
              , cg_modBreaks = mod_breaks } = tidy_cg
      !ModDetails { md_insts = cls_insts
                  , md_fam_insts = fam_insts } = mod_details
      data_tycons = filter isDataTyCon tycons
  prepd_binds <-
    liftIO (corePrepPgm hsc_env this_mod interactive_loc core_binds
                        data_tycons)
  cbc <- liftIO (byteCodeGen hsc_env this_mod prepd_binds
                             data_tycons mod_breaks)
  let src_span = srcLocSpan evalDeclsSrcLoc
  liftIO (linkDecls hsc_env src_span cbc)

  -- Not done in ghc-8.0.2.
  -- liftIO (hscAddSptEntries hsc_env (cg_spt_entries tidy_cg))

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

-- GHC head exports this from HscMain, but 8.0.2 doesn't.
ioMsgMaybe :: IO (Messages, Maybe a) -> Skc a
ioMsgMaybe ioA = do
  -- XXX: Show warning messages with DynFlags settings.
  ((_warns, errs), mb_r) <- liftIO ioA
  debugIO (putStrLn "ioMsgMaybe")
  case mb_r of
    Nothing -> liftIO (throwIO (mkSrcErr errs))
    Just r  -> return r

-- Like 'HscMain.hscDesugar'', but for 'SKC'.
skcDesugar' :: ModLocation -> TcGblEnv -> Skc ModGuts
skcDesugar' mod_location tc_result = do
  hsc_env <- getSession
  r <- ioMsgMaybe (deSugar hsc_env mod_location tc_result)

  -- In `Hsc', `handleWarning' is called at this point. But currently
  -- Skc does not keep tracks of warning messages, so does nothing ...
  --
  -- handleWarnings

  return r

evalDeclsSrcLoc :: SrcLoc
evalDeclsSrcLoc = UnhelpfulLoc (fsLit "<SK.Eval.evalDecls>")
