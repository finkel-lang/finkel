{-# LANGUAGE CPP   #-}
{-# LANGUAGE GADTs #-}
-- Hooks, in HscEnv
module Language.Finkel.Hooks
  ( finkelHooks
  ) where

#if MIN_VERSION_ghc(9,6,0)
-- base
import Control.Exception                 (displayException)
import Control.Monad                     (when)
import Control.Monad.IO.Class            (MonadIO (..))
import Data.Maybe                        (fromMaybe)
import System.Console.GetOpt             (ArgOrder (..), getOpt)
import System.Exit                       (exitFailure, exitSuccess)
import System.IO                         (hPutStrLn, stderr)

#if !MIN_VERSION_base(4,20,0)
import Data.List                         (foldl')
#endif

-- ghc
import GHC.Driver.Env                    (HscEnv (..), hscSetFlags, runHsc')
import GHC.Driver.Errors.Types           (GhcMessage)
import GHC.Driver.Hooks                  (Hooks (..))
import GHC.Driver.Main                   (hscTypecheckAndGetWarnings,
                                          hscTypecheckRename)
import GHC.Driver.Phases                 (Phase (..))
import GHC.Driver.Pipeline.Execute       (phaseOutputFilenameNew, runPhase)
import GHC.Driver.Pipeline.Phases        (PhaseHook (..), TPhase (..))
import GHC.Driver.Session                (GeneralFlag (..), gopt)

import GHC.Plugins                       (CommandLineOption)

import GHC.Types.SourceError             (throwOneError)
import GHC.Types.SourceFile              (HscSource (..))
import GHC.Types.SrcLoc                  (noLoc, noSrcSpan)

import GHC.Unit.Module.Location          (ModLocation (..))
import GHC.Unit.Module.ModSummary        (ModSummary (..))

import GHC.Utils.Error                   (Messages)
import GHC.Utils.Exception               (ExceptionMonad)
import GHC.Utils.Misc                    (getModificationUTCTime,
                                          modificationTimeIfExists)
import GHC.Utils.Outputable              (text)

import GHC.Runtime.Context               (InteractiveContext (..))
import GHC.Tc.Types                      (FrontendResult (..))

-- Internal
import Language.Finkel.Error             (mkPlainWrappedMsg)
import Language.Finkel.Exception         (finkelExceptionLoc,
                                          handleFinkelException)
import Language.Finkel.Expand            (bcoDynFlags)
import Language.Finkel.Fnk               (FnkEnv (..), FnkInvokedMode (..),
                                          initFnkEnv, runFnk')
import Language.Finkel.Make              (fnkSourceToSummary)
import Language.Finkel.Make.Summary      (TargetSummary (..))
import Language.Finkel.Make.TargetSource (TargetSource (..),
                                          findTargetSourceWithPragma)
import Language.Finkel.Options           (FnkPluginOptions (..),
                                          defaultFnkPluginOptions,
                                          fnkPluginOptions, fpoPragma,
                                          printPluginUsage)
import Language.Finkel.Preprocess        (PpOptions (..), mkPpOptions,
                                          preprocessOrCopy)

-- | Add hooks for compiling Finkel source codes.
--
-- This function will constantly turn on the 'Opt_Pp' flag in the 'DynFlags' of
-- given 'HscEnv' to always trigger the preprocess phase.
finkelHooks :: String -> FnkEnv -> [CommandLineOption] -> HscEnv -> IO HscEnv

-- Actual implementation is for ghc >= 9.6, older versions are not supported.
finkelHooks mod_name fnk_env0 cmd_line_opts hsc_env0 = do
  -- Always setting the Opt_Pp flag on for fnk_default_dflags1 and dflags1,
  -- otherwise the hook for T_HsPp will not run.
  let fnk_default_dflags = bcoDynFlags (ic_dflags (hsc_IC hsc_env0))
      -- XXX: Update targets in expanding seession?
      -- enable_pp_phase =
      --   setGeneralFlag' Opt_Pp .
      --   setGeneralFlag' Opt_UseBytecodeRatherThanObjects .
      --   setGeneralFlag' Opt_WriteIfSimplifiedCore .
      --   flip xopt_set TemplateHaskell

      -- XXX: File local plugin options are ignored.
      (os, _ls, errs) = getOpt Permute fnkPluginOptions cmd_line_opts
      fpo0 = foldl' (flip id) (defaultFnkPluginOptions fnk_env0) os
      fnk_env1 = fpoFnkEnv fpo0
      fnk_env2 = fnk_env1 { envDefaultDynFlags = Just fnk_default_dflags
                          , envInvokedMode = GhcPluginMode }

  fnk_env3 <- initFnkEnv fnk_env2

  let fpo1 = fpo0 {fpoFnkEnv = fnk_env3}
      phase_hook = PhaseHook (fnkPhaseHook fpo1)
      hooks = (hsc_hooks hsc_env0) {runPhaseHook = Just phase_hook}
      hsc_env1 = hsc_env0 {hsc_hooks = hooks}

  case errs of
    _ | fpoHelp fpo1 -> printPluginUsage mod_name >> exitSuccess
    _:_              -> mapM_ putStrLn errs >> exitFailure
    []               -> pure hsc_env1


-- ------------------------------------------------------------------------
--
-- Internal
--
-- ------------------------------------------------------------------------

-- | Hooks for compiling Finkel source codes.
--
-- The hook modifies 'T_HsPp' and 'T_Hsc' phases, other phases are delegated to
-- 'runPhase'.
--
-- The 'T_HsPp' phase is to get preprocessed module header, which is
-- used during module dependency resolution.
--
-- The 'T_Hsc' phase does the compilation of the body of the source code to get
-- 'FrontendResult'.
fnkPhaseHook :: FnkPluginOptions -> TPhase a -> IO a
fnkPhaseHook fpo phase = do
  logStrLn fpo ("fnkPhaseHook: running " <> showTPhase phase)
  case phase of
    T_HsPp pipe_env hsc_env fp hsc_src -> do
      let next = HsPp HsSrcFile
      out_path <- phaseOutputFilenameNew next pipe_env hsc_env Nothing
      runFnkPpPhase fpo hsc_env fp hsc_src out_path
    T_Hsc hsc_env ms -> runFnkTcPhase fpo hsc_env ms
    _ -> runPhase phase

showTPhase :: TPhase a -> String
showTPhase phase = case phase of
  T_Unlit {}        -> "T_Unlit"
  T_FileArgs _ path -> "T_FileArgs: " <> path
  T_Cpp {}          -> "T_Cpp"
  T_HsPp _ _ o i    -> "T_HsPp: " <> o <> " " <> i
  T_HscRecomp {}    -> "T_HscRecomp"
  T_Hsc {}          -> "T_Hsc"
  T_HscPostTc {}    -> "T_HscPostTc"
  T_HscBackend {}   -> "T_HscBackend"
  T_CmmCpp {}       -> "T_CmmCpp"
  T_Cmm {}          -> "T_Cmm"
  T_Cc {}           -> "T_Cc"
  T_As {}           -> "T_As"
#if MIN_VERSION_ghc(9,6,0)
  T_Js {}           -> "T_Js"
  T_ForeignJs {}    -> "T_ForeignJs"
#endif
  T_LlvmOpt {}      -> "T_LlvmOpt"
  T_LlvmLlc {}      -> "T_LlvmLlc"
  T_LlvmMangle {}   -> "T_LlvmMangle"
#if MIN_VERSION_ghc(9,10,0)
  T_LlvmAs {}       -> "T_LlvmAs"
#endif
  T_MergeForeign {} -> "T_MergeForeign"

runFnkPpPhase
  :: FnkPluginOptions -> HscEnv -> FilePath -> FilePath -> FilePath
  -> IO FilePath
runFnkPpPhase fpo hsc_env _orig_fn input_fn output_fn = do
  -- Not parsing command line argument in preprocess phase, the arguments are
  -- shared with fnkPluginOptions.
  let fnk_env = fpoFnkEnv fpo
      ppo0 = mkPpOptions "runFnkPpPhase" fnk_env
      ppo1 = ppo0 { ppoWarnInterp = False
                  , ppoFnkSrcOptions = fpoSrcOptions fpo
                  , ppoVerbosity = envVerbosity fnk_env }

  -- XXX: Not checking the dependency files (the 'mi_usages' field) stored in
  -- interface, will not preprocess this module when the macros in required
  -- modules were changed ... is it fine?
  input_mtime <- getModificationUTCTime input_fn
  mb_output_mtime <- modificationTimeIfExists output_fn
  let can_reuse_output = maybe False (input_mtime <) mb_output_mtime
      no_force_recomp = not $ gopt Opt_ForceRecomp (hsc_dflags hsc_env)

  if no_force_recomp && can_reuse_output
    then logStrLn fpo ("runFnkPpPhase: Reusing " <> output_fn)
    else withFinkelExceptionHandler hsc_env $
         preprocessOrCopy (Just hsc_env) ppo1 input_fn (Just output_fn)

  pure output_fn

runFnkTcPhase :: FnkPluginOptions -> HscEnv -> ModSummary
              -> IO (FrontendResult, Messages GhcMessage)
runFnkTcPhase fpo hsc_env ms0 =
  case ml_hs_file (ms_location ms0) of
    Nothing -> error "runFnkTcPhase: no hs file ..."
    Just hs_file -> do
      let dflags = hsc_dflags hsc_env
          pragma = fpoPragma fpo
          fnk_env = fpoFnkEnv fpo
      ts <- findTargetSourceWithPragma pragma dflags (noLoc hs_file)
      case ts of
        FnkSource {} -> fnkTypecheckAndGetWarnings fnk_env hsc_env ts
        HsSource {}  -> hscTypecheckAndGetWarnings hsc_env ms0
        _            -> error "runFnkTcPhase: other source ..."

-- See: GHC.Driver.Main.hsc_typecheck, which is not exported.
fnkTypecheckAndGetWarnings :: FnkEnv -> HscEnv -> TargetSource
                           -> IO (FrontendResult, Messages GhcMessage)
fnkTypecheckAndGetWarnings fnk_env hsc_env ts = runHsc' hsc_env $ do
  ems <- liftIO $ withFinkelExceptionHandler hsc_env $
         runFnk' (fnkSourceToSummary ts) fnk_env hsc_env
  case ems of
    -- XXX: Invoke hscFrontendHook as done in hscTypecheckAndGetWarings?
    EMS ms1 _ _ | Just pm <- ms_parsed_mod ms1 -> do
      let lcl_hsc_env = hscSetFlags (ms_hspp_opts ms1) hsc_env
      (tc_gbl, _) <- liftIO $ hscTypecheckRename lcl_hsc_env ms1 pm
      pure $ FrontendTypecheck tc_gbl
    _ -> error "runFnkTcPhase: no parsed mod ..."

logStrLn :: FnkPluginOptions -> String -> IO ()
logStrLn fpo msg =
  when (1 < envVerbosity (fpoFnkEnv fpo)) $ hPutStrLn stderr msg

withFinkelExceptionHandler :: ExceptionMonad m => HscEnv -> m a -> m a
withFinkelExceptionHandler hsc_env = handleFinkelException handler
  where
    handler e =
      throwOneError (mkPlainWrappedMsg (hsc_dflags hsc_env) (mb_loc e)
                     (text (displayException e)))
    mb_loc = fromMaybe noSrcSpan . finkelExceptionLoc

#else /* ghc < 9.6.0 */
-- Does nothing in ghc < 9.6.
finkelHooks :: str -> fnk_env -> opts -> hsc_env -> IO hsc_env
finkelHooks _name _fnk_env _cmd_line_opts = pure
#endif
