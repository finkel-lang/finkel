-- | Module for typecheck.
--
-- Not used in exposed modules yet.
--
module SK.Core.Typecheck where

import FastString
import DynFlags
import ErrUtils
import GHC
import HscTypes
-- import Module
import Outputable
-- import Util
import MonadUtils

import qualified Data.Map as Map

-- import SK.Core.GHC


-- Action for type checking module.
--
-- Error location are derived from 'HsModule', locations match
-- precisely with S-expression source code and helpful.
---
tcHsModule :: GhcMonad m => HsModule RdrName
           -> m (Either String TypecheckedModule)
tcHsModule mdl = go
  where
    go = do
      let -- modName = case hsmodName mdl of
          --    Nothing -> error "no module name"
          --    Just name -> unLoc name
          fn = "data/sexpr/input10.hs"
          langExts = languageExtensions (Just Haskell2010)
      flags <- getSessionDynFlags
      _ <- setSessionDynFlags (foldl xopt_set flags langExts)

      -- XXX: Manual creation of ModSummary value is not going
      -- well. Using dummy target to get dummy ModSummary.
      target <- guessTarget "test/data/0001-hello.hs" Nothing
      setTargets [target]
      _ <- load LoadAllTargets
      modSum <- getModSummary (mkModuleName "Main")
      liftIO (putStrLn (showSDoc flags (ppr modSum)))

      -- timestamp <- liftIO (getModificationUTCTime fn)
      let -- unitId = stringToUnitId "main"
          -- unitId = mainUnitId
          -- mmod = mkModule unitId modName
          -- mloc = ModLocation { ml_hs_file = Nothing
          --                    , ml_hi_file = "/tmp/foo.hi"
          --                    , ml_obj_file = "/tmp/foo.o" }
          -- prelude = L r_s_span (mkModuleName "Prelude")
          -- ms = ModSummary { ms_mod = mmod
          --                 , ms_hsc_src = HsSrcFile
          --                 , ms_location = mloc
          --                 , ms_hs_date = timestamp
          --                 , ms_obj_date = Nothing
          --                 , ms_iface_date = Nothing
          --                 , ms_srcimps = []
          --                 , ms_textual_imps = [(Nothing, prelude)]
          --                 , ms_hspp_file = fn
          --                 , ms_hspp_opts = flags
          --                 , ms_hspp_buf = Nothing }
          ann = (Map.empty, Map.empty)
          r_s_loc = mkSrcLoc (fsLit fn) 1 1
          r_s_span = mkSrcSpan r_s_loc r_s_loc
          pm = ParsedModule { pm_mod_summary = modSum
                            , pm_parsed_source = L r_s_span mdl
                            , pm_extra_src_files = [fn]
                            , pm_annotations = ann }
      handleSourceError
        (\se -> return (Left (unlines
                                (map (showSDoc flags)
                                     (pprErrMsgBagWithLoc
                                        (srcErrorMessages se))))))
        (fmap Right (typecheckModule pm))
