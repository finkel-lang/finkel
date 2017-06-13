-- | Module for typecheck.
--
-- Not used in exposed modules yet.
--
module SK.Core.Typecheck where

-- containers
import qualified Data.Map as Map

-- time
import Data.Time (getCurrentTime)

-- Internal
import SK.Core.GHC


-- | Action to type check module.
--
-- Error location are derived from 'HsModule', locations match
-- precisely with S-expression source code and helpful.
---
tcHsModule :: GhcMonad m => HsModule RdrName
           -> m (Either String TypecheckedModule)
tcHsModule mdl = do
  let modName = case hsmodName mdl of
         Nothing -> error "no module name"
         Just name -> unLoc name
      fn = "tcHsModule"
      langExts = languageExtensions (Just Haskell2010)
  preflags <- getSessionDynFlags
  _ <- setSessionDynFlags
         (foldl xopt_set
                (preflags { hscTarget = HscNothing
                          , ghcLink = NoLink })
                langExts)
  flags <- getSessionDynFlags
  timestamp <- liftIO getCurrentTime
  let unitId = mainUnitId
      mmod = mkModule unitId modName
      mloc =
        ModLocation { ml_hs_file = Nothing
                    , ml_hi_file = "generated_by_typecheck.hi"
                    , ml_obj_file = "generated_by_typecheck.o" }
      prelude = L r_s_span (mkModuleName "Prelude")
      -- XXX: Have not tested with complex module importing modules
      -- from non-standard packages.
      ms = ModSummary { ms_mod = mmod
                      , ms_hsc_src = HsSrcFile
                      , ms_location = mloc
                      , ms_hs_date = timestamp
                      , ms_obj_date = Nothing
                      , ms_iface_date = Nothing
                      , ms_srcimps = []
                      , ms_textual_imps = [(Nothing, prelude)]
                      , ms_hspp_file = fn
                      , ms_hspp_opts = flags
                      , ms_hspp_buf = Nothing }
      ann = (Map.empty, Map.empty)
      r_s_loc = mkSrcLoc (fsLit fn) 1 1
      r_s_span = mkSrcSpan r_s_loc r_s_loc
      pm = ParsedModule { pm_mod_summary = ms
                        , pm_parsed_source = L r_s_span mdl
                        , pm_extra_src_files = [fn]
                        , pm_annotations = ann }
  handleSourceError
    (\se -> return (Left (unlines
                            (map (showSDoc flags)
                                 (pprErrMsgBagWithLoc
                                    (srcErrorMessages se))))))
    (fmap Right (typecheckModule pm))
