-- | Module for typecheck.
--
-- Not used in exposed modules yet.
--
module SK.Core.Typecheck
  ( mkModSummary
  , tcHsModule )
   where

-- containers
import qualified Data.Map as Map

-- time
import Data.Time (getCurrentTime)

-- Internal
import SK.Core.GHC

-- | Make 'ModSummary'. 'UnitId' is main unit.
mkModSummary :: GhcMonad m => Maybe FilePath -> HsModule RdrName
             -> m ModSummary
mkModSummary mbfile mdl = do
  let modName = case hsmodName mdl of
                      Just name -> unLoc name
                      Nothing   -> mkModuleName "Main"
      fn = maybe "anonymous" id mbfile
      mmod = mkModule mainUnitId modName
      prelude = noLoc (mkModuleName "Prelude")
  flags <- getSessionDynFlags
  mloc <- liftIO (mkHomeModLocation flags modName fn)
  timestamp <- liftIO (maybe getCurrentTime
                             getModificationUTCTime
                             mbfile)
  let imports = map importedName (hsmodImports mdl)
      importedName lm = ideclName (unLoc lm)
      imported = map (\x -> (Nothing, x)) imports
  -- XXX: Have not tested with complex module importing modules from
  -- non-standard packages.
  return ModSummary { ms_mod = mmod
                    , ms_hsc_src = HsSrcFile
                    , ms_location = mloc
                    , ms_hs_date = timestamp
                    , ms_obj_date = Nothing
                    , ms_iface_date = Nothing
                    , ms_srcimps = []
                    , ms_textual_imps = (Nothing, prelude) : imported
                    , ms_hspp_file = fn
                    , ms_hspp_opts = flags
                    , ms_hspp_buf = Nothing }


-- | Action to type check module.
--
-- Error location are derived from 'HsModule', locations match
-- precisely with S-expression source code and helpful.
---
tcHsModule :: GhcMonad m
           => Maybe FilePath -- ^ Source of the module.
           -> Bool -- ^ True to generate files, otherwise False.
           -> HsModule RdrName -- ^ Module to typecheck.
           -> m TypecheckedModule
tcHsModule mbfile genFile mdl = do
  let modName = case hsmodName mdl of
         Nothing -> error "no module name"
         Just name -> unLoc name
      fn = maybe "anon" id mbfile
      langExts = languageExtensions (Just Haskell2010)
  preflags <- getSessionDynFlags
  -- XXX: Does not take care of user specified DynFlags settings.
  let preflags' =
       if genFile
          then preflags {hscTarget = HscAsm, ghcLink = LinkBinary}
          else preflags {hscTarget = HscNothing, ghcLink = NoLink}
  _ <- setSessionDynFlags (foldl xopt_set preflags' langExts)
  flags <- getSessionDynFlags
  timestamp <- liftIO (maybe getCurrentTime
                             getModificationUTCTime
                             mbfile)
  mloc <- liftIO (mkHomeModLocation flags modName fn)
  ms <- mkModSummary mbfile mdl
  let unitId = mainUnitId
      mmod = mkModule unitId modName
      ann = (Map.empty, Map.empty)
      r_s_loc = mkSrcLoc (fsLit fn) 1 1
      r_s_span = mkSrcSpan r_s_loc r_s_loc
      pm = ParsedModule { pm_mod_summary = ms
                        , pm_parsed_source = L r_s_span mdl
                        , pm_extra_src_files = [fn]
                        , pm_annotations = ann }
  typecheckModule pm
