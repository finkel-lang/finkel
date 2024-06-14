{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
-- | Internal module for 'ModSummary'.
module Language.Finkel.Make.Summary
  ( -- * Target summary
    TargetSummary(..)
  , plainEMS

    -- * ModSummary helpers
  , summariseTargetUnit
  , mkModSummaryForRecompile
  , updateSummaryTimestamps
  , compileFnkFile
  , dumpParsedAST
  , dumpModSummary

    -- * Builder helpers
  , buildHsSyn

    -- * GHC version compatibility
  , mkModuleGraph'
  , mgModSummaries'
  , mgElemModule'
  , extendMG'
  , withTiming'
  , isObjectBackend

    -- * Re-export
  , Option(..)
  ) where

#include "ghc_modules.h"

-- base
import           Control.Monad.IO.Class            (MonadIO (..))
import           Data.Foldable                     (find)
import           Data.List                         (nub)
import           System.IO                         (IOMode (..), withFile)

#if !MIN_VERSION_base(4,20,0)
import           Data.List                         (foldl')
#endif

-- container
#if !MIN_VERSION_ghc(9,2,0)
import qualified Data.Map                          as Map
#endif

-- date
import           Data.Time                         (UTCTime)

-- directory
import           System.Directory                  (createDirectoryIfMissing)

-- filepath
import           System.FilePath                   (takeBaseName, takeDirectory,
                                                    (<.>), (</>))

-- ghc
import           GHC_Data_EnumSet                  (toList)
import           GHC_Data_FastString               (fsLit)
import           GHC_Data_StringBuffer             (StringBuffer,
                                                    hGetStringBuffer)
import           GHC_Driver_Env_Types              (HscEnv (..))
import           GHC_Driver_Monad                  (GhcMonad (..))
import           GHC_Driver_Phases                 (Phase (..))
import           GHC_Driver_Pipeline               (compileFile, preprocess)
import           GHC_Driver_Ppr                    (printForUser)
import           GHC_Driver_Session                (DumpFlag (..),
                                                    DynFlags (..),
                                                    HasDynFlags (..),
                                                    parseDynamicFilePragma)
import           GHC_Hs                            (HsModule (..))
import           GHC_Hs_Dump                       (BlankSrcSpan (..),
                                                    showAstData)
import           GHC_Hs_ImpExp                     (ImportDecl (..))
import           GHC_Hs_Stats                      (ppSourceStats)
import           GHC_Parser_Header                 (getImports)
import           GHC_Types_SourceError             (throwErrors, throwOneError)
import           GHC_Types_SourceFile              (HscSource (..))
import           GHC_Types_SrcLoc                  (GenLocated (..), Located,
                                                    mkSrcLoc, mkSrcSpan, unLoc)
import           GHC_Unit_Finder                   (addHomeModuleToFinder,
                                                    mkHomeModLocation)
import           GHC_Unit_Home_ModInfo             (HomeModInfo (..), lookupHpt)
import           GHC_Unit_Module                   (ModLocation (..), Module,
                                                    ModuleName, mkModuleName,
                                                    moduleName,
                                                    moduleNameSlashes,
                                                    moduleNameString)
import           GHC_Unit_Module_Deps              (Usage (..))
import           GHC_Unit_Module_Graph             (ModuleGraph, mgLookupModule,
                                                    mgModSummaries,
                                                    mkModuleGraph)
import           GHC_Unit_Module_ModIface          (ModIface_ (..))
import           GHC_Unit_Module_ModSummary        (ModSummary (..),
                                                    ms_mod_name)
import           GHC_Utils_CliOption               (Option (..))
import           GHC_Utils_Misc                    (looksLikeModuleName,
                                                    modificationTimeIfExists)
import           GHC_Utils_Outputable              (Outputable (..), SDoc, hcat,
                                                    quotes, text, vcat, ($$),
                                                    (<+>))


#if MIN_VERSION_ghc(9,8,0)
import           GHC.Data.FastString               (unpackFS)
#endif

#if MIN_VERSION_ghc(9,6,0)
import           GHC.Runtime.Context               (icNamePprCtx)
#else
import           GHC_Runtime_Context               (icPrintUnqual)
#endif

#if MIN_VERSION_ghc(9,6,0)
import           GHC.Driver.Backend                (backendWritesFiles)
#elif MIN_VERSION_ghc(9,2,0)
import           GHC.Driver.Backend                (backendProducesObject)
#else
import           GHC_Driver_Session                (isObjectTarget)
#endif


#if MIN_VERSION_ghc(9,4,0)
import           GHC.Driver.Config.Finder          (initFinderOpts)
import           GHC.Driver.Env                    (hscSetFlags, hsc_HPT)
import           GHC.Driver.Phases                 (StopPhase (..))
import           GHC.Parser.Header                 (mkPrelImports)
import           GHC.Rename.Names                  (renameRawPkgQual)
import           GHC.Types.PkgQual                 (PkgQual (..),
                                                    RawPkgQual (..))
import           GHC.Unit.Module.Graph             (ModuleGraphNode (..))
import qualified GHC.Unit.Module.Graph             as Graph
import           GHC.Unit.Module.ModSummary        (ms_imps)
import           GHC.Utils.Fingerprint             (getFileHash)
#else
import           GHC_Data_FastString               (FastString)
import           GHC_Driver_Pipeline               (writeInterfaceOnlyMode)
import           GHC_Unit_Module_Graph             (mgElemModule)
import           GHC_Utils_Misc                    (getModificationUTCTime)
#endif

#if MIN_VERSION_ghc(9,4,0)
import           GHC.Driver.Config.Parser          (initParserOpts)
import           GHC.Driver.Errors.Types           (GhcMessage (..))
import           GHC.Utils.Logger                  (putDumpFileMaybe)
#elif MIN_VERSION_ghc(9,2,0)
import           GHC.Driver.Config                 (initParserOpts)
import           GHC.Parser.Errors.Ppr             (pprError)
import           GHC.Utils.Logger                  (dumpIfSet_dyn)
#else
import           GHC_Utils_Error                   (dumpIfSet_dyn)
#endif

#if !MIN_VERSION_ghc(9,4,0)
import           GHC_Unit_Module_ModSummary        (ms_home_allimps)
#endif

#if !MIN_VERSION_ghc(9,4,0) && MIN_VERSION_ghc(9,2,0)
import           GHC.Unit.Module.ModSummary        (extendModSummaryNoDeps)
#endif

#if !MIN_VERSION_ghc(9,4,0)
import           GHC_Unit_Module_Graph             (extendMG)
#endif

#if MIN_VERSION_ghc(9,2,0)
import           GHC.Driver.Env                    (hsc_home_unit)
import           GHC.Driver.Session                (xopt)
import           GHC.Hs.Dump                       (BlankEpAnnotations (..))
import           GHC.LanguageExtensions            (Extension (ImplicitPrelude))
#endif

#if MIN_VERSION_ghc(9,2,0)
import           GHC.Hs                            (HsParsedModule (..))
#else
import           GHC_Driver_Types                  (HsParsedModule (..))
#endif

#if MIN_VERSION_ghc(9,2,0)
import           GHC.Unit.Home                     (mkHomeModule)
#elif MIN_VERSION_ghc(9,0,0)
import           GHC_Driver_Session                (homeUnit)
import           GHC_Unit_Module                   (mkModule)
#else
import           GHC_Driver_Session                (thisPackage)
import           GHC_Unit_Module                   (mkModule)
#endif

#if MIN_VERSION_ghc(9,2,0)
import           GHC.Utils.Logger                  (DumpFormat (..))
#elif MIN_VERSION_ghc(9,0,0)
import           GHC_Parser_Annotation             (ApiAnns (..))
import           GHC_Utils_Error                   (DumpFormat (..))
#endif

#if MIN_VERSION_ghc(9,2,0)
import           GHC.Driver.Monad                  (withTimingM)
#else
import           GHC_Utils_Error                   (withTimingD)
#endif

#if MIN_VERSION_ghc(9,0,0)
import           GHC_Utils_Outputable              (Depth (..))
#endif

-- Internal
import           Language.Finkel.Builder
import           Language.Finkel.Emit
import           Language.Finkel.Error
import           Language.Finkel.Expand
import           Language.Finkel.Fnk
import           Language.Finkel.Form
import           Language.Finkel.Lexer
import           Language.Finkel.Make.TargetSource
import           Language.Finkel.Reader
import           Language.Finkel.Syntax
import           Language.Finkel.Syntax.Location


-- | Data type to represent summarised 'TargetSource'.
data TargetSummary
  = -- | Expanded 'ModSummary', from 'FnkSource' and 'HsSource'.
    EMS !ModSummary      -- ^ Summary of itself
        !(Maybe SPState) -- ^ Parsed state for FnkSource
        [ModSummary]     -- ^ Required home package modules for FnkSource

    -- | Link time file option, from 'OtherSource'.
  | LdInput !Option

#if !MIN_VERSION_ghc(9,4,0)
-- PkgQual and RawPkgQual did not exist until ghc 9.4, instead, Maybe FastString
-- were used for both types of package qualified imports.
type PkgQual = Maybe FastString
type RawPkgQual = Maybe FastString
#endif

-- | Make 'EMS' with no 'SPState' and empty list of required 'ModSummary'.
plainEMS :: ModSummary -> TargetSummary
plainEMS ms = EMS ms Nothing []

-- | Make a 'TargetSummary' from given 'TargetUnit'.
summariseTargetUnit :: TargetUnit -> Fnk TargetSummary
summariseTargetUnit (tsrc, mbphase) =
  case tsrc of
    FnkSource path mn -> compileFnkFile path mn
    HsSource path _   -> compileHsFile path mbphase
    OtherSource path  -> compileOtherFile path

-- | Compile Finkel source.
compileFnkFile :: FilePath -> ModuleName -> Fnk TargetSummary
compileFnkFile path modname = do
  fnk_env0 <- getFnkEnv
  hsc_env <- getSession

  contents <- liftIO (hGetStringBuffer path)
  (forms, sp) <- parseSexprs (Just path) contents
  dflags1 <- getDynFlagsFromSPState hsc_env sp

  let tr = traceSummary fnk_env0 "compileFnkFile"
      mname_str = moduleNameString modname
      mname_sdoc = text (mname_str ++ ":")

  tr [ "path:" <+> text path
     , "language:" <+> ppr (language dflags1)
     , "extensionFlags:" <+> ppr (toList (extensionFlags dflags1))]

  -- Compile the form with local DynFlags to support file local pragmas.
  (mdl, reqs) <- withTmpDynFlags dflags1 $
    withTiming' ("FinkelModule [" ++ mname_str ++ "]") $ do
      -- Reset current FnkEnv. No need to worry about managing DynFlags, this
      -- action is wrapped with 'withTmpDynFlags' above.
      resetFnkEnv
      mdl <- compileFnkModuleForm forms
      reqs <- envRequiredHomeModules <$> getFnkEnv
      return (mdl, reqs)

  tr ["reqs in" <+> mname_sdoc <+> ppr (map ms_mod_name reqs)]

  let rreqs = reverse reqs

  -- XXX: Pass the Bool value for ms_ghc_prim_import somehow.
  ms <- mkModSummary hsc_env dflags1 path mdl rreqs

  return $! EMS ms (Just sp) rreqs

-- | Parse the file header LANGUAGE pragmas and update given 'DynFlags'.
parseFnkFileHeader
  :: (HasLogger m, MonadIO m, MonadThrow m) => HscEnv -> FilePath -> m DynFlags
parseFnkFileHeader hsc_env path = do
  contents <- liftIO (hGetStringBuffer path)
  (_, sp) <- parseHeaderPragmas (Just path) contents
  getDynFlagsFromSPState hsc_env sp

-- | Compile 'HModule' from given list of 'Code'.
compileFnkModuleForm :: [Code] -> Fnk HModule
compileFnkModuleForm form = do
  expanded <- withExpanderSettings (expands form)
  let colons = replicate 19 ';'
  fnk_env <- getFnkEnv
  debugWhen fnk_env
            Fnk_dump_expand
            [ text ""
            , text colons <+> text "Expanded" <+> text colons
            , vcat (map ppr expanded)
            , text ""]
  buildHsSyn parseModule expanded

-- | Get language extensions in current 'Fnk' from given 'SPState'.
getDynFlagsFromSPState :: (HasLogger m, MonadIO m) => HscEnv -> SPState -> m DynFlags
getDynFlagsFromSPState hsc_env sp = do
  -- Adding "-X" to 'String' representation of 'LangExt' data type, as done in
  -- 'HeaderInfo.checkExtension'.
  let dflags0 = hsc_dflags hsc_env
      mkx = fmap ("-X" ++)
      exts = map mkx (langExts sp)
  logger <- getLogger
  (dflags1,_,warns1) <- parseDynamicFilePragma dflags0 exts
  printOrThrowDiagnostics' logger dflags1 warns1
  (dflags2,_,warns2) <- parseDynamicFilePragma dflags1 (ghcOptions sp)
  printOrThrowDiagnostics' logger dflags2 warns2
  return dflags2

resetFnkEnv :: Fnk ()
resetFnkEnv =
  modifyFnkEnv (\fnk_env ->
                   fnk_env { envMacros = envDefaultMacros fnk_env
                           , envRequiredHomeModules = [] })

compileHsFile :: FilePath -> Maybe Phase -> Fnk TargetSummary
compileHsFile path mb_phase = do
  -- Not fully parsing the Haskell source code, it will be parsed by the "load'"
  -- function later.
  hsc_env <- getSession
  (dflags, pp_path) <- liftIO (preprocess' hsc_env (path, mb_phase))
  sbuf <- liftIO (hGetStringBuffer pp_path)
  (simps, timps, ghc_prim_import, L _l mname) <-
    liftIO (getImports' dflags sbuf pp_path path)
  let simps' = fmap (rnRPQI hsc_env) simps
      timps' = fmap (rnRPQI hsc_env) timps
  ms <- mkModSummary' hsc_env dflags path mname simps' timps'
                      Nothing (Just sbuf) ghc_prim_import
  return $! plainEMS ms

-- | Rename raw package qualified import. See
-- 'GHC.Driver.Make.getPreprocessedImports', which is not exported.
rnRPQI :: HscEnv -> (RawPkgQual, Located ModuleName) -> (PkgQual, Located ModuleName)
#if MIN_VERSION_ghc(9,4,0)
rnRPQI hsc_env (pk, lmn@(L _ mn)) =
  (renameRawPkgQual (hsc_unit_env hsc_env) mn pk, lmn)
#else
rnRPQI _ = id
#endif
{-# INLINABLE rnRPQI #-}

compileOtherFile :: FilePath -> Fnk TargetSummary
compileOtherFile path = do
  hsc_env <- getSession
  fnk_env <- getFnkEnv
  traceSummary fnk_env
            "compileOtherFile"
            ["Compiling OtherSource:" <+> text path]
#if MIN_VERSION_ghc(9,4,0)
  -- ghc 9.4, introduced StopPhase data type. Before that, Phase data type was
  -- directly used as stopping phase.
  let phase_to_stop = NoStop
#else
  let phase_to_stop = StopLn
#endif
  o_file0 <- liftIO (compileFile hsc_env phase_to_stop (path, Nothing))
#if MIN_VERSION_ghc(9,4,0)
  -- Resulting type of compileFile changed in ghc 9.4 from 'FilePath' to 'Maybe
  -- FilePath'.
  let o_file1 = maybe "" id o_file0
#else
  let o_file1 = o_file0
#endif
  return $! LdInput (FileOption "" o_file1)

-- Note [Avoiding Recompilation]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- See below for details of how GHC avoid recompilation:
--
--   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/compiler/recompilation-avoidance
--
-- To support recompiling the target module when the required modules were
-- changed, checking the file paths of home package modules are stored as
-- "UsageFile" in "mi_usages", via "hpm_src_files" field in "HsParsedModule"
-- used when making "ModSummary" data.
--
-- Currently, dependencies of required home package module are chased with plain
-- file path, since the information of required module is stored as a plain file
-- path, not as a module. This is to avoid compiling required modules as object
-- code, because macro expansions are done with byte code interpreter.

-- | Make 'ModSummary'.
mkModSummary
  :: HscEnv   -- ^ Current session.
  -> DynFlags -- ^ File local 'DynFlags'.
  -> FilePath -- ^ The source code path.
  -> HModule  -- ^ Parsed module.
  -> [ModSummary] -- ^ List of required 'ModSummary' in home package.
  -> Fnk ModSummary
mkModSummary hsc_env dflags file mdl reqs = do
  let mod_name = case hsmodName mdl of
                  Just name -> unLoc name
                  Nothing   -> mkModuleName "Main"
      r_s_loc = mkSrcLoc (fsLit file) 1 1
      r_s_span = mkSrcSpan r_s_loc r_s_loc

      -- XXX: PackageImports language extension not yet supported.  See
      -- 'HscTypes.ms_home_imps'
      rn_idecl = reLoc . ideclName . unLoc
      imports0 = hsmodImports mdl
#if MIN_VERSION_ghc(9,4,0)
      implicit_prelude = xopt ImplicitPrelude dflags
      imports1 = mkPrelImports mod_name r_s_span implicit_prelude imports0
      imports2 = map (\lm -> (NoPkgQual, rn_idecl lm)) (imports1 ++ imports0)
#else
      imports2 = map (\lm -> (Nothing, rn_idecl lm)) imports0
#endif

  -- Adding file path of the required modules and file paths of imported home
  -- package modules to "hpm_src_files" to support recompilation.
  req_srcs <- requiredDependencies reqs

  let pm = HsParsedModule
        { hpm_module = L r_s_span mdl
        , hpm_src_files = req_srcs
#if !MIN_VERSION_ghc(9,2,0)
          -- The hpm_annotations field disappeared in ghc 9.2.
        , hpm_annotations = mkEmptyApiAnns
#endif
        }

  mkModSummary' hsc_env dflags file mod_name [] imports2 (Just pm) Nothing False

#if !MIN_VERSION_ghc(9,2,0)
#  if MIN_VERSION_ghc(9,0,0)
mkEmptyApiAnns :: ApiAnns
mkEmptyApiAnns = ApiAnns { apiAnnItems = Map.empty
                         , apiAnnEofPos = Nothing
                         , apiAnnComments = Map.empty
                         , apiAnnRogueComments = []
                         }
#  else
mkEmptyApiAnns :: (Map.Map a b, Map.Map c d)
mkEmptyApiAnns = (Map.empty, Map.empty)
#  endif
#endif

-- | Make 'ModSummary' for recompilation check done with 'doCheckOldIface'.
mkModSummaryForRecompile :: (HasLogger m, MonadIO m, MonadThrow m)
                         => HscEnv -> TargetUnit -> m ModSummary
mkModSummaryForRecompile hsc_env tu@(tsource, _) = do
  let path = targetSourcePath tsource
      mod_name = targetUnitName tu
  dflags1 <- case tsource of
    FnkSource {} -> parseFnkFileHeader hsc_env path
    _            -> return (hsc_dflags hsc_env)
  mkModSummary' hsc_env dflags1 path mod_name [] [] Nothing Nothing False

-- | Make 'ModSummary' from source file, module name, and imports.
mkModSummary'
  :: MonadIO m
  => HscEnv
  -> DynFlags -- Potentially file local DynFlags
  -> FilePath
  -> ModuleName
  -> [(PkgQual, Located ModuleName)]
  -> [(PkgQual, Located ModuleName)]
  -> Maybe HsParsedModule
  -> Maybe StringBuffer
  -> Bool
  -> m ModSummary
mkModSummary' hsc_env dflags file mod_name srcimps txtimps mb_pm mb_buf
              _ghc_prim_import = do
  -- Throw an exception on module name mismatch.
  assertModuleNameMatch dflags file mb_pm

  let tryGetObjectDate path =
        if isObjectBackend dflags
           then modificationTimeIfExists path
           else return Nothing
#if MIN_VERSION_ghc(9,4,0)
  let mkMLoc df mname path =
        pure (mkHomeModLocation (initFinderOpts df) mname path)
      addHomeMod henv =
        addHomeModuleToFinder (hsc_FC henv) (hsc_home_unit henv)
#else
  let mkMLoc = mkHomeModLocation
      addHomeMod = addHomeModuleToFinder
#endif

  liftIO
    (do mloc <- mkMLoc dflags mod_name file
        mmod <- addHomeMod hsc_env mod_name mloc
        obj_date <- tryGetObjectDate (ml_obj_file mloc)
#if MIN_VERSION_ghc(9,4,0)
        src_hash <- getFileHash file
        dyn_obj_date <- modificationTimeIfExists (ml_dyn_obj_file mloc)
#else
        hs_date <- getModificationUTCTime file
#endif
        iface_date <- maybeGetIfaceDate dflags mloc
        hie_date <- modificationTimeIfExists (ml_hie_file mloc)
        return ModSummary { ms_mod = mmod
                          , ms_hsc_src = HsSrcFile
                          , ms_location = mloc
#if MIN_VERSION_ghc(9,4,0)
                          , ms_hs_hash = src_hash
                          , ms_dyn_obj_date = dyn_obj_date
                          , ms_ghc_prim_import = _ghc_prim_import
#else
                          , ms_hs_date = hs_date
#endif
                          , ms_obj_date = obj_date
                          , ms_iface_date = iface_date
                          , ms_hie_date = hie_date
                          , ms_parsed_mod = mb_pm
                          , ms_srcimps = srcimps
                          , ms_textual_imps = txtimps
                          , ms_hspp_file = file
                          , ms_hspp_opts = dflags
                          , ms_hspp_buf = mb_buf })

-- | Update timestamps of given 'ModSummary'.
updateSummaryTimestamps
  :: MonadIO m => DynFlags -> Bool -> ModSummary -> m ModSummary
updateSummaryTimestamps dflags obj_allowed ms = do
  -- Check timestamps, update the obj_data, iface_date, and hie_date to reflect
  -- the changes in file system from last compilation. See
  -- 'GhcMake.checkSummaryTimestamp' called during down sweep, which does
  -- similar works.
  let ms_loc = ms_location ms
  obj_date <-
      if isObjectBackend dflags || obj_allowed
        then liftIO (modificationTimeIfExists (ml_obj_file ms_loc))
        else return Nothing
  iface_date <- liftIO (maybeGetIfaceDate dflags ms_loc)
  hie_date <- liftIO (modificationTimeIfExists (ml_hie_file ms_loc))
  -- XXX: Fill in the list of required ModSummary.
  return (ms { ms_obj_date = obj_date
             , ms_iface_date = iface_date
             , ms_hie_date = hie_date })

-- See: "GhcMake.summariseModule"
assertModuleNameMatch
  :: MonadIO m => DynFlags -> FilePath -> Maybe HsParsedModule -> m ()
assertModuleNameMatch dflags file mb_pm =
  case mb_pm of
    Just pm | Just lsaw <- hsmodName (unLoc (hpm_module pm))
            , let wanted = asModuleName file
            , let saw = moduleNameString (unLoc lsaw)
            , saw /= "Main"
            , saw /= wanted
            -> let msg = text "File name does not match module"
                         $$ text "Saw:" <+> quotes (text saw)
                         $$ text "Expected:" <+> quotes (text wanted)
                   loc = getLocA lsaw
               in  throwOneError (mkPlainWrappedMsg dflags loc msg)
    _ -> return ()

-- See: GhcMake.maybeGetIfaceDate
maybeGetIfaceDate :: DynFlags -> ModLocation -> IO (Maybe UTCTime)
maybeGetIfaceDate dflags location =
  if writeIface dflags
     then modificationTimeIfExists (ml_hi_file location)
     else return Nothing
  where
#if MIN_VERSION_ghc(9,4,0)
    writeIface = const True
#else
    writeIface = writeInterfaceOnlyMode
#endif

-- | Dump the module contents of given 'ModSummary'.
dumpModSummary
  :: (MonadIO m, HasDynFlags m)
  => FnkEnv -> HscEnv -> Maybe SPState -> ModSummary -> m ()
dumpModSummary fnk_env hsc_env mb_sp ms =
  case mb_sp of
    Just sp | Just pm <- ms_parsed_mod ms -> work sp pm
    _                                     -> return ()
  where
    work sp pm = do
      let hsrc = gen sp pm
          hdr = text (unwords [colons, orig_path, colons])
      debugWhen fnk_env Fnk_dump_hs ["", hdr, "" , hsrc, ""]
      mapM_ (doWrite hsrc) (envHsOutDir fnk_env)
    doWrite hsrc dir = do
       let out_path = get_out_path dir
           out_dir = takeDirectory out_path
       traceSummary fnk_env "dumpModSummary" ["Writing to" <+> text out_path]
       let dflags = hsc_dflags hsc_env
#if MIN_VERSION_ghc(9,6,0)
           unqual = icNamePprCtx (hsc_unit_env hsc_env) (hsc_IC hsc_env)
#elif MIN_VERSION_ghc(9,2,0)
           unqual = icPrintUnqual (hsc_unit_env hsc_env) (hsc_IC hsc_env)
#else
           unqual = icPrintUnqual dflags (hsc_IC hsc_env)
#endif
#if MIN_VERSION_ghc(9,0,0)
           emit hdl = printForUser dflags hdl unqual AllTheWay hsrc
#else
           emit hdl = printForUser dflags hdl unqual hsrc
#endif
       liftIO (do createDirectoryIfMissing True out_dir
                  withFile out_path WriteMode emit)
    get_out_path dir =
      let mname = moduleName (ms_mod ms)
          bname = takeBaseName orig_path
          file_name = if looksLikeModuleName bname
                         then moduleNameSlashes mname
                         else bname
      in  dir </> file_name <.> "hs"
    gen sp pm = toHsSrc sp (Hsrc (unLoc (hpm_module pm)))
    orig_path = ms_hspp_file ms
    colons = replicate 20 ';'

-- See: "hscParse'" in GHC.Driver.Main (or main/HscMain.hs in ghc < 9).
dumpParsedAST :: MonadIO m => HscEnv -> DynFlags -> ModSummary -> m ()
dumpParsedAST _hsc_env dflags ms =
  liftIO
    (case ms_parsed_mod ms of
       Just pm ->
         do let rdr_module = hpm_module pm
            dumpIfSet_dyn_hs dflags Opt_D_dump_parsed "Parser"
                             (ppr rdr_module)
            dumpIfSet_dyn_hs dflags Opt_D_dump_parsed_ast "Parser AST"
                             (show_ast_data NoBlankSrcSpan rdr_module)
            dumpIfSet_dyn_txt dflags Opt_D_source_stats "Source Statistic"
                              (ppSourceStats False rdr_module)
       Nothing -> return ())
  where
#if MIN_VERSION_ghc(9,2,0)
    show_ast_data sp = showAstData sp NoBlankEpAnnotations
#else
    show_ast_data = showAstData
#endif
#if MIN_VERSION_ghc(9,4,0)
    dumpIfSet_dyn_hs = putDumpFileMaybe_for FormatHaskell
    dumpIfSet_dyn_txt = putDumpFileMaybe_for FormatText
    putDumpFileMaybe_for format df flag label sdoc =
      let hsc_env = hscSetFlags df _hsc_env
      in  putDumpFileMaybe (hsc_logger hsc_env) flag label format sdoc
#elif MIN_VERSION_ghc(9,2,0)
    dumpIfSet_dyn_hs = dumpIfSet_dyn_with FormatHaskell
    dumpIfSet_dyn_txt = dumpIfSet_dyn_with FormatText
    dumpIfSet_dyn_with format df flag label sdoc =
      dumpIfSet_dyn (hsc_logger _hsc_env) df flag label format sdoc
#elif MIN_VERSION_ghc(9,0,0)
    dumpIfSet_dyn_hs = dumpIfSet_dyn_with FormatHaskell
    dumpIfSet_dyn_txt = dumpIfSet_dyn_with FormatText
    dumpIfSet_dyn_with format df flag label sdoc =
      dumpIfSet_dyn df flag label format sdoc
#else
    dumpIfSet_dyn_hs = dumpIfSet_dyn
    dumpIfSet_dyn_txt = dumpIfSet_dyn
#endif

-- Note [Chasing dependencies of required home package module]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- When home package module were required, recompilation happen when any of the
-- required module was changed.
--
-- The required modules lives in a dedicated HscEnv (the one stored at
-- `envSessionForExpand' field in 'FnkEnv') which uses bytecode interpreter.
-- Required modules are stored as 'UsageFile' in the 'mi_usages' field of
-- 'ModIface', not as 'UsageHomeModule', because if stored as 'UsageHomeModule',
-- required modules will compiled as object codes, which is not used by the
-- macro expander at the moment.
--
-- To chase dependencies of the required home package modules, the
-- "requiredDependencies" functions temporary switch to the macro expansion
-- session and recursively chases the file paths of imported modules and
-- required modules.

requiredDependencies :: [ModSummary] -> Fnk [FilePath]
requiredDependencies mss = do
  hsc_env0 <- getSession
  let getDeps he = pure $! nub $! foldl' (requiredDependency he) [] mss
  if isExpanding (hsc_dflags hsc_env0)
    then getDeps hsc_env0
    else do
      mb_hsc_env <- envSessionForExpand <$> getFnkEnv
      case mb_hsc_env of
        Just hsc_env -> getDeps hsc_env
        Nothing      -> pure []

requiredDependency :: HscEnv -> [FilePath] -> ModSummary -> [FilePath]
requiredDependency hsc_env = go
  where
    go acc ms =
      case ml_hs_file (ms_location ms) of
        Nothing -> acc
        Just me -> dep_files (me : acc) ms

    dep_files acc ms =
      let mg = hsc_mod_graph hsc_env
          hpt = hsc_HPT hsc_env
          acc1 = find_require_paths hpt acc ms
      in  foldl' (find_import_path mg) acc1 (msHomeAllimps ms)

    find_import_path mg acc mod_name =
      let mdl = mkModuleFromHscEnv hsc_env mod_name
      in  maybe acc (go acc) (mgLookupModule' mg mdl)

    find_require_paths hpt acc ms =
      case lookupHpt hpt (ms_mod_name ms) of
        Nothing  -> acc
        Just hmi -> foldl' req_paths acc (mi_usages (hm_iface hmi))

    req_paths acc usage =
      case usage of
        -- Recursively calling `go' with the ModSummary referred by the usage
        -- file path, but only for Haskell and Finkel source codes.
        --
        -- It is important to track only those "UsageFile"s of source code that
        -- potentially containing macro definitions. Otherwise the compilation
        -- time of modules containing ":require" of home package module was
        -- noticeably slow in ghc 9.4.2.
        UsageFile {usg_file_path = path_fs} | isFnkFile path || isHsFile path ->
          let mb_ms1 = find is_my_path mss
              is_my_path = (Just path ==) . ml_hs_file . ms_location
              mss = mgModSummaries' (hsc_mod_graph hsc_env)
              acc1 = path : acc
          in  maybe acc1 (go acc1) mb_ms1
          where
            path = unpackFSFor908 path_fs
        _ -> acc

#if MIN_VERSION_ghc(9,8,0)
    unpackFSFor908 = unpackFS
#else
    unpackFSFor908 = id
#endif

mkModuleFromHscEnv :: HscEnv -> ModuleName -> Module
mkModuleFromHscEnv hsc_env =
#if MIN_VERSION_ghc(9,2,0)
  mkHomeModule (hsc_home_unit hsc_env)
#elif MIN_VERSION_ghc(9,0,0)
  mkModule (homeUnit (hsc_dflags hsc_env))
#else
  mkModule (thisPackage (hsc_dflags hsc_env))
#endif

-- | Trace function for this module.
traceSummary
  :: (MonadIO m, HasDynFlags m) => FnkEnv -> SDoc -> [SDoc] -> m ()
traceSummary fnk_env name msgs0 =
  let msgs1 = hcat [";;; [Language.Finkel.Make.Summary.", name, "]:"] : msgs0
  in  debugWhen fnk_env Fnk_trace_make msgs1

-- | Run given builder.
buildHsSyn
  :: Builder a -- ^ Builder to use.
  -> [Code]    -- ^ Input codes.
  -> Fnk a
buildHsSyn bldr forms = do
  dflags <- getDynFlags
  qualify <- envQualifyQuotePrimitives <$> getFnkEnv
  case evalBuilder dflags qualify bldr forms of
    Right a                     -> return a
    Left (SyntaxError code msg) -> finkelSrcError code msg


-- ------------------------------------------------------------------------
-- ModuleGraph
-- ------------------------------------------------------------------------

extendMG' :: ModuleGraph -> ModSummary -> ModuleGraph
{-# INLINABLE extendMG' #-}

mgElemModule' :: ModuleGraph -> Module -> Bool
{-# INLINABLE mgElemModule' #-}

mkModuleGraph' :: [ModSummary] -> ModuleGraph
{-# INLINABLE mkModuleGraph' #-}

mgModSummaries' :: ModuleGraph -> [ModSummary]
{-# INLINABLE mgModSummaries' #-}

mgLookupModule' :: ModuleGraph -> Module -> Maybe ModSummary
{-# INLINABLE mgLookupModule' #-}

#if MIN_VERSION_ghc(9,4,0)
extendMG' g ms = Graph.extendMG' g (ModuleNode [] ms)
mgElemModule' mg m = moduleName m `elem` map ms_mod_name (mgModSummaries mg)
mkModuleGraph' = mkModuleGraph . map (\ms -> ModuleNode [] ms)
mgModSummaries' = mgModSummaries
mgLookupModule' = mgLookupModule
#elif MIN_VERSION_ghc(9,2,0)
extendMG' g ms = extendMG g (extendModSummaryNoDeps ms)
mgElemModule' = mgElemModule
mkModuleGraph' = mkModuleGraph . map extendModSummaryNoDeps
mgModSummaries' = mgModSummaries
mgLookupModule' = mgLookupModule
#else
extendMG' = extendMG
mgElemModule' = mgElemModule
mkModuleGraph' = mkModuleGraph
mgModSummaries' = mgModSummaries
mgLookupModule' = mgLookupModule
#endif

-- The `ms_home_allimps' function did not exist until ghc 8.10.x, and removed in
-- ghc 9.4.x.
#if MIN_VERSION_ghc(9,4,0)
-- XXX: Use 'GHC.Unit.Module.ModSummary.home_imps' ?
msHomeAllimps :: ModSummary -> [ModuleName]
msHomeAllimps = map (unLoc . snd) . ms_imps
#else
msHomeAllimps :: ModSummary -> [ModuleName]
msHomeAllimps = ms_home_allimps
#endif

-- ------------------------------------------------------------------------
-- Header parser
-- ------------------------------------------------------------------------

preprocess' :: HscEnv -> (FilePath, Maybe Phase) -> IO (DynFlags, FilePath)
{-# INLINABLE preprocess' #-}

preprocess' hsc_env (path, mb_phase) = do
  et_result <- preprocess hsc_env path Nothing mb_phase
  case et_result of
#  if MIN_VERSION_ghc(9,4,0)
    Left err   -> throwErrors (fmap GhcDriverMessage err)
#  else
    Left err   -> throwErrors err
#  endif
    Right pair -> return pair

getImports' :: DynFlags -> StringBuffer -> FilePath -> FilePath
            -> IO ([(RawPkgQual, Located ModuleName)],
                   [(RawPkgQual, Located ModuleName)],
                   Bool,
                   Located ModuleName)
{-# INLINABLE getImports' #-}

#if MIN_VERSION_ghc(9,4,0)
getImports' dflags sbuf pp_path path = do
  let imp_prelude = xopt ImplicitPrelude dflags
      popts = initParserOpts dflags
  et_ret <- getImports popts imp_prelude sbuf pp_path path
  either (throwErrors . fmap GhcPsMessage) pure et_ret
#elif MIN_VERSION_ghc(9,2,0)
getImports' dflags sbuf pp_path path = do
  let imp_prelude = xopt ImplicitPrelude dflags
      popts = initParserOpts dflags
  et_ret <- getImports popts imp_prelude sbuf pp_path path
  case et_ret of
    Right (simps, timps, lm) -> pure (simps, timps, False, lm)
    Left err                 -> throwErrors (fmap pprError err)
#else
getImports' dflags sbuf pp_path path = do
  et_ret <- getImports dflags sbuf pp_path path
  case et_ret of
    Right (simps, timps, lm) -> pure (simps, timps, False, lm)
    Left err                 -> throwErrors err
#endif

-- ------------------------------------------------------------------------
-- Timing
-- ------------------------------------------------------------------------

-- | Label and wrap the given action with 'withTiming'.
withTiming' :: String -> Fnk a -> Fnk a
#if MIN_VERSION_ghc(9,2,0)
withTiming' label = withTimingM (text label) (const ())
#else
withTiming' label = withTimingD (text label) (const ())
#endif
{-# INLINABLE withTiming' #-}

-- | 'True' if the backend used by given 'DynFlags' produces object code.
isObjectBackend :: DynFlags -> Bool
#if MIN_VERSION_ghc(9,6,0)
isObjectBackend = backendWritesFiles . backend
#elif MIN_VERSION_ghc(9,2,0)
isObjectBackend = backendProducesObject . backend
#else
isObjectBackend = isObjectTarget . hscTarget
#endif
{-# INLINEABLE isObjectBackend #-}
