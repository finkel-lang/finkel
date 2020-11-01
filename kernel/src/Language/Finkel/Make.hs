{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Make mode for Finkel compiler.
module Language.Finkel.Make
  ( make
  , makeFromRequire
  , initSessionForMake
  , buildHsSyn
  ) where

#include "Syntax.h"

-- base
import           Control.Monad                (foldM, unless, void, when)
import           Control.Monad.IO.Class       (MonadIO (..))
import           Data.Foldable                (find)
import           Data.Maybe                   (isJust, mapMaybe)
import           GHC.Fingerprint              (getFileHash)
import           System.IO                    (IOMode (..), withFile)

-- container
import qualified Data.Map                     as Map

-- date
import           Data.Time                    (UTCTime)

-- directory
import           System.Directory             (createDirectoryIfMissing)

-- filepath
import           System.FilePath              (splitExtension, takeBaseName,
                                               takeDirectory, (<.>), (</>))

-- ghc
import           BasicTypes                   (SuccessFlag (..))
import           DriverPhases                 (HscSource (..), Phase (..))
import           DriverPipeline               (compileFile, preprocess,
                                               writeInterfaceOnlyMode)
import           DynFlags                     (DumpFlag (..), DynFlags (..),
                                               GeneralFlag (..), GhcMode (..),
                                               getDynFlags, gopt, gopt_set,
                                               gopt_unset, isObjectTarget,
                                               parseDynamicFilePragma)
import           ErrUtils                     (MsgDoc, dumpIfSet_dyn,
                                               mkPlainErrMsg)
import           Exception                    (handleIO)
import           FastString                   (FastString, fsLit)
import           Finder                       (addHomeModuleToFinder,
                                               cannotFindModule,
                                               findExposedPackageModule,
                                               mkHomeModLocation)
import           GHC                          (getPrintUnqual,
                                               setSessionDynFlags)
import           GHC_Hs                       (HsModule (..))
import           GHC_Hs_Dump                  (BlankSrcSpan (..), showAstData)
import           GHC_Hs_ImpExp                (ImportDecl (..))
import           GhcMake                      (LoadHowMuch (..), load')
import           GhcMonad                     (GhcMonad (..))
import           HeaderInfo                   (getImports)
import           HscStats                     (ppSourceStats)
import           HscTypes                     (FindResult (..),
                                               HomeModInfo (..),
                                               HsParsedModule (..), HscEnv (..),
                                               ModSummary (..), ModuleGraph,
                                               Usage (..), lookupHpt,
                                               ms_mod_name, throwOneError)
import           Module                       (ModLocation (..), Module (..),
                                               ModuleName, mkModuleName,
                                               moduleName, moduleNameSlashes,
                                               moduleNameString, moduleUnitId)
import           Outputable                   (Outputable (..), SDoc, hcat,
                                               nest, printForUser, quotes, text,
                                               vcat, ($$), (<+>))
import           SrcLoc                       (GenLocated (..), Located, getLoc,
                                               mkSrcLoc, mkSrcSpan, unLoc)
import           StringBuffer                 (StringBuffer, hGetStringBuffer)
import           Util                         (getModificationUTCTime,
                                               looksLikeModuleName,
                                               modificationTimeIfExists)

#if MIN_VERSION_ghc(8,10,0)
import           HscTypes                     (ModIface_ (..))
#else
import           HscTypes                     (ModIface (..))
#endif

#if MIN_VERSION_ghc (8,10,0)
import           ErrUtils                     (withTimingD)
#else
import           ErrUtils                     (withTiming)
#endif

#if MIN_VERSION_ghc(8,8,0)
import           HscTypes                     (throwErrors)
#endif

#if MIN_VERSION_ghc(8,4,0)
import           HscTypes                     (extendMG, mgElemModule,
                                               mgModSummaries, mkModuleGraph)

#endif

#if MIN_VERSION_ghc(8,10,0)
import           CliOption                    (Option (..))
#else
import           DynFlags                     (Option (..))
#endif

-- internal
import           Language.Finkel.Builder
import           Language.Finkel.Emit
import           Language.Finkel.Expand
import           Language.Finkel.Fnk
import           Language.Finkel.Form
import           Language.Finkel.Lexer
import           Language.Finkel.Reader
import           Language.Finkel.Syntax
import           Language.Finkel.TargetSource

#include "finkel_kernel_config.h"


-- ---------------------------------------------------------------------
--
-- Exported make interface
--
-- ---------------------------------------------------------------------

-- [Requiring home package module]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- The problem in dependency resolution when requiring home package module is,
-- we need module imports list to make ModSummary, but modules imports could not
-- be obtained unless the source code is macro expanded. However,
-- macro-expansion may use macros from other home package modules, which are not
-- loaded to GHC session yet.
--
-- Currently, compilation is done with recursively calling 'make' function from
-- 'require' macro, during macro-expansion.
--
-- Once these dependency resolution works were tried with custom user hooks in
-- cabal setup script. However, as of Cabal version 1.24.2, building part of
-- some modules from contents of cabal configuration file were not so
-- easy. Though when cabal support multiple libraraies, situation might change.

-- | Finkel variant of @"ghc --make"@.
make
  :: [(Located FilePath, Maybe Phase)]
  -- ^ List of pairs of input file and phase.
  -> Bool -- ^ Force recompilation when 'True'.
  -> Maybe FilePath -- ^ Output file, if any.
  -> Fnk SuccessFlag
make infiles force_recomp mb_output = do

  -- Setting ghcMode as done in ghc's "Main.hs".
  --
  -- Also setting force recompilation field from the argument, since the current
  -- ghc running in OneShot mode instead of CompManager mode until this
  -- point. Some of the dump flags will turn the force recompilation flag
  -- on. Ghc does this switching of recompilation checker in
  -- DynFlags.{setDumpFlag',forceRecompile}.
  dflags0 <- getDynFlags
  let dflags1 = dflags0 { ghcMode = CompManager
                        , outputFile = mb_output }
      dflags2 = if force_recomp
                   then gopt_set dflags1 Opt_ForceRecomp
                   else gopt_unset dflags1 Opt_ForceRecomp
  setDynFlags dflags2
  dflags3 <- getDynFlags

  fnk_env <- getFnkEnv
  dumpDynFlags fnk_env "Language.Finkel.Make.make" dflags3

  -- Decide the kind of sources of the inputs, inputs arguments could be file
  -- paths, or module names.
  targets <- mapM findTargetUnit infiles

  -- Do the compilation work.
  old_summaries <- fmap (mgModSummaries' . hsc_mod_graph) getSession
  make1 LoadAllTargets old_summaries targets

-- | Calls 'GHC.setSessionDynFlags' to initialize session.
initSessionForMake :: Fnk ()
initSessionForMake = do
  -- Returned list of 'InstalledUnitId's are ignored.
  dflags0 <- getDynFlags
  _preload <- setSessionDynFlags dflags0

  -- Above 'setSessionDynFlags' changes the current 'DynFlags', get the updated
  -- "DynFlags".
  dflags1 <- getDynFlags

  -- Load modules names in FnkEnv to current interactive context.
  fnk_env <- getFnkEnv
  let ctx_modules = envContextModules fnk_env
  unless (null ctx_modules) (setContextModules ctx_modules)

  -- Verbosity level could be specified from environment variable and command
  -- line option.
  debug0 <- getFnkDebug
  let vrbs1 = envVerbosity fnk_env
      vrbs2 = if debug0
                then max 3 vrbs1
                else vrbs1

  -- Updating the debug settings. Also setting the default 'DynFlag' at this
  -- point.
  putFnkEnv (fnk_env { envVerbosity = vrbs2
                     , envDefaultDynFlags = Just dflags1 })

-- | Simple make function returning compiled home module information. Intended
-- to be used in 'require' macro.
makeFromRequire :: Located ModuleName -> Fnk ()
makeFromRequire lmname = do
  fnk_env <- getFnkEnv
  hsc_env <- getSession

  let old_summaries = mgModSummaries' (hsc_mod_graph hsc_env)
      tr = traceMake fnk_env "makeFromRequire"

  tr ["required module:" <+> ppr (unLoc lmname)]
  tu <- emptyTargetUnit <$> findTargetModuleName lmname
  _success_flag <- make1 (LoadUpTo (targetUnitName tu)) old_summaries [tu]

  mgraph <- hsc_mod_graph <$> getSession
  let mod_summaries = mgModSummaries' mgraph
  tr ["summaries:", nvc_or_none mod_summaries]

-- | Run given builder.
buildHsSyn
  :: Builder a -- ^ Builder to use.
  -> [Code]    -- ^ Input codes.
  -> Fnk a
buildHsSyn bldr forms = do
  dflags <- getDynFlags
  case evalBuilder dflags bldr forms of
    Right a                     -> return a
    Left (SyntaxError code msg) -> finkelSrcError code msg


-- ---------------------------------------------------------------------
--
-- Internal of make
--
-- ---------------------------------------------------------------------

-- | Unit for compilation target.
--
-- Simply a 'TargetSource' paired with 'Maybe' 'Phase'.
type TargetUnit = (TargetSource, Maybe Phase)

-- | Make empty 'TargetUnit' from 'TargetSource'
emptyTargetUnit :: TargetSource -> TargetUnit
emptyTargetUnit ts = (ts, Nothing)

-- | Get 'TargetUnit' from pair of module name or file path, and phase.
findTargetUnit :: (Located String, Maybe Phase) -> Fnk TargetUnit
findTargetUnit (lpath,mbp) = (\t -> (t, mbp)) <$> findTargetSource lpath

-- | Get 'ModuleName' from given 'TargetUnit'.
targetUnitName :: TargetUnit -> ModuleName
targetUnitName (ts, _) =
  case ts of
    FnkSource _ mn -> mn
    HsSource _ mn  -> mn
    _              -> mkModuleName "module-name-unknown"

-- | Compile 'TargetUnit' to interface file and object code, and return a list
-- of compiled 'ModSummary'.
--
-- Do macro expansion, conver to 'ModSummary', and then pass to 'GhcMake.load''.
--
make1 :: LoadHowMuch -> [ModSummary] -> [TargetUnit] -> Fnk SuccessFlag
make1 how_much old_summaries targets = do
  fnk_env <- getFnkEnv

  let tr = traceMake fnk_env "make1"
      targets_sdoc = nest 2 (vcat (map ppr targets))
      total = length targets

  tr [ "total:" <+> text (show total)
     , "targets:", targets_sdoc
     , "old summaries:", nvc_or_none old_summaries]

  mss0 <- expandTargets old_summaries targets
  tr ["expanded mod summaries:", nvc_or_none mss0]

  -- Make new ModuleGrpah from expanded summaries, then update the old mod
  -- summaries if the summaries were missing.
  let mgraph0 = updateMG (mkModuleGraph' mss0) old_summaries
      messager = envMessager fnk_env
  tr ["merged module mgraph:", nvc_or_none (mgModSummaries' mgraph0)]

  -- Pass the merged ModuleGraph to the "load'" function, delegate the hard
  -- works to it.
  success_flag <- load' how_much (Just messager) mgraph0
  tr ["done:", targets_sdoc]

  return success_flag

updateMG :: ModuleGraph -> [ModSummary] -> ModuleGraph
updateMG = foldr f
  where
    f ms mg = if mgElemModule' mg (ms_mod ms)
                 then mg
                 else extendMG' mg ms

-- | Make a list of 'ModSummary' from the given 'TargetUnit's.
--
-- Purpose is similar to the 'downsweep' function, but does less work (e.g.,
-- does not detect circular module dependencies).
expandTargets
  :: [ModSummary]
  -- ^ List of old 'ModSummary'.
  -> [TargetUnit]
  -- ^ List of 'TargetUnit' to compile.
  -> Fnk [ModSummary]
  -- ^ Expanded module summaries.
expandTargets old_summaries tus_to_compile = timeIt label $ do
  hsc_env <- getSession
  fnk_env <- getFnkEnv
  go [] fnk_env hsc_env tus_to_compile
  where
    label = "expandTargets [Finkel]"
    expandOne fnk_env hsc_env (acc0, more_tus) tu = do
       if acc0 `has_seen` tu
          then
            -- Earlier modules already added this TargetUnit, skipping
            -- the expansion.
            return (acc0, more_tus)
          else do
            -- Expand this TargetUnit, then add imported home modules if not
            -- added yet.
            mb_es <- maybeExpandedSummary old_summaries tu
            case mb_es of
              Nothing -> return (acc0, more_tus)
              Just (ms, reqs) -> do
                let limps = not_yet_ready acc0 more_tus ms
                    new_reqs = reqs `not_in` acc0
                    dflags = hsc_dflags hsc_env

                    -- Adding required ModSummary to the accumulator when using
                    -- interpreter, since it could be reused.
                    acc1 = if isInterpreted dflags
                             then ms:new_reqs ++ acc0
                             else ms:acc0

                not_compiled <- filterNotCompiled fnk_env hsc_env limps
                return (acc1, not_compiled ++ more_tus)

    has_seen mss tu =
      targetUnitName tu `elem` map ms_mod_name mss

    not_in rs as =
      let anames = map ms_mod_name as
      in  filter (\r -> ms_mod_name r `notElem` anames) rs

    not_yet_ready mss_so_far more_tus ms =
      let names1 = map targetUnitName tus_to_compile
          names2 = map targetUnitName more_tus
          names3 = map ms_mod_name mss_so_far
      in  foldr (\(_, lmn) acc ->
                   if unLoc lmn `elem` names1 ||
                      unLoc lmn `elem` names2 ||
                      unLoc lmn `elem` names3
                     then acc
                     else lmn:acc)
                []
                (ms_textual_imps ms)

    -- Recursive function to expand all home modules from the 'tus' argument.
    go acc fnk_env hsc_env tus = do
      if null tus
         then return acc
         else do
           let expandOne' = expandOne fnk_env hsc_env
           (next_acc, more_tus) <- foldM expandOne' (acc,[]) tus
           go next_acc fnk_env hsc_env more_tus

-- | Return a list of 'TargetUnit' to compile for given 'ModuleName's.
filterNotCompiled
   :: FnkEnv
   -> HscEnv
   -> [Located ModuleName]
   -> Fnk [TargetUnit]
filterNotCompiled fnk_env hsc_env lmnames = foldM f [] lmnames
  where
    f acc lmname =
      maybe acc (:acc) <$> findNotCompiledImport fnk_env hsc_env lmname

-- | Find not compiled module.
findNotCompiledImport
  :: FnkEnv -- ^ Current fnk environment.
  -> HscEnv       -- ^ Current hsc environment.
  -> Located ModuleName -- ^ The target module to find.
  -> Fnk (Maybe TargetUnit)
findNotCompiledImport fnk_env hsc_env lmname = do
  -- Search files in home package modules sources before searching in external
  -- packages, to support loading home package modules when working with modules
  -- in cabal packages.
  mb_ts <- findTargetModuleNameMaybe lmname
  case mb_ts of
    Just ts -> do
      tr ["Found" <+> ppr mname <+> "at" <+> text (targetSourcePath ts)]
      return $! Just $! emptyTargetUnit ts
    Nothing -> do
      -- Search for import from external packages.
      find_result <- liftIO (findExposedPackageModule hsc_env mname Nothing)
      case find_result of
        Found _ mdl -> do
          tr ["Found" <+> ppr mname <+> "in" <+> ppr (moduleUnitId mdl)]
          return Nothing
        _ -> do
          let dflags = hsc_dflags hsc_env
              doc = cannotFindModule dflags mname find_result
              err = mkPlainErrMsg dflags loc doc
          throwOneError err
  where
    mname = unLoc lmname
    loc = getLoc lmname
    tr = traceMake fnk_env "findNotCompiledImport"

-- | Set 'dumpPrefix' from file path.
setDumpPrefix :: FilePath -> Fnk ()
setDumpPrefix path = do
  dflags0 <- getDynFlags
  let (basename, _suffix) = splitExtension path
      dflags1 = dflags0 {dumpPrefix = Just (basename ++ ".")}
  setDynFlags dflags1

-- | Trace function for this module.
traceMake :: FnkEnv -> MsgDoc -> [MsgDoc] -> Fnk ()
traceMake fnk_env fn_name msgs0 =
  let msgs1 = (hcat [";;; [Language.Finkel.Make.", fn_name, "]:"] : msgs0)
  in  debugWhen fnk_env Fnk_trace_make msgs1

-- | Nested 'vcat' or text @"none"@.
nvc_or_none :: Outputable a => [a] -> SDoc
nvc_or_none xs = nest 2 sdoc
  where
    sdoc =
       if null xs
         then "none"
         else vcat (map ppr xs)

-- ------------------------------------------------------------------------
--
-- For ModSummary
--
-- ------------------------------------------------------------------------

-- See 'GhcMake.{summariseModule,summariseFile}'.
--
-- Seems like 'addDependentFile' method used by Template Haskell is not working
-- well in GHCi (as of ghc 8.8), see:
--
--   https://gitlab.haskell.org/ghc/ghc/-/issues/18330

-- | Returns 'Just' pair of compiled 'ModSummary' and the required home package
-- module 'ModSummary' for 'FnkSource' and 'HsSource', or 'Nothing' for
-- 'OtherSource'.
maybeExpandedSummary
  :: [ModSummary] -> TargetUnit -> Fnk (Maybe (ModSummary, [ModSummary]))
maybeExpandedSummary old_summaries tu@(tsource,_) = do
  hsc_env <- getSession
  fnk_env <- getFnkEnv

  let new_summary = do
        tr ["Making new summary for" <+> ppr tsource]
        make_new_summary hsc_env fnk_env
      this_mod_name = targetUnitName tu
      by_mod_name = (== this_mod_name) . ms_mod_name
      force_recomp = gopt Opt_ForceRecomp (hsc_dflags hsc_env)
      src_path = targetSourcePath tsource
      mb_usages = do
        old_hmi <- lookupHpt (hsc_HPT hsc_env) this_mod_name
        return (mi_usages (hm_iface old_hmi))
      tr = traceMake fnk_env "maybeExpandedSummary"

  if force_recomp
     then new_summary
     else case find by_mod_name old_summaries of
       Nothing -> new_summary
       Just ms -> do
         -- Checking whether recompilation is required or not at this point,
         -- since when reompiling, may need to parse the source code to reflect
         -- the changes in macros from home package modules.
         mtime <- liftIO (getModificationUTCTime src_path)
         if ms_hs_date ms < mtime
            then new_summary
            else do
              usages_ok <- maybe (pure False) checkUsages mb_usages
              if not usages_ok
                 then new_summary
                 else do
                  tr ["Reusing old summary for" <+> ppr tsource]

                  -- Check timestamps, update the obj_data, iface_date, and
                  -- hie_date to reflect the changes in file system from last
                  -- compilation. See 'GhcMake.checkSummaryTimestamp' called
                  -- during down sweep, which does similar works.
                  let ms_loc = ms_location ms
                      dflags = hsc_dflags hsc_env

                      -- XXX: The below `obj_allowed' is constantly 'False' at
                      -- the moment, it will be nice to pass this from REPL.
                      obj_allowed = False

                  obj_date <-
                      if isObjectTarget (hscTarget dflags) || obj_allowed
                        then liftIO (getObjTimestamp ms_loc)
                        else return Nothing
                  hi_date <- liftIO (maybeGetIfaceDate dflags ms_loc)
#if MIN_VERSION_ghc(8,8,0)
                  hie_date <-
                    liftIO (modificationTimeIfExists (ml_hie_file ms_loc))
#endif
                  -- XXX: Fill in the list of required ModSummary.
                  return $ Just (ms { ms_obj_date = obj_date
                                    , ms_iface_date = hi_date
#if MIN_VERSION_ghc(8,8,0)
                                    , ms_hie_date = hie_date
#endif
                                    }, [])
  where
    make_new_summary hsc_env fnk_env = do
      mb_tup <- newExpandedSummary tu
      case mb_tup of
        Nothing -> return Nothing
        Just (ms, mb_sp, reqs) -> do
          -- Since the entire compilation work does not use DriverPipeline,
          -- setting the dumpPrefix at this point.
          setDumpPrefix (ms_hspp_file ms)

          -- Dump the module contents as Haskell source when dump option were
          -- set and this is the first time for compiling the target Module.
          when (fopt Fnk_dump_hs fnk_env || isJust (envHsOutDir fnk_env)) $
            case lookupHpt (hsc_HPT hsc_env) (ms_mod_name ms) of
              Nothing -> dumpModSummary mb_sp ms
              Just _  -> return ()

          -- To support -ddump-parsed-ast option.
          dumpParsedAST (ms_hspp_opts ms) ms

          return $ Just (ms, reqs)
    getObjTimestamp loc =
      modificationTimeIfExists (ml_obj_file loc)

-- | Simple function to check whther the 'UsageFile' is up to date.
checkUsages :: [Usage] -> Fnk Bool
-- See: 'MkIface.checkModUsage'.
checkUsages =  go
  where
    go us =
      case us of
        [] -> return True
        u:us' -> do
          ret <- check u
          if ret
             then go us'
             else return False
    check u =
      case u of
        UsageFile {usg_file_path = file
                  ,usg_file_hash = old_hash} ->
          liftIO (handleIO (const (return False))
                           (fmap (== old_hash) (getFileHash file)))
        _ -> return True

-- See: GhcMake.maybeGetIfaceDate
maybeGetIfaceDate :: DynFlags -> ModLocation -> IO (Maybe UTCTime)
maybeGetIfaceDate dflags location =
  if writeInterfaceOnlyMode dflags
     then modificationTimeIfExists (ml_hi_file location)
     else return Nothing

-- | Make a pair of 'ModSummary' and a list of required 'ModSummary' in home
-- package.
newExpandedSummary
  :: TargetUnit -> Fnk (Maybe (ModSummary, Maybe SPState, [ModSummary]))
newExpandedSummary (tsrc, mbphase) =
  case tsrc of
    FnkSource path mn -> Just <$> compileFnkFile path mn
    HsSource path _   -> Just <$> compileHsFile path mbphase
    OtherSource path  -> compileOtherFile path >> return Nothing

-- | Compile Finkel source.
compileFnkFile
  :: FilePath -> ModuleName -> Fnk (ModSummary, Maybe SPState, [ModSummary])
compileFnkFile path modname = do
  contents <- liftIO (hGetStringBuffer path)
  (forms, sp) <- parseSexprs (Just path) contents
  dflags0 <- getDynFlagsFromSPState sp
  fnk_env0 <- getFnkEnv
  let tr = traceMake fnk_env0 "compileFnkFile"
      mname_str = moduleNameString modname
      mname_sdoc = text (mname_str ++ ":")

  -- Compile the form with local DynFlags to support file local pragmas.
  (mdl, reqs) <- withTmpDynFlags dflags0 $
    timeIt ("FinkelModule [" ++ mname_str ++ "]") $ do
      -- Reset current FnkEnv. No need to worry about managing DynFlags, this
      -- action is wrapped with 'withTmpDynFlags' above.
      resetFnkEnv
      mdl <- compileFnkModuleForm forms
      fnk_env1 <- getFnkEnv
      return (mdl, envRequiredHomeModules fnk_env1)

  tr ["reqs in" <+> mname_sdoc <+> ppr (map ms_mod_name reqs)]

  let rreqs = reverse reqs
  ms <- mkModSummary dflags0 path mdl rreqs
  return (ms, Just sp, rreqs)

-- | Compile 'HModule' from given list of 'Code'.
compileFnkModuleForm :: [Code] -> Fnk HModule
compileFnkModuleForm form = do
  expanded <- withExpanderSettings (expands form)
  let colons = replicate 20 ';'
  fnk_env <- getFnkEnv
  debugWhen fnk_env
            Fnk_dump_expand
            [ text ""
            , text colons <+> text "Expanded" <+> text colons
            , text ""
            , vcat (map ppr expanded)
            , text ""]
  buildHsSyn parseModule expanded

-- | Get language extensions in current 'Fnk' from given 'SPState'.
getDynFlagsFromSPState :: SPState -> Fnk DynFlags
getDynFlagsFromSPState sp = do
  dflags0 <- getDynFlags
  -- Adding "-X" to 'String' representation of 'LangExt' data type, as done in
  -- 'HeaderInfo.checkExtension'.
  let mkx = fmap ("-X" ++)
      exts = map mkx (langExts sp)
  (dflags1,_,_) <- parseDynamicFilePragma dflags0 exts
  (dflags2,_,_) <- parseDynamicFilePragma dflags1 (ghcOptions sp)
  return dflags2

resetFnkEnv :: Fnk ()
resetFnkEnv =
  modifyFnkEnv (\fnk_env ->
                   fnk_env { envMacros = envDefaultMacros fnk_env
                           , envRequiredHomeModules = [] })

compileHsFile
  :: FilePath -> Maybe Phase -> Fnk (ModSummary, Maybe SPState, [a])
compileHsFile path mb_phase = do
  -- Not parsing the Haskell source code, it will be parsed by the "load'"
  -- function later.
  hsc_env <- getSession
  (dflags, pp_path) <- liftIO (preprocess' hsc_env (path, mb_phase))
  sbuf <- liftIO (hGetStringBuffer pp_path)
  (simps, timps, L _l mname) <- liftIO (getImports' dflags sbuf pp_path path)
  ms <- mkModSummary' dflags path mname simps timps Nothing (Just sbuf)
  return (ms, Nothing, [])

compileOtherFile :: FilePath -> Fnk ()
compileOtherFile path = do
  hsc_env <- getSession
  fnk_env <- getFnkEnv
  traceMake fnk_env
            "compileOtherFile"
            ["Compiling OtherSource:" <+> text path]
  o_file <- liftIO (compileFile hsc_env StopLn (path, Nothing))
  let dflags0 = hsc_dflags hsc_env
      dflags1 = dflags0 {ldInputs = FileOption "" o_file : ldInputs dflags0}
  void (setSessionDynFlags dflags1)

-- [Avoiding Recompilation]
-- ~~~~~~~~~~~~~~~~~~~~~~~~
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
-- XXX: Currently, dependencies of required home package module are not chased,
-- since the information of required module is stored as a plain file path, not
-- as a module.

-- | Make 'ModSummary'.
mkModSummary
  :: GhcMonad m
  => DynFlags -- ^ File local 'DynFlags'.
  -> FilePath -- ^ The source code path.
  -> HModule  -- ^ Parsed module.
  -> [ModSummary] -- ^ List of required 'ModSummary' in home package.
  -> m ModSummary
mkModSummary dflags file mdl reqs =
  let modName = case hsmodName mdl of
                  Just name -> unLoc name
                  Nothing   -> mkModuleName "Main"
      emptyAnns = (Map.empty, Map.empty)
      r_s_loc = mkSrcLoc (fsLit file) 1 1
      r_s_span = mkSrcSpan r_s_loc r_s_loc

      -- XXX: PackageImports language extension not yet supported.  See
      -- 'HscTypes.ms_home_imps'
      imports = map (\lm -> (Nothing, ideclName (unLoc lm)))
                    (hsmodImports mdl)

      -- Adding file path of the required modules to "hpm_src_files" to
      -- support recompilation.
      --
      -- XXX: Add the dependencies of the required modules. The required modules
      -- may import or require other home package modules.
      req_srcs = mapMaybe (ml_hs_file . ms_location) reqs

      pm = HsParsedModule
        { hpm_module = L r_s_span mdl
        , hpm_src_files = file : req_srcs
        , hpm_annotations = emptyAnns }
  in  mkModSummary' dflags file modName [] imports (Just pm) Nothing

-- | Make 'ModSummary' from source file, module name, and imports.
mkModSummary'
  :: GhcMonad m
  => DynFlags
  -> FilePath
  -> ModuleName
  -> [(Maybe FastString, Located ModuleName)]
  -> [(Maybe FastString, Located ModuleName)]
  -> Maybe HsParsedModule
  -> Maybe StringBuffer
  -> m ModSummary
mkModSummary' dflags file mod_name srcimps txtimps mb_pm mb_buf = do
  -- Throw an exception on module name mismatch.
  assertModuleNameMatch dflags file mb_pm
  hsc_env <- getSession

  let tryGetObjectDate path =
        if isObjectTarget (hscTarget dflags)
           then modificationTimeIfExists path
           else return Nothing

  liftIO
    (do mloc <- mkHomeModLocation dflags mod_name file
        mmod <- addHomeModuleToFinder hsc_env mod_name mloc
        hs_date <- getModificationUTCTime file
        obj_date <- tryGetObjectDate (ml_obj_file mloc)
        iface_date <- maybeGetIfaceDate dflags mloc
#if MIN_VERSION_ghc(8,8,0)
        hie_date <- modificationTimeIfExists (ml_hie_file mloc)
#endif
        return ModSummary { ms_mod = mmod
                          , ms_hsc_src = HsSrcFile
                          , ms_location = mloc
                          , ms_hs_date = hs_date
                          , ms_obj_date = obj_date
                          , ms_iface_date = iface_date
#if MIN_VERSION_ghc(8,8,0)
                          , ms_hie_date = hie_date
#endif
                          , ms_parsed_mod = mb_pm
                          , ms_srcimps = srcimps
                          , ms_textual_imps = txtimps
                          , ms_hspp_file = file
                          , ms_hspp_opts = dflags
                          , ms_hspp_buf = mb_buf })

-- See: "GhcMake.summariseModule"
assertModuleNameMatch
  :: GhcMonad m => DynFlags -> FilePath -> Maybe HsParsedModule -> m ()
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
                   loc = getLoc lsaw
               in  throwOneError (mkPlainErrMsg dflags loc msg)
    _ -> return ()

-- | Dump the module contents of given 'ModSummary'.
dumpModSummary :: Maybe SPState -> ModSummary -> Fnk ()
dumpModSummary mb_sp ms =
  case mb_sp of
    Just sp | Just pm <- ms_parsed_mod ms -> work sp pm
    _                                     -> return ()
  where
    work sp pm = do
      fnk_env <- getFnkEnv
      let hsrc = gen sp pm
          hdr = text (unwords [colons, orig_path, colons])
      debugWhen fnk_env Fnk_dump_hs ["", hdr, "" , hsrc, ""]
      mapM_ (doWrite fnk_env hsrc) (envHsOutDir fnk_env)
    doWrite fnk_env hsrc dir = do
       let out_path = get_out_path dir
           out_dir = takeDirectory out_path
       traceMake fnk_env "dumpModSummary" ["Writing to" <+> text out_path]
       dflags <- getDynFlags
       unqual <- getPrintUnqual
       let emit hdl = printForUser dflags hdl unqual hsrc
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

-- See: "hscParse'" in main/HscMain.hs
dumpParsedAST :: DynFlags -> ModSummary -> Fnk ()
dumpParsedAST dflags ms =
  liftIO
    (case ms_parsed_mod ms of
       Just pm ->
         do let rdr_module = hpm_module pm
            dumpIfSet_dyn dflags Opt_D_dump_parsed "Parser"
                          (ppr rdr_module)
            dumpIfSet_dyn dflags Opt_D_dump_parsed_ast "Parser AST"
                          (txt (showAstData NoBlankSrcSpan rdr_module))
            dumpIfSet_dyn dflags Opt_D_source_stats "Source Statistic"
                          (ppSourceStats False rdr_module)
       Nothing -> return ())
  where
#if MIN_VERSION_ghc(8,4,0)
    txt = id
#else
    txt = text
#endif

-- ------------------------------------------------------------------------
--
-- GHC version compatibility functions
--
-- ------------------------------------------------------------------------

-- ModuleGraph was an alias of [ModSummary] in ghc < 8.4.

extendMG' :: ModuleGraph -> ModSummary -> ModuleGraph
mgElemModule' :: ModuleGraph -> Module -> Bool
mkModuleGraph' :: [ModSummary] -> ModuleGraph
mgModSummaries' :: ModuleGraph -> [ModSummary]

#if MIN_VERSION_ghc(8,4,0)
extendMG' = extendMG
mgElemModule' = mgElemModule
mkModuleGraph' = mkModuleGraph
mgModSummaries' = mgModSummaries
#else
extendMG' = flip (:)
mgElemModule' mg mdl = go mg
  where
    go []       = False
    go (ms:mss) = if ms_mod ms == mdl then True else go mss
mkModuleGraph' = id
mgModSummaries' = id
#endif

-- | Label and wrap the given action with 'withTiming'.
timeIt :: String -> Fnk a -> Fnk a
#if MIN_VERSION_ghc(8,10,0)
timeIt label = withTimingD (text label) (const ())
#else
timeIt label = withTiming getDynFlags (text label) (const ())
#endif

preprocess' :: HscEnv -> (FilePath, Maybe Phase) -> IO (DynFlags, FilePath)

getImports' :: DynFlags -> StringBuffer -> FilePath -> FilePath
            -> IO ([(Maybe FastString, Located ModuleName)],
                   [(Maybe FastString, Located ModuleName)],
                   Located ModuleName)

#if MIN_VERSION_ghc(8,8,0)
preprocess' hsc_env (path, mb_phase) =
  do et_result <- preprocess hsc_env path Nothing mb_phase
     case et_result of
       Left err   -> throwErrors err
       Right pair -> return pair

getImports' dflags sbuf pp_path path = do
  et_ret <- getImports dflags sbuf pp_path path
  case et_ret of
    Left errs -> throwErrors errs
    Right ret -> return ret
#else
preprocess' = preprocess
getImports' = getImports
#endif
