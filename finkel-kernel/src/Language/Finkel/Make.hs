{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Make mode for Finkel compiler.
module Language.Finkel.Make
  (
    -- * Make functions
    initSessionForMake
  , make
  , makeFromRequire

    -- * Syntax builder utility
  , buildHsSyn

    -- * Target unit utilities
  , TargetUnit
  , TargetSource(..)
  , findTargetModuleName
  , findTargetModuleNameMaybe
  , findTargetSource
  , findTargetSourceMaybe
  , asModuleName
  , isFnkFile
  , isHsFile
  ) where

#include "ghc_modules.h"


-- base
import Control.Monad                     (foldM, unless, void, when)
import Control.Monad.IO.Class            (MonadIO (..))
import Data.Foldable                     (find)
import Data.Maybe                        (isJust)

-- filepath
import System.FilePath                   (splitExtension)

-- ghc
import GHC                               (setSessionDynFlags)
import GHC_Driver_Finder                 (cannotFindModule,
                                          findExposedPackageModule)
import GHC_Driver_Make                   (LoadHowMuch (..), load')
import GHC_Driver_Monad                  (GhcMonad (..))
import GHC_Driver_Phases                 (Phase (..))
import GHC_Driver_Session                (DynFlags (..), GeneralFlag (..),
                                          GhcMode (..), HasDynFlags (..), gopt,
                                          gopt_set, gopt_unset, isObjectTarget)
import GHC_Driver_Types                  (FindResult (..), HscEnv (..),
                                          ModSummary (..), lookupHpt,
                                          ms_mod_name, throwOneError)
import GHC_Types_Basic                   (SuccessFlag (..))
import GHC_Types_SrcLoc                  (Located, getLoc, unLoc)
import GHC_Unit_Module                   (ModuleName)
import GHC_Utils_Error                   (mkPlainErrMsg)
import GHC_Utils_Misc                    (getModificationUTCTime)
import GHC_Utils_Outputable              (Outputable (..), brackets, nest, text,
                                          vcat, (<+>))

#if MIN_VERSION_ghc(9,0,0)
import GHC_Unit_Types                    (moduleUnit)
#else
import GHC_Unit_Module                   (Module (..), moduleUnitId)
#endif

#if MIN_VERSION_ghc(8,6,0)
import GHC_Driver_Types                  (runHsc)
import GHC_Plugins                       (Plugin (..), withPlugins)
import GHC_Runtime_Loader                (initializePlugins)
#endif

-- internal
import Language.Finkel.Expand
import Language.Finkel.Fnk
import Language.Finkel.Make.Recompile
import Language.Finkel.Make.Summary
import Language.Finkel.Make.TargetSource
import Language.Finkel.Make.Trace


-- ---------------------------------------------------------------------
--
-- The make function
--
-- ---------------------------------------------------------------------

-- Note [Requiring home package module]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
  -- Also setting the force recompilation field from the argument, since the
  -- current ghc may running in OneShot mode instead of CompManager mode until
  -- this point. Some of the dump flags will turn the force recompilation flag
  -- on. Ghc does this in DynFlags.{setDumpFlag',forceRecompile}.
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
  targets <- mapM (findTargetUnit dflags3) infiles

  -- Do the compilation work.
  old_summaries <- fmap (mgModSummaries' . hsc_mod_graph) getSession
  make1 LoadAllTargets old_summaries targets

-- | Calls 'GHC.setSessionDynFlags' and do some works to initialize session.
initSessionForMake :: Fnk ()
initSessionForMake = do
  dflags0 <- getDynFlags

#if MIN_VERSION_ghc(8,6,0)
  -- Initializing the DynFlags for plugin at this point, to avoid repeated calls
  -- of "initializePlugins" before applying plugin action "parsedResultAction".
  -- The 'setSessionDynFlags' changes the current 'DynFlags', so getting the
  -- updated "DynFlags". Returned list of 'InstalledUnitId's are ignored.
  _preload0 <- setSessionDynFlags dflags0
  hsc_env <- getSession
  let dflags0' = hsc_dflags hsc_env
  dflags1 <- liftIO $! initializePlugins hsc_env dflags0'
#else
  let dflags1 = dflags0
#endif

  -- Mangle the function name in "mainFunIs" field, to support mangled name,
  -- e.g. to support "foo-bar-buzz" instead of "foo_bar_buzz".
  let updateMainFunIs = maybe Nothing (Just . mangle)
      mangle = map (\c -> if c == '-' then '_' else c)
      dflags2 = dflags1 { mainFunIs = updateMainFunIs (mainFunIs dflags1) }

  -- ... And setting and getting the DynFlags again.
  _preload1 <- setSessionDynFlags dflags2
  dflags3 <- getDynFlags

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
                     , envDefaultDynFlags = Just dflags3 })

-- | Simple make function returning compiled home module information. Intended
-- to be used in 'require' macro.
makeFromRequire :: Located ModuleName -> Fnk ()
makeFromRequire lmname = do
  fnk_env <- getFnkEnv
  hsc_env <- getSession

  let old_summaries = mgModSummaries' (hsc_mod_graph hsc_env)
      tr = traceMake fnk_env "makeFromRequire"
      dflags = hsc_dflags hsc_env

  tr ["required module:" <+> ppr (unLoc lmname)]
  tu <- emptyTargetUnit <$> findTargetModuleName dflags lmname
  _success_flag <- make1 (LoadUpTo (targetUnitName tu)) old_summaries [tu]

  mgraph <- hsc_mod_graph <$> getSession
  let mod_summaries = mgModSummaries' mgraph
  tr ["summaries:", nvc_or_none mod_summaries]


-- ---------------------------------------------------------------------
--
-- Internal of make
--
-- ---------------------------------------------------------------------

-- | Compile 'TargetUnit' to interface file and object code, and return a list
-- of compiled 'ModSummary'.
--
-- This function does macro expansion, conver 'TargetUnit' to 'ModSummary', and
-- pass the results to 'GhcMake.load''.
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

  hsc_env <- getSession
  (mss0, options) <- summariseTargets hsc_env old_summaries targets
  unless (null options) (updateFlagOptions options)

  -- Make new ModuleGrpah from expanded summaries, then update the old mod
  -- summaries if the summaries were missing.
  let mgraph0 = foldr updateMG (mkModuleGraph' mss0) old_summaries
      updateMG ms mg = if mgElemModule' mg (ms_mod ms)
                          then mg
                          else extendMG' mg ms
      messager = envMessager fnk_env
  tr ["new summaries:", nvc_or_none (mgModSummaries' mgraph0)]

  -- Pass the merged ModuleGraph to the "load'" function, delegate the hard
  -- works to it.
  success_flag <- load' how_much (Just messager) mgraph0
  tr ["done:", targets_sdoc]

  return success_flag

updateFlagOptions :: [Option] -> Fnk ()
updateFlagOptions options = do
  hsc_env <- getSession
  let dflags0 = hsc_dflags hsc_env
      dflags1 = dflags0 {ldInputs = options ++ ldInputs dflags0}
  void (setSessionDynFlags dflags1)


-- ------------------------------------------------------------------------
--
-- For summarising TargetUnit
--
-- ------------------------------------------------------------------------

-- See 'GhcMake.{summariseModule,summariseFile}'.
--
-- Seems like 'addDependentFile' method used by Template Haskell is not working
-- well in GHCi (as of ghc 8.8), see:
--
--   https://gitlab.haskell.org/ghc/ghc/-/issues/18330

-- | Newtype to summaries list of 'TargetUnit'.
newtype MakeM a = MakeM {unMakeM :: MkSt -> Fnk (a, MkSt)}

instance Functor MakeM where
  fmap f (MakeM k) = MakeM (\st -> fmap (\(a,st') -> (f a, st')) (k st))
  {-# INLINE fmap #-}

instance Applicative MakeM where
  pure a = MakeM (\st -> pure (a, st))
  {-# INLINE pure #-}
  f <*> m = do {g <- f; a <- m; pure (g a)}
  {-# INLINE (<*>) #-}

instance Monad MakeM where
  MakeM m >>= k = MakeM (\s0 -> m s0 >>= \(a,s1) -> unMakeM (k a) s1)
  {-# INLINE (>>=) #-}

instance MonadIO MakeM where
  liftIO io = MakeM (\s -> liftIO io >>= \a -> pure (a, s))
  {-# INLINE liftIO #-}

-- | State for 'MakeM'.
data MkSt = MkSt
  { -- | Resulting list of 'ModSummary'.
    mks_summarised    :: ![ModSummary]
    -- | Resulting list of 'Option'.
  , mks_flag_options  :: ![Option]
    -- | List of 'TargetUnit' to compile.
  , mks_to_summarise  :: ![TargetUnit]

    -- | Old ModSummary from last run, if any.
  , mks_old_summaries :: ![ModSummary]
  }

getMkSt :: MakeM MkSt
getMkSt = MakeM (\s -> pure (s,s))
{-# INLINABLE getMkSt #-}

putMkSt :: MkSt -> MakeM ()
putMkSt s = MakeM (\_ -> pure ((),s))
{-# INLINABLE putMkSt #-}

toMakeM :: Fnk a -> MakeM a
toMakeM fnk = MakeM (\st -> fnk >>= \a -> pure (a,st))
{-# INLINABLE toMakeM #-}

-- | Make a list of 'ModSummary' and 'Option' from the given 'TargetUnit's.
--
-- Purpose is similar to the 'downsweep' function, but does less work (e.g.,
-- does not detect circular module dependencies).
summariseTargets
  :: HscEnv
  -- ^ Current session.
  -> [ModSummary]
  -- ^ List of old 'ModSummary'.
  -> [TargetUnit]
  -- ^ List of 'TargetUnit' to compile.
  -> Fnk ([ModSummary], [Option])
  -- ^ A pair of list of 'ModSummary' and list of file option.
summariseTargets hsc_env old_summaries tus_to_summarise =
  withTiming' "summariseTargets [Finkel]" $ do
    fnk_env <- getFnkEnv
    let mks0 = MkSt { mks_summarised = []
                    , mks_flag_options = []
                    , mks_to_summarise = tus_to_summarise
                    , mks_old_summaries = old_summaries }
        rs0 = emptyRecompState hsc_env
    (_, mks1) <- unMakeM (summariseAll fnk_env hsc_env rs0) mks0
    return (mks_summarised mks1, reverse (mks_flag_options mks1))

-- | 'MakeM' action to summarise all 'TargetUnit's.
summariseAll :: FnkEnv -> HscEnv -> RecompState -> MakeM RecompState
summariseAll fnk_env hsc_env rs0 = go rs0
  where
    -- When compiling object codes, macro expander will update HomePackageTable
    -- to check old interface read from file. Recursively passing the
    -- RecompState to use the updated HomePackageTable.
    go rs = do
      s0 <- getMkSt
      case mks_to_summarise s0 of
        []   -> return rs
        t:ts -> do
          putMkSt (s0 {mks_to_summarise = ts})
          summariseOne fnk_env hsc_env t rs >>= go

-- | Summarise one 'TargetUnit'.
summariseOne
  :: FnkEnv -> HscEnv -> TargetUnit -> RecompState -> MakeM RecompState
summariseOne fnk_env hsc_env tu rs0 = do
  mks@MkSt{ mks_summarised = summarised
          , mks_flag_options = flag_options
          , mks_to_summarise = to_summarise } <- getMkSt

  let summarised_names = map ms_mod_name summarised
      names_to_summarise = map targetUnitName to_summarise
      reqs_not_in_mss =
        filter (\r -> ms_mod_name r `notElem` summarised_names)
      not_yet_ready ms =
        foldr (\(_, lmn) acc ->
                 if unLoc lmn `elem` names_to_summarise ||
                    unLoc lmn `elem` summarised_names
                   then acc
                   else lmn:acc)
              []
              (ms_textual_imps ms)

  -- Skip the expansion when earlier modules already saw this TargetUnit.
  if targetUnitName tu `elem` summarised_names
     then return rs0
     else do
       (tsum, rs1) <- makeTargetSummary fnk_env rs0 tu
       case tsum of
         -- Linker option, not a module.
         LdInput fo -> putMkSt (mks {mks_flag_options=fo:flag_options})

         -- Expanded to ModSummary, add imported home modules if not added yet.
         -- Adding the required ModSummary to the accumulator when using
         -- interpreter, since it could be reused.
         EMS ms _ reqs -> do
           not_compiled <- filterNotCompiled fnk_env hsc_env (not_yet_ready ms)
           putMkSt (mks { mks_summarised =
                           if isInterpreted (hsc_dflags hsc_env)
                             then ms:reqs_not_in_mss reqs ++ summarised
                             else ms:summarised
                        , mks_to_summarise = not_compiled ++ to_summarise
                        })
       return rs1

-- | Returns 'Just' pair of compiled 'ModSummary' and the required home package
-- module 'ModSummary' for 'FnkSource' and 'HsSource', or 'Nothing' for
-- 'OtherSource'.
makeTargetSummary
  :: FnkEnv -> RecompState -> TargetUnit -> MakeM (TargetSummary, RecompState)
makeTargetSummary fnk_env rs0 tu@(tsource,_) = do
  -- To maximize recompilation avoidance when compiling object codes, seems like
  -- it is required to first scan all the home package interfaces on file system
  -- to mark outdated "ModSummary"s, and then do the expansion to avoid parsing
  -- the source codes.
  --
  -- Perhaps the "checkOldIface" function is designed to work with topologically
  -- sorted list of "ModSummary", not to be called during macro expansion of
  -- Finkel module source code.  'MkIface.getFromModIface' is calling
  -- 'loadInterface', which is adding empty interface to PIT when loading non
  -- hi-boot interface for home package module. To avoid loading dummy
  -- interfaces, recompilation checks done in "RecompM" is reading interfaces
  -- and updating HPT before invoking 'checkOldIface'.

  old_summaries <- mks_old_summaries <$> getMkSt

  let tr = traceMake' dflags fnk_env "makeTargetSummary"
      hsc_env = rs_hsc_env rs0
      dflags = hsc_dflags hsc_env
      this_mod_name = targetUnitName tu
      by_mod_name = (== this_mod_name) . ms_mod_name

      update_summary obj_allowed rs ms0 = do
        ms1 <- updateSummaryTimestamps dflags obj_allowed ms0
        return (plainEMS ms1, rs)

      new_summary rs why = do
        tr ["Making new summary for" <+> ppr this_mod_name <+> brackets why]
        tsum  <- makeNewSummary fnk_env hsc_env tu
        return (tsum, rs)

      -- XXX: In below, `obj_allowed' argument is constantly 'False' at the
      -- moment, it will be nice to pass this arg from REPL.
      reuse_summary rs ms0 = do
        tr ["Reusing old summary for" <+> ppr this_mod_name]
        update_summary False rs ms0

      reuse_iface rs ms0 = do
        tr ["Reusing iface file for" <+> ppr this_mod_name]
        update_summary True rs ms0

  if gopt Opt_ForceRecomp dflags
     then new_summary rs0 "force recomp"
     else case find by_mod_name old_summaries of
       -- Old summaries did not contain this module, checking whether the
       -- interface file and object code file are reusable when compiling to
       -- object code.
       Nothing ->
         if not (isObjectTarget (hscTarget dflags))
            then new_summary rs0 "non object target"
            else do
              (et_ms, rs1) <- runRecompilationCheck fnk_env rs0 tu
              case et_ms of
                Left why -> new_summary rs1 (text why)
                Right ms -> reuse_iface rs1 ms

       -- Checking whether recompilation is required or not at this point, since
       -- when reompiling, may need to parse the source code to reflect the
       -- changes in macros from home package modules.
       Just ms -> do
         mtime <- liftIO (getModificationUTCTime (targetSourcePath tsource))
         if ms_hs_date ms < mtime
            then new_summary rs0 "source code is new"
            else do
              summary_ok <- checkModSummary hsc_env ms
              if not summary_ok
                 then new_summary rs0 "out of date usages"
                 else reuse_summary rs0 ms

-- | Make new 'ModSummary' with required home package modules and.
makeNewSummary :: FnkEnv -> HscEnv -> TargetUnit -> MakeM TargetSummary
makeNewSummary fnk_env hsc_env tu = toMakeM $ do
  tsum <- summariseTargetUnit tu
  case tsum of
    LdInput _option -> return tsum
    EMS ms0 mb_sp reqs -> do
      dumpDynFlags fnk_env "makeNewSummary" (ms_hspp_opts ms0)
      -- Since the entire compilation work does not use DriverPipeline,
      -- setting the dumpPrefix at this point.
      setDumpPrefix (ms_hspp_file ms0)

      -- Dump the module contents as Haskell source when dump option were
      -- set and this is the first time for compiling the target Module.
      when (fopt Fnk_dump_hs fnk_env || isJust (envHsOutDir fnk_env)) $
        case lookupHpt (hsc_HPT hsc_env) (ms_mod_name ms0) of
          Nothing -> dumpModSummary fnk_env hsc_env mb_sp ms0
          Just _  -> return ()

      -- To support -ddump-parsed-ast option.
      dumpParsedAST (ms_hspp_opts ms0) ms0

#if MIN_VERSION_ghc(8,6,0)
      -- To support parsedResultAction in plugin. See "HscMain.hscParse'"
      ms1 <- case ms_parsed_mod ms0 of
        Nothing -> return ms0
        Just pm -> do
          let do_action p opts = parsedResultAction p opts ms0
              dflags0 = hsc_dflags hsc_env
              dflags1 = adjustIncludePaths dflags0 ms0
              act = withPlugins dflags1 do_action pm
              hsc_env' = hsc_env {hsc_dflags = dflags1}
          new_pm <- liftIO (runHsc hsc_env' act)
          return $! ms0 {ms_parsed_mod = Just new_pm}
#else
      -- Ghc does not support parsedResultAction.
      let ms1 = ms0
#endif
      return $! EMS ms1 Nothing reqs

-- | Set 'dumpPrefix' from file path.
setDumpPrefix :: GhcMonad m => FilePath -> m ()
setDumpPrefix path = do
  dflags0 <- getDynFlags
  let (basename, _suffix) = splitExtension path
      dflags1 = dflags0 {dumpPrefix = Just (basename ++ ".")}
  setDynFlags dflags1
{-# INLINABLE setDumpPrefix #-}

-- | Run the recompilation check.
runRecompilationCheck
  :: FnkEnv -> RecompState -> TargetUnit
  -> MakeM (Either String ModSummary, RecompState)
runRecompilationCheck fnk_env rs tu =
  toMakeM (unRecompM (checkRecompileRequired fnk_env tu) rs)
{-# INLINABLE runRecompilationCheck #-}

-- | Return a list of 'TargetUnit' to compile for given 'ModuleName's.
filterNotCompiled
  :: MonadIO m => FnkEnv -> HscEnv -> [Located ModuleName] -> m [TargetUnit]
filterNotCompiled fnk_env hsc_env = foldM find_not_compiled []
  where
    dflags = hsc_dflags hsc_env
    tr = traceMake' dflags fnk_env "findNotCompiledImport"
    find_not_compiled acc lmname = do
      let mname = unLoc lmname
      mb_ts <- findTargetModuleNameMaybe dflags lmname
      case mb_ts of
        Just ts -> do
          tr ["Found" <+> ppr mname <+> "at" <+> text (targetSourcePath ts)]
          return $! (emptyTargetUnit ts : acc)
        Nothing -> do
          fr <- liftIO (findExposedPackageModule hsc_env mname Nothing)
          case fr of
            Found _ mdl -> do
#if MIN_VERSION_ghc(9,0,0)
              let mod_unit = moduleUnit mdl
#else
              let mod_unit = moduleUnitId mdl
#endif
              tr ["Found" <+> ppr mname <+> "in" <+> ppr mod_unit]
              return acc
            _ -> do
              let doc = cannotFindModule dflags mname fr
                  err = mkPlainErrMsg dflags (getLoc lmname) doc
              throwOneError err
