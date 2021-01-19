{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Internal module for 'ModSummary'.
module Language.Finkel.Make.Summary
  ( -- * Target summary
    TargetSummary(..)
  , plainEMS

    -- * ModSummary helpers
  , summariseTargetUnit
  , mkModSummaryForRecompile
  , updateSummaryTimestamps
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

    -- * Re-export
  , Option(..)
  ) where

#include "Syntax.h"

-- base
import           Control.Monad.IO.Class            (MonadIO (..))
import           Data.Foldable                     (find)
import           Data.List                         (foldl', nub)
import           System.IO                         (IOMode (..), withFile)

#if !MIN_VERSION_ghc(8,8,0)
import           Control.Monad.Fail                (MonadFail (..))
#endif

-- container
import qualified Data.Map                          as Map

-- date
import           Data.Time                         (UTCTime)

-- directory
import           System.Directory                  (createDirectoryIfMissing)

-- filepath
import           System.FilePath                   (takeBaseName, takeDirectory,
                                                    (<.>), (</>))

-- ghc
import           DriverPhases                      (HscSource (..), Phase (..))
import           DriverPipeline                    (compileFile, preprocess,
                                                    writeInterfaceOnlyMode)
import           DynFlags                          (DumpFlag (..),
                                                    DynFlags (..),
                                                    HasDynFlags (..),
                                                    isObjectTarget,
                                                    parseDynamicFilePragma,
                                                    thisPackage)
import           ErrUtils                          (MsgDoc, dumpIfSet_dyn,
                                                    mkPlainErrMsg)
import           FastString                        (FastString, fsLit)
import           Finder                            (addHomeModuleToFinder,
                                                    mkHomeModLocation)
import           GHC_Hs                            (HsModule (..))
import           GHC_Hs_Dump                       (BlankSrcSpan (..),
                                                    showAstData)
import           GHC_Hs_ImpExp                     (ImportDecl (..))
import           GhcMonad                          (GhcMonad (..))
import           HeaderInfo                        (getImports)
import           HscStats                          (ppSourceStats)
import           HscTypes                          (HomeModInfo (..),
                                                    HsParsedModule (..),
                                                    HscEnv (..),
                                                    ModSummary (..),
                                                    ModuleGraph, Usage (..),
                                                    icPrintUnqual, lookupHpt,
                                                    ms_mod_name, throwOneError)
import           Module                            (ModLocation (..), Module,
                                                    ModuleName, mkModule,
                                                    mkModuleName, moduleName,
                                                    moduleNameSlashes,
                                                    moduleNameString)
import           Outputable                        (Outputable (..), hcat,
                                                    printForUser, quotes, text,
                                                    vcat, ($$), (<+>))
import           SrcLoc                            (GenLocated (..), Located,
                                                    getLoc, mkSrcLoc, mkSrcSpan,
                                                    unLoc)
import           StringBuffer                      (StringBuffer,
                                                    hGetStringBuffer)
import           Util                              (getModificationUTCTime,
                                                    looksLikeModuleName,
                                                    modificationTimeIfExists)

#if MIN_VERSION_ghc(8,4,0)
import           HscTypes                          (extendMG, mgElemModule,
                                                    mgLookupModule,
                                                    mgModSummaries,
                                                    mkModuleGraph)
#endif

#if MIN_VERSION_ghc(8,10,0)
import           CliOption                         (Option (..))
#else
import           DynFlags                          (Option (..))
#endif

#if MIN_VERSION_ghc (8,10,0)
import           ErrUtils                          (withTimingD)
#else
import           ErrUtils                          (withTiming)
#endif

#if MIN_VERSION_ghc(8,10,0)
import           HscTypes                          (ModIface_ (..),
                                                    ms_home_allimps)
#else
import           HscTypes                          (ModIface (..), ms_imps)
#endif

#if MIN_VERSION_ghc(8,8,0)
import           HscTypes                          (throwErrors)
#endif

-- Internal
import           Language.Finkel.Builder
import           Language.Finkel.Emit
import           Language.Finkel.Expand
import           Language.Finkel.Fnk
import           Language.Finkel.Form
import           Language.Finkel.Lexer
import           Language.Finkel.Make.TargetSource
import           Language.Finkel.Reader
import           Language.Finkel.Syntax


-- | Data type to represent summarised 'TargetSource'.
data TargetSummary
  = -- | Expanded 'ModSummary', from 'FnkSource' and 'HsSource'.
    EMS !ModSummary      -- ^ Summary of itself
        !(Maybe SPState) -- ^ Parsed state for FnkSource
        [ModSummary]     -- ^ Required home package modules for FnkSource

    -- | Link time file option, from 'OtherSource'.
  | LdInput !Option

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
  contents <- liftIO (hGetStringBuffer path)
  (forms, sp) <- parseSexprs (Just path) contents
  hsc_env <- getSession
  let dflags0 = hsc_dflags hsc_env
  dflags1 <- getDynFlagsFromSPState dflags0 sp
  fnk_env0 <- getFnkEnv
  let tr = traceSummary fnk_env0 "compileFnkFile"
      mname_str = moduleNameString modname
      mname_sdoc = text (mname_str ++ ":")

  -- Compile the form with local DynFlags to support file local pragmas.
  (mdl, reqs) <- withTmpDynFlags dflags1 $
    withTiming' ("FinkelModule [" ++ mname_str ++ "]") $ do
      -- Reset current FnkEnv. No need to worry about managing DynFlags, this
      -- action is wrapped with 'withTmpDynFlags' above.
      resetFnkEnv
      mdl <- compileFnkModuleForm forms
      fnk_env1 <- getFnkEnv
      return (mdl, envRequiredHomeModules fnk_env1)

  tr ["reqs in" <+> mname_sdoc <+> ppr (map ms_mod_name reqs)]

  let rreqs = reverse reqs
  ms <- mkModSummary hsc_env dflags1 path mdl rreqs
  return $! EMS ms (Just sp) rreqs

-- | Parse the file header LANGUAGE pragmas and update given 'DynFlags'.
parseFnkFileHeader
  :: (MonadIO m, MonadFail m) => DynFlags -> FilePath -> m DynFlags
parseFnkFileHeader dflags path = do
  contents <- liftIO (hGetStringBuffer path)
  (_, sp) <- parseHeaderPragmas (Just path) contents
  getDynFlagsFromSPState dflags sp

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
getDynFlagsFromSPState :: MonadIO m => DynFlags -> SPState -> m DynFlags
getDynFlagsFromSPState dflags0 sp = do
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

compileHsFile :: FilePath -> Maybe Phase -> Fnk TargetSummary
compileHsFile path mb_phase = do
  -- Not fully parsing the Haskell source code, it will be parsed by the "load'"
  -- function later.
  hsc_env <- getSession
  (dflags, pp_path) <- liftIO (preprocess' hsc_env (path, mb_phase))
  sbuf <- liftIO (hGetStringBuffer pp_path)
  (simps, timps, L _l mname) <- liftIO (getImports' dflags sbuf pp_path path)
  ms <- mkModSummary' hsc_env dflags path mname simps timps Nothing (Just sbuf)
  return $! plainEMS ms

compileOtherFile :: FilePath -> Fnk TargetSummary
compileOtherFile path = do
  hsc_env <- getSession
  fnk_env <- getFnkEnv
  traceSummary fnk_env
            "compileOtherFile"
            ["Compiling OtherSource:" <+> text path]
  o_file <- liftIO (compileFile hsc_env StopLn (path, Nothing))
  return $! LdInput (FileOption "" o_file)

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
      empty_anns = (Map.empty, Map.empty)
      r_s_loc = mkSrcLoc (fsLit file) 1 1
      r_s_span = mkSrcSpan r_s_loc r_s_loc

      -- XXX: PackageImports language extension not yet supported.  See
      -- 'HscTypes.ms_home_imps'
      imports = map (\lm -> (Nothing, ideclName (unLoc lm)))
                    (hsmodImports mdl)

  -- Adding file path of the required modules and file paths of imported home
  -- package modules to "hpm_src_files" to support recompilation.
  req_srcs <- requiredDependencies reqs

  let pm = HsParsedModule
        { hpm_module = L r_s_span mdl
        , hpm_src_files = req_srcs
        , hpm_annotations = empty_anns }

  mkModSummary' hsc_env dflags file mod_name [] imports (Just pm) Nothing

-- | Make 'ModSummary' for recompilation check done with 'doCheckOldIface'.
mkModSummaryForRecompile
  :: (MonadIO m, MonadFail m) => HscEnv -> TargetUnit -> m ModSummary
mkModSummaryForRecompile hsc_env tu@(tsource, _) = do
  let path = targetSourcePath tsource
      mod_name = targetUnitName tu
      dflags0 = hsc_dflags hsc_env
  dflags1 <- case tsource of
    FnkSource {} -> parseFnkFileHeader dflags0 path
    _            -> return dflags0
  mkModSummary' hsc_env dflags1 path mod_name [] [] Nothing Nothing

-- | Make 'ModSummary' from source file, module name, and imports.
mkModSummary'
  :: MonadIO m
  => HscEnv
  -> DynFlags
  -> FilePath
  -> ModuleName
  -> [(Maybe FastString, Located ModuleName)]
  -> [(Maybe FastString, Located ModuleName)]
  -> Maybe HsParsedModule
  -> Maybe StringBuffer
  -> m ModSummary
mkModSummary' hsc_env dflags file mod_name srcimps txtimps mb_pm mb_buf = do
  -- Throw an exception on module name mismatch.
  assertModuleNameMatch dflags file mb_pm
  -- hsc_env <- getSession

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
      if isObjectTarget (hscTarget dflags) || obj_allowed
        then liftIO (modificationTimeIfExists (ml_obj_file ms_loc))
        else return Nothing
  iface_date <- liftIO (maybeGetIfaceDate dflags ms_loc)
#if MIN_VERSION_ghc(8,8,0)
  hie_date <- liftIO (modificationTimeIfExists (ml_hie_file ms_loc))
#endif
  -- XXX: Fill in the list of required ModSummary.
  return (ms { ms_obj_date = obj_date
             , ms_iface_date = iface_date
#if MIN_VERSION_ghc(8,8,0)
             , ms_hie_date = hie_date
#endif
             })

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
                   loc = getLoc lsaw
               in  throwOneError (mkPlainErrMsg dflags loc msg)
    _ -> return ()

-- See: GhcMake.maybeGetIfaceDate
maybeGetIfaceDate :: DynFlags -> ModLocation -> IO (Maybe UTCTime)
maybeGetIfaceDate dflags location =
  if writeInterfaceOnlyMode dflags
     then modificationTimeIfExists (ml_hi_file location)
     else return Nothing

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
       let emit hdl = printForUser dflags hdl unqual hsrc
           unqual = icPrintUnqual dflags (hsc_IC hsc_env)
           dflags = hsc_dflags hsc_env
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
dumpParsedAST :: MonadIO m => DynFlags -> ModSummary -> m ()
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
requiredDependencies mss = withExpanderSettings' False $ do
  hsc_env <- getSession
  return $! nub $! foldl' (requiredDependency hsc_env) [] mss

requiredDependency :: HscEnv -> [FilePath] -> ModSummary -> [FilePath]
requiredDependency hsc_env = go
  where
    go acc ms =
      case ml_hs_file (ms_location ms) of
        Nothing -> acc
        Just me -> dep_files ms (me : acc)

    dep_files ms acc =
      let mg = hsc_mod_graph hsc_env
          hpt = hsc_HPT hsc_env
          acc1 = find_require_paths hpt acc ms
      in  foldl' (find_import_path mg) acc1 (ms_home_allimps ms)

    find_import_path mg acc mod_name =
      let mdl = mkModule (thisPackage (hsc_dflags hsc_env)) mod_name
      in  maybe acc (go acc) (mgLookupModule' mg mdl)

    find_require_paths hpt acc ms =
      case lookupHpt hpt (ms_mod_name ms) of
        Nothing  -> acc
        Just hmi -> foldl' req_paths acc (mi_usages (hm_iface hmi))

    req_paths acc usage =
      case usage of
        -- Recursively calling `dependencyFile' with the ModSummary referred by
        -- the usage file path .
        UsageFile {usg_file_path = path} ->
          let mb_ms1 = find is_my_path mss
              is_my_path = maybe False (== path) . ml_hs_file . ms_location
              mss = mgModSummaries' (hsc_mod_graph hsc_env)
              acc1 = path : acc
          in  maybe acc1 (go acc1) mb_ms1
        _ -> acc

-- | Trace function for this module.
traceSummary
  :: (MonadIO m, HasDynFlags m) => FnkEnv -> MsgDoc -> [MsgDoc] -> m ()
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


-- ---------------------------------------------------------------------
--
-- Version compatibility functions
--
-- ---------------------------------------------------------------------

extendMG' :: ModuleGraph -> ModSummary -> ModuleGraph
{-# INLINE extendMG' #-}

mgElemModule' :: ModuleGraph -> Module -> Bool
{-# INLINE mgElemModule' #-}

mkModuleGraph' :: [ModSummary] -> ModuleGraph
{-# INLINE mkModuleGraph' #-}

mgModSummaries' :: ModuleGraph -> [ModSummary]
{-# INLINE mgModSummaries' #-}

mgLookupModule' :: ModuleGraph -> Module -> Maybe ModSummary
{-# INLINE mgLookupModule' #-}

#if MIN_VERSION_ghc(8,4,0)
-- ModuleGraph was an alias of [ModSummary] in ghc < 8.4.
extendMG' = extendMG
mgElemModule' = mgElemModule
mkModuleGraph' = mkModuleGraph
mgModSummaries' = mgModSummaries
mgLookupModule' = mgLookupModule
#else
extendMG' = flip (:)
mgElemModule' mg mdl = go mg
  where
    go []       = False
    go (ms:mss) = if ms_mod ms == mdl then True else go mss
mkModuleGraph' = id
mgModSummaries' = id
mgLookupModule' mg mdl = find (\ms -> ms_mod_name ms == moduleName mdl) mg
#endif

#if !MIN_VERSION_ghc(8,10,0)
-- The `ms_home_allimps' function did not exist until ghc 8.10.x.
ms_home_allimps :: ModSummary -> [ModuleName]
ms_home_allimps ms = map unLoc (ms_home_srcimps ms ++ ms_home_imps ms)

ms_home_srcimps :: ModSummary -> [Located ModuleName]
ms_home_srcimps = home_imps . ms_srcimps

ms_home_imps :: ModSummary -> [Located ModuleName]
ms_home_imps = home_imps . ms_imps

home_imps :: [(Maybe FastString, Located ModuleName)] -> [Located ModuleName]
home_imps imps = [ lmodname |  (mb_pkg, lmodname) <- imps, isLocal mb_pkg ]
  where isLocal Nothing    = True
        isLocal (Just pkg) | pkg == fsLit "this" = True -- "this" is special
        isLocal _          = False
#endif

preprocess' :: HscEnv -> (FilePath, Maybe Phase) -> IO (DynFlags, FilePath)
{-# INLINE preprocess' #-}

getImports' :: DynFlags -> StringBuffer -> FilePath -> FilePath
            -> IO ([(Maybe FastString, Located ModuleName)],
                   [(Maybe FastString, Located ModuleName)],
                   Located ModuleName)
{-# INLINE getImports' #-}

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

-- | Label and wrap the given action with 'withTiming'.
withTiming' :: String -> Fnk a -> Fnk a
{-# INLINE withTiming' #-}

#if MIN_VERSION_ghc(8,10,0)
withTiming' label = withTimingD (text label) (const ())
#else
withTiming' label = withTiming getDynFlags (text label) (const ())
#endif
