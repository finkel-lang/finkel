{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Make mode for Finkel compiler.
module Language.Finkel.Make
  ( make
  , simpleMake
  , initSessionForMake
  , buildHsSyn
  ) where

#include "Syntax.h"

-- base
import           Control.Monad                (unless, void, when)
import           Control.Monad.IO.Class       (MonadIO (..))
import           Data.List                    (foldl')
import           Data.Maybe                   (catMaybes, fromMaybe, isJust)
import           System.IO                    (IOMode (..), withFile)

-- container
import           Data.Graph                   (flattenSCCs)
import qualified Data.Map                     as Map

-- directory
import           System.Directory             (createDirectoryIfMissing)

-- filepath
import           System.FilePath              (dropExtension, splitExtension,
                                               takeBaseName, takeDirectory,
                                               takeExtension, (<.>), (</>))

-- ghc
import           BasicTypes                   (SuccessFlag (..))
import           DriverPhases                 (HscSource (..), Phase (..))
import           DriverPipeline               (compileFile, compileOne', link,
                                               preprocess)
import           DynFlags                     (DumpFlag (..), DynFlags (..),
                                               GeneralFlag (..), GhcLink (..),
                                               GhcMode (..), getDynFlags, gopt,
                                               gopt_set, gopt_unset,
                                               isObjectTarget,
                                               parseDynamicFilePragma,
                                               thisPackage)
import           ErrUtils                     (MsgDoc, dumpIfSet_dyn,
                                               mkPlainErrMsg)
import           Exception                    (gbracket)
import           FastString                   (fsLit)
import           Finder                       (addHomeModuleToFinder,
                                               cannotFindModule,
                                               findImportedModule,
                                               findObjectLinkableMaybe,
                                               mkHomeModLocation)
import           GHC                          (getPrintUnqual,
                                               setSessionDynFlags)
import           GHC_Hs                       (HsModule (..))
import           GHC_Hs_Dump                  (BlankSrcSpan (..), showAstData)
import           GHC_Hs_ImpExp                (ImportDecl (..))
import           GhcMake                      (topSortModuleGraph)
import           GhcMonad                     (GhcMonad (..), modifySession)
import           HeaderInfo                   (getOptionsFromFile)
import           HscStats                     (ppSourceStats)
import           HscTypes                     (FindResult (..),
                                               HomeModInfo (..),
                                               HomePackageTable,
                                               HsParsedModule (..), HscEnv (..),
                                               ModSummary (..), ModuleGraph,
                                               SourceModified (..), addToHpt,
                                               eltsHpt, isBootSummary,
                                               isObjectLinkable, linkableTime,
                                               lookupHpt, mi_boot, ms_mod_name,
                                               throwOneError)
import           Module                       (ModLocation (..), Module (..),
                                               ModuleName, installedUnitIdEq,
                                               mkModule, mkModuleName,
                                               moduleName, moduleNameSlashes,
                                               moduleNameString, moduleUnitId)
import           Outputable                   (SDoc, braces, comma, hcat, nest,
                                               ppr, printForUser, punctuate,
                                               quotes, text, vcat, ($$), (<+>))
import           Panic                        (GhcException (..),
                                               throwGhcException)
import           SrcLoc                       (GenLocated (..), Located, getLoc,
                                               mkRealSrcLoc, mkSrcLoc,
                                               mkSrcSpan, unLoc)
import           StringBuffer                 (stringToStringBuffer)
import           Util                         (getModificationUTCTime,
                                               looksLikeModuleName,
                                               modificationTimeIfExists)

import qualified Lexer                        as GHCLexer
import qualified Parser                       as GHCParser

#if MIN_VERSION_ghc(8,10,0)
import           HscTypes                     (mi_module)
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
import           HscTypes                     (extendMG, mapMG, mgElemModule,
                                               mkModuleGraph)

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
  -> Bool -- ^ Skip linking when 'True'.
  -> Bool -- ^ Force recompilation when 'True'.
  -> Maybe FilePath -- ^ Output file, if any.
  -> Fnk ()
make infiles no_link force_recomp mb_output = do

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
  sources <- mapM findTargetUnit infiles

  -- Do the compilation work.
  mod_summaries <- make1 sources

  -- Update current module graph and link. Linking work is delegated to deriver
  -- pipelin's `link' function.
  let mgraph = mkModuleGraph' mod_summaries
      mgraph_flattened = flattenSCCs (topSortModuleGraph True mgraph Nothing)
  modifySession (\hsc_env -> hsc_env {hsc_mod_graph=mgraph})
  unless no_link (doLink mgraph_flattened)

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
  -- moment.
  putFnkEnv (fnk_env { envVerbosity = vrbs2
                     , envDefaultDynFlags = Just dflags1 })

-- | Simple make function returning compiled home module information. Intended
-- to be used in 'require' macro.
simpleMake :: Located String -> Fnk [(ModuleName, HomeModInfo)]
simpleMake lname = do
  let as_pair hmi = (moduleName (mi_module (hm_iface hmi)), hmi)
  tu <- findTargetUnit (lname, Nothing)
  _mss <- make1 [tu]
  fmap (map as_pair . eltsHpt . hsc_HPT) getSession

-- | Run given builder.
buildHsSyn
  :: Builder a -- ^ Builder to use.
  -> [Code]    -- ^ Input codes.
  -> Fnk a
buildHsSyn bldr forms =
  do dflags <- getDynFlags
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
-- Simply a 'TargetSource' maybe paired with 'Phase'.
type TargetUnit = (TargetSource, Maybe Phase)

-- | Make empty 'TargetUnit' from 'TargetSource'
emptyTargetUnit :: TargetSource -> TargetUnit
emptyTargetUnit ts = (ts, Nothing)

-- | Get 'TargetUnit' from pair of module name or file path, and phase.
findTargetUnit :: (Located String, Maybe Phase) -> Fnk TargetUnit
findTargetUnit (lpath, mb_phase) =
  fmap (\ts -> (ts, mb_phase)) (findTargetSource lpath)

-- | Compile 'TargetUnit' to 'ModSummary' and 'HsModule', then compile
-- to interface file and object code.
--
-- Do macro expansion and get the Haskell `import' declarations from
-- parsed source contents. If the macro expanded result does not
-- contain imports from pending modules, compile to '*.hi' and
-- '*.o'. If the HsModule contained imports of pending module, add the
-- module to the pending modules.
--
make1 :: [TargetUnit] -> Fnk [ModSummary]
make1 targets = do
  fnk_env <- getFnkEnv
  let tr = traceMake fnk_env "make1"
      targets_sdoc = nest 2 (vcat (map ppr targets))
  tr [ "total:" <+> text (show total)
     , "targets:", targets_sdoc ]
  summaries <- timeIt "make1 [Finkel]" (make2 [] total total [] targets)
  tr ["done:", targets_sdoc]
  return summaries
  where
    total = length targets

make2 :: [ModSummary] -- ^ Accumulator.
      -> Int          -- ^ module n ...
      -> Int          -- ^ ... of m.
      -> [TargetUnit] -- ^ Ready to compile targets.
      -> [TargetUnit] -- ^ Pending targets.
      -> Fnk [ModSummary]

-- No more modules to compile, return the accumulated ModSummary.
make2 acc i _  _  _ | i <= 0 = return acc
make2 acc _ _ [] [] = return acc

-- Target modules are empty, ready to compile pending modules to read time
-- ModSummary.  Partition the modules, make read time ModSummaries, then sort
-- via topSortModuleGraph, and recurse.
make2 acc i k [] pendings = do
    -- Mixed target sources in pending modules. Reorder the targets and recurse.
    -- No need to worry about module dependency analysis for OtherSource inputs,
    -- those sources do not have dependencies, so moving to the end.
    let (readies0, pendings') = (pendings, [])
        sep_by_trg tu@(trg, _) (fhs, oths) =
            if isOtherSource trg
               then (fhs, tu:oths)
               else (tu:fhs, oths)
        (fnk_and_hs, other) = foldr sep_by_trg ([],[]) readies0
        readies1 = fnk_and_hs ++ other
    make2 acc i k readies1 pendings'

-- Compile ready-to-compile targets to ModSummary and HsModule. Input could be
-- Finkel source code, Haskell source code, or something else. If Finkel source
-- code or Haskell source code, get ModSummary to resolve the dependencies.
make2 acc i k (target@(tsr,_mbp):summarised) pendings = do
  fnk_env <- getFnkEnv
  traceMake fnk_env "make2" ["target:" <+> ppr target]

  -- Since Finkel make is not using 'DriverPipeline.runPipeline', setting
  -- 'DynFlags.dumpPrefix' manually.
  setDumpPrefix (targetSourcePath tsr)

  case tsr of
     FnkSource path _mn _form sp -> do
       let go_fnk mb_result = do
             case mb_result of
               Nothing                   -> failS "compileToHsModule"
               Just (hmdl, dflags, reqs) -> do
                 summary <- mkModSummary path hmdl
                 let summary' = summary {ms_hspp_opts = dflags}
                     imports = hsmodImports hmdl
                     import_names = map import_name imports
                     pending_names = map tsmn pendings
                     summarised_names = map tsmn summarised
                     not_yet_ready =
                       any (\m -> m `elem` pending_names ||
                                  m `elem` summarised_names)
                           import_names

                 traceMake fnk_env
                           "make2"
                           ["imports:" <+>
                             if null import_names
                                then text "none"
                                else vcat (map text import_names)]

                 -- Test whether imported modules are in pendings. If found, skip
                 -- the compilation and add this module to the list of pending
                 -- modules.
                 --
                 -- N.B. For Finkel source target, dependency modules passed to
                 -- 'makeOne' are required modules.
                 --
                 if not_yet_ready
                    then make2 acc i k summarised (target:pendings)
                    else compileIfReady fnk_env summary' (Just sp) imports reqs

       compileToHsModule target >>= go_fnk

     -- XXX: This approach will not work when required home modules from _mn
     -- were modified. Basically, cannot use ModSummary from current module
     -- graph. Need to parse the source code to expand with potentially updated
     -- macros from required home package modules.
     --
     -- hsc_env <- getSession
     -- fr <- liftIO (findHomeModule hsc_env (mkModuleName _mn))
     -- case fr of
     --   Found _mloc mdl
     --     | Just summary <- mgLookupModule (hsc_mod_graph hsc_env) mdl
     --     , Just hpm <- ms_parsed_mod summary -> do
     --       -- Doing simple source code modification check to support
     --       -- reloading from REPL.
     --       src_modified <- checkSrcModified summary
     --       if src_modified
     --          then compileToHsModule target >>= go_fnk
     --          else do
     --            let imports = hsmodImports (unLoc (hpm_module hpm))
     --                reqs = return []
     --            compileIfReady fnk_env summary (Just sp) imports reqs
     --   _ -> do
     --     traceMake fnk_env "make2"
     --               [text _mn <+> "not found in current session"]
     --     compileToHsModule target >>= go_fnk

     HsSource path -> do
       mb_result <- compileToHsModule target
       case mb_result of
         Nothing             -> failS "compileToHsModule"
         Just (hmdl, dflags, _) -> do
           summary <- mkModSummary path hmdl
           let summary' = summary {ms_hspp_opts = dflags}
               imports = hsmodImports hmdl
           compileIfReady fnk_env summary' Nothing imports []

     OtherSource _ -> do
       _ <- compileToHsModule target
       make2 acc i k summarised pendings

     where
       compileIfReady :: FnkEnv
                      -> ModSummary
                      -> Maybe SPState
                      -> [HImportDecl]
                      -> [ModSummary]
                      -> Fnk [ModSummary]
       compileIfReady fnk_env summary mb_sp imports reqs = do
         hsc_env <- getSession
         is <- mapM (findNotCompiledImport hsc_env acc) imports
         let is' = catMaybes is
         traceMake fnk_env
                   "make2.compileIfReady"
                   ["filtered imports:" <+> text (show is')]
         if not (null is')
            then do
              -- Imported modules are not fully compiled yet. Move this module
              -- to the end of summarised modules and recurse.
              let summarised' = is' ++ summarised ++ [target]
                  i' = i + length is'
                  k' = k + length is'
              make2 acc i' k' summarised' pendings
            else do
              -- Ready to compile this target unit. Compile it, add the returned
              -- ModSummary to accumulator, and continue.
              summary' <- makeOne i k mb_sp summary reqs
              make2 (summary':acc) (i-1) k summarised pendings

tsmn :: (TargetSource, a) -> String
tsmn (ts, _) =
  case ts of
    FnkSource _ mn _ _ -> mn
    HsSource path      -> asModuleName path
    _                  -> "module-name-unknown"

-- | Check whether recompilation is required, and compile the 'HsModule' when
-- the codes or dependency modules were updated.
makeOne
  :: Int -> Int -> Maybe SPState -> ModSummary
  -> [ModSummary] -> Fnk ModSummary
makeOne i total mb_sp summary required_summaries = timeIt label go
  where
    label = "MakeOne [" ++ mname ++ "]"
    mname = moduleNameString (ms_mod_name summary)
    go = do
      up_to_date <- checkUpToDate summary required_summaries
      fnk_env <- getFnkEnv
      traceMake fnk_env "makeOne"
                [ "required:" <+> nest 2 (vcat (map (ppr . ms_mod_name)
                                                    required_summaries))
                , "up_to_date:" <+> ppr up_to_date]
      let midx = total - i + 1
          src_modified = if up_to_date
                            then SourceUnmodified
                            else SourceModified

      -- Using the cached dynflags from ModSummary. Note that the cached
      -- dynflags is always used, no matter whether the module is updated or
      -- not. This is to support loading already compiled module objects with
      -- language extensions not set in current dynflags.
      withTmpDynFlags (ms_hspp_opts summary) $
        doMakeOne midx total mb_sp summary src_modified

-- | Compile single module.
doMakeOne
  :: Int -- ^ Module index number.
  -> Int -- ^ Total number of modules.
  -> Maybe SPState -- ^ State returned from parser.
  -> ModSummary -- ^ Summary of module to compile.
  -> SourceModified -- ^ Source modified?
  -> Fnk ModSummary -- ^ Updated summary.
doMakeOne i total mb_sp ms src_modified = do
  hsc_env <- getSession
  fnk_env <- getFnkEnv

  let dflags = ms_hspp_opts ms
      loc = ms_location ms
      mod_name = ms_mod_name ms
      tryGetTimeStamp = liftIO . modificationTimeIfExists
      messager = envMessager fnk_env
      hpt0 = hsc_HPT hsc_env
      mb_hm_info = lookupHpt hpt0 mod_name
      mb_old_iface =
        -- See: GhcMake.upsweep_mod
        case mb_hm_info of
          Nothing                            -> Nothing
          Just hm_info | isBootSummary ms    -> Just iface
                       | not (mi_boot iface) -> Just iface
                       | otherwise           -> Nothing
                       where
                        iface = hm_iface hm_info
      obj_allowed = isObjectTarget (hscTarget dflags)

  traceMake fnk_env "doMakeOne" ["Module:" <+> ppr mod_name]

  -- Dump the parsed AST.
  dumpParsedAST dflags ms

  -- Lookup reusable old linkable. Strategy for reusing object codes and byte
  -- codes differs to support reloading modules from REPL, and to support
  -- rebuilding cabal package without unnecessary recompilation.
  --
  -- For object codes, use the one found with 'findObjectLinkableMaybe', which
  -- is written as file, or the oen found from home package table. For byte
  -- code, reuse the one found in home package table if the linkable was not an
  -- object code.
  --
  mb_old_linkable <- do
    let mb_linkable = mb_hm_info >>= hm_linkable
    if not obj_allowed
       then case mb_linkable of
              Just l | not (isObjectLinkable l) -> return mb_linkable
              _                                 -> return Nothing
       else case mb_linkable of
              Just l | isObjectLinkable l -> return mb_linkable
              _ -> do
                mb_obj <- liftIO (findObjectLinkableMaybe (ms_mod ms) loc)
                case mb_obj of
                  Just l | linkableTime l >= ms_hs_date ms ->
                           return mb_obj
                  _ -> return Nothing

  -- Adjust 'SourceModified'. Using the 'src_modified' as-is only when compiling
  -- object code and reusable old interface and old linkable were found.
  let src_modified'
        | is_bco, Nothing <- mb_old_iface = SourceModified
        | is_bco, Nothing <- mb_old_linkable = SourceModified
        | otherwise = src_modified
        where is_bco = not obj_allowed

  -- Compile and add the returned module to finder and home package table.
  home_mod_info <-
    liftIO
      (do home_mod_info <-
            compileOne' Nothing (Just messager) hsc_env ms i total
                        mb_old_iface mb_old_linkable src_modified'
          _ <- addHomeModuleToFinder hsc_env mod_name loc
          return home_mod_info)

  -- Update the time stamp of generated obj and hi files.
  mb_obj_date <- tryGetTimeStamp (ml_obj_file loc)
  mb_iface_date <- tryGetTimeStamp (ml_hi_file loc)

  let ms1 = ms { ms_obj_date = mb_obj_date
               , ms_iface_date = mb_iface_date }
      mgraph0 = hsc_mod_graph hsc_env
      mgraph1 = if mgElemModule' mgraph0 (ms_mod ms1)
                   then mapMG' replace_ms mgraph0
                   else extendMG' mgraph0 ms1
      replace_ms ms0 = if ms_mod ms0 == ms_mod ms
                          then ms1
                          else ms0

  modifySession
    (\e -> e { hsc_HPT = addToHpt hpt0 mod_name home_mod_info
             , hsc_mod_graph = mgraph1 })

  -- Dump the module contents as haskell source when dump option were set and
  -- this is the first time for compiling the target Module.
  when (fopt Fnk_dump_hs fnk_env || isJust (envHsOutDir fnk_env))
       (case lookupHpt hpt0 mod_name of
          Nothing -> dumpModSummary mb_sp ms
          Just _  -> return ())

  return ms1

-- [Avoiding Recompilation]
-- ~~~~~~~~~~~~~~~~~~~~~~~~
--
-- See below for details of how GHC avoid recompilation:
--
--   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/compiler/recompilation-avoidance
--
-- The logic used in below function `checkUpToDate' is almost same as in
-- 'GhcMake.checkStability', but for object codes only, and the arguments are
-- different.
--
-- Other than this, most recompilation checks are done in
-- "DriverPipeLine.compileOne'" function, which is called in "doMakeOne".
--
-- To support recompiling the target module when the required modules were
-- changed, comparing the object code timestamp of the target with source
-- modification time of the dependencies.

checkUpToDate :: ModSummary -> [ModSummary] -> Fnk Bool
checkUpToDate ms required_deps =
  case ms_obj_date ms of
    -- No object code, test whether the current hscTarget is object code or not.
    -- When compiling bytecode, further up-to-date check will be done in
    -- "compileOne'".
    --
    -- XXX: Lookup home package table, use `linkableTime' of the bytecode
    -- linkable to support recompiling on required module update for
    -- HscInterpreted hscTarget.
    Nothing -> do
      dflags <- getDynFlags
      return (not (isObjectTarget (hscTarget dflags)))

    -- Found timestamp for object code.
    Just obj_date -> do
      hpt <- hsc_HPT <$> getSession
      let mn = ms_mod_name ms
          sccs_without_self = filter (\m -> mn /= ms_mod_name m)
                                     required_deps
          object_ok m =
            case ms_obj_date m of
              Just t -> t >= ms_hs_date ms && same_as_prev t
                        -- XXX: Will it be better to skip this test when
                        -- calling from 'makeOne'?
                        && obj_date >= ms_hs_date m
              _      -> False
          same_as_prev t =
            case lookupHpt hpt mn of
              Just hmi | Just l <- hm_linkable hmi ->
                         isObjectLinkable l && t == linkableTime l
              _other -> True
          up_to_date = obj_date >= ms_hs_date ms &&
                       all object_ok sccs_without_self

      return up_to_date

-- | Find not compiled module.
findNotCompiledImport
  :: HscEnv       -- ^ Current hsc environment.
  -> [ModSummary] -- ^ List of accumulated 'ModSummary'.
  -> HImportDecl  -- ^ The target module name to find.
  -> Fnk (Maybe TargetUnit)
findNotCompiledImport hsc_env acc idecl = do
  findResult <- liftIO (findImportedModule hsc_env mname Nothing)
  dflags <- getDynFlags
  let myInstalledUnitId = thisInstalledUnitId dflags
  case findResult of
    -- Haskell module returned by `Finder.findImportedModule' may not compiled
    -- yet. If the source code has Haskell file extension, checking whether the
    -- module is listed in accumulator containing compiled modules.
    Found mloc mdl -> do
      fnk_env <- getFnkEnv
      traceMake fnk_env
                "findNotCompiledImport"
                [ "Found" <+> ppr mname
                , pprMLoc mloc
                , "moduleUnitId:" <+>  ppr (moduleUnitId mdl) ]
      case ml_hs_file mloc of
        Just path | takeExtension path `elem` [".hs"] ->
                    if moduleName mdl `elem` map ms_mod_name acc
                       then return Nothing
                       else do
                         ts <- findTargetModuleName lmname
                         return (Just (ts, Nothing))
        _ | inSameUnit && notInAcc -> do
            -- Workaround for loading home package modules when working with
            -- cabal package from REPL.  'Finder.findImportedModule' uses hard
            -- coded source file extensions in `Finder.findInstalledHomeModule'
            -- to find Haskell source codes of home package module, which will
            -- not find Finkel source files. When looking up modules in home
            -- package as dependency, looking up in accumurated ModSummary list
            -- to avoid using the modules found in already compiled linkable
            -- package. The `linkable' mentioned here are those
            -- 'libXXX.{a,dll,so}' artifact files for library package
            -- components.
            --
            -- This workaround needs the 'UnitId' from REPL session to be set
            -- via "-this-unit-id" DynFlag option, to avoid loading modules from
            -- linkable. The 'UnitId' set from REPL need to exactly match the
            -- 'UnitId' of package, including the hash part.
            --
            mb_ts <- findTargetModuleNameMaybe lmname
            return (fmap emptyTargetUnit mb_ts)
          | otherwise -> return Nothing
        where
          inSameUnit =
            myInstalledUnitId `installedUnitIdEq` moduleUnitId mdl
          notInAcc =
            moduleName mdl `notElem` map ms_mod_name acc
    _              -> do
      let loc = getLoc idecl
      mb_ts <- findTargetModuleNameMaybe lmname
      case mb_ts of
        Just ts -> return (Just (emptyTargetUnit ts))
        Nothing -> do
          let doc = cannotFindModule dflags mname findResult
              err = mkPlainErrMsg dflags loc doc
          throwOneError err
  where
    mname = mkModuleName (import_name idecl)
    lmname = L (getLoc idecl) mname

-- | Make 'ModSummary'.
mkModSummary :: GhcMonad m => FilePath -> HModule -> m ModSummary
mkModSummary file mdl =
  let modName = case hsmodName mdl of
                  Just name -> unLoc name
                  Nothing   -> mkModuleName "Main"
      imports = map (ideclName . unLoc) (hsmodImports mdl)
      emptyAnns = (Map.empty, Map.empty)
      r_s_loc = mkSrcLoc (fsLit file) 1 1
      r_s_span = mkSrcSpan r_s_loc r_s_loc
      pm = HsParsedModule
        { hpm_module = L r_s_span mdl
        , hpm_src_files = [file]
        , hpm_annotations = emptyAnns }
  in  mkModSummary' file modName imports (Just pm)

-- | Make 'ModSummary' from source file, module name, and imports.
mkModSummary'
  :: GhcMonad m
  => FilePath
  -> ModuleName
  -> [Located ModuleName]
  -> Maybe HsParsedModule
  -> m ModSummary
mkModSummary' file modName imports mb_pm = do
  dflags0 <- getDynFlags

  -- Throw an exception on module name mismatch.
  assertModuleNameMatch dflags0 file mb_pm

  let unitId = thisPackage dflags0
      mmod = mkModule unitId modName
      imported = map (\x -> (Nothing, x)) imports
      tryGetObjectDate path =
        if isObjectTarget (hscTarget dflags0)
           then modificationTimeIfExists path
           else return Nothing
  liftIO
    (do mloc <- mkHomeModLocation dflags0 modName file
        hs_date <- getModificationUTCTime file
        obj_date <- tryGetObjectDate (ml_obj_file mloc)
        iface_date <- modificationTimeIfExists (ml_hi_file mloc)
        dflags1 <-
          if isHsFile file
             then do
               opts <- getOptionsFromFile dflags0 file
               (dflags1,_,_) <- parseDynamicFilePragma dflags0 opts
               return dflags1
             else return dflags0
        return ModSummary { ms_mod = mmod
                          , ms_hsc_src = HsSrcFile
                          , ms_location = mloc
                          , ms_hs_date = hs_date
                          , ms_obj_date = obj_date
                          , ms_iface_date = iface_date
#if MIN_VERSION_ghc(8,8,0)
                            -- XXX: .hie file not supported yet.
                          , ms_hie_date = Nothing
#endif
                          , ms_parsed_mod = mb_pm
                          , ms_srcimps = []
                          , ms_textual_imps = imported
                          , ms_hspp_file = file
                          , ms_hspp_opts = dflags1
                          , ms_hspp_buf = Nothing })

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
dumpModSummary mb_sp ms = maybe (return ()) work (ms_parsed_mod ms)
  where
    work pm = when (isFnkFile orig_path) $ do
      fnk_env <- getFnkEnv
      let hsrc = gen pm
          hdr = text (unwords [colons, orig_path, colons])
      debugWhen fnk_env Fnk_dump_hs ["", hdr, "" , hsrc, ""]
      mapM_ (doWrite fnk_env hsrc) (envHsOutDir fnk_env)
    doWrite fnk_env hsrc dir = do
       let mname = moduleName (ms_mod ms)
           bname = takeBaseName orig_path
           file_name = if looksLikeModuleName bname
                          then moduleNameSlashes mname
                          else bname
           out_path = dir </> file_name <.> "hs"
           out_dir = takeDirectory out_path
       traceMake fnk_env "dumpModSummary" ["Writing to" <+> text out_path]
       dflags <- getDynFlags
       unqual <- getPrintUnqual
       let emit hdl = printForUser dflags hdl unqual hsrc
       liftIO (do createDirectoryIfMissing True out_dir
                  withFile out_path WriteMode emit)
    gen pm = toHsSrc sp (Hsrc (unLoc (hpm_module pm)))
    orig_path = ms_hspp_file ms
    sp = fromMaybe dummy_sp mb_sp
    dummy_sp = initialSPState (fsLit orig_path) 1 1
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

compileToHsModule
  :: TargetUnit -> Fnk (Maybe (HModule, DynFlags, [ModSummary]))
compileToHsModule (tsrc, mbphase) =
  case tsrc of
    FnkSource _ mn form sp -> Just <$> compileFnkModuleForm sp mn form
    HsSource path          -> Just <$> compileHsFile path mbphase
    OtherSource path       -> compileOtherFile path >> return Nothing

-- | Compile Finkel module form, to use fresh set of modules, language
-- extensions, and macros in 'FnkEnv'. Returns tuple of compiled module,
-- potentially modified 'Dynflags' to update 'ModSummary', and required module
-- names.
compileFnkModuleForm :: SPState -> String -> [Code]
                     -> Fnk (HModule, DynFlags, [ModSummary])
compileFnkModuleForm sp modname forms = do
  dflags0 <- getDynFlagsFromSPState sp
  hsc_env <- getSession
  fnk_env0 <- getFnkEnv
  let tr = traceMake fnk_env0 "compileFnkModuleForm"

  -- Compile the form with the file specific DynFlags to support file local
  -- pragmas.
  (mdl, reqs, compiled) <- withTmpDynFlags dflags0 $ do
    timeIt ("FinkelModule [" ++ modname ++ "]") $ do
      -- Reset current FnkEnv. No need to worry about managing DynFlags, this
      -- action is wrapped by 'withTmpDynFlags' above.
      resetFnkEnv
      mdl <- compileFnkModuleForm' forms
      fnk_env1 <- getFnkEnv
      let required = envRequiredHomeModules fnk_env1
          compiled = envCompiledInRequire fnk_env1
      return (mdl, required, compiled)

  tr ["reqs in" <+> text (modname ++ ":") <+>
      text (show (map (moduleNameString . ms_mod_name) reqs))
     ,"compiled in" <+> text (modname ++ ":") <+>
      ppr (map fst compiled)]

  -- Add the compiled home modules to current session, if any. This fill avoid
  -- recompilation of required modules with "-fforce-recomp" option, which is
  -- required more than once.
  let hpt1 he = addHomeModInfoIfMissing (hsc_HPT he) compiled
  setSession (hsc_env {hsc_HPT = hpt1 hsc_env})

  return (mdl, dflags0, reverse reqs)

-- -- | Compile 'HModule' from given list of codes.
compileFnkModuleForm' :: [Code] -> Fnk HModule
compileFnkModuleForm' form = do
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

-- | Add pair of module name and home module to home package table only
-- when the module is missing.
addHomeModInfoIfMissing
  :: HomePackageTable
  -> [(ModuleName, HomeModInfo)]
  -> HomePackageTable
addHomeModInfoIfMissing = foldl' f
  where
    f hpt (name, hmi) =
      case lookupHpt hpt name of
        Just _  -> hpt
        Nothing -> addToHpt hpt name hmi

-- | Reset macros, required modules, and 'DynFlags' for make in current
-- FnkEnv.
resetFnkEnv :: Fnk ()
resetFnkEnv =
  modifyFnkEnv (\fnkc_env ->
                   fnkc_env { envMacros = envDefaultMacros fnkc_env
                            , envRequiredHomeModules = []
                            , envCompiledInRequire = [] })

compileHsFile :: FilePath -> Maybe Phase -> Fnk (HModule, DynFlags, [a])
compileHsFile source mbphase = do
  hsc_env <- getSession
  (dflags, source') <- liftIO (preprocess' hsc_env (source, mbphase))
  contents <- liftIO (readFile source')
  let location = mkRealSrcLoc (fsLit source) 1 1
      sbuf = stringToStringBuffer contents
      parseState = GHCLexer.mkPState dflags sbuf location
  case GHCLexer.unP GHCParser.parseModule parseState of
    GHCLexer.POk _ m    -> return (unLoc m, dflags, [])
    GHCLexer.PFailed {} -> failS ("Parser error with " ++ source)

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

-- | Link 'ModSummary's, when required.
doLink :: [ModSummary] -> Fnk ()
doLink mgraph = do
  guessOutputFile mgraph
  hsc_env <- getSession

  -- Following the works done in "GhcMake.load'".
  let dflags = hsc_dflags hsc_env
      main_mod = mainModIs dflags
      root_has_Main = any ((== main_mod) . ms_mod) mgraph
      no_hs_main = gopt Opt_NoHsMain dflags
      do_linking = root_has_Main ||
                   no_hs_main ||
                   ghcLink dflags == LinkDynLib ||
                   ghcLink dflags == LinkStaticLib
  linkResult <-
    liftIO (link (ghcLink dflags) dflags do_linking (hsc_HPT hsc_env))

  case linkResult of
    Failed    -> failS "Language.Finkel.Make.doLink: link failed"
    Succeeded -> return ()

-- | Set 'dumpPrefix' from file path.
setDumpPrefix :: FilePath -> Fnk ()
setDumpPrefix path = do
  dflags0 <- getDynFlags
  let (basename, _suffix) = splitExtension path
      dflags1 = dflags0 {dumpPrefix = Just (basename ++ ".")}
  setDynFlags dflags1

-- | Get module name from import declaration.
import_name :: HImportDecl -> String
import_name = moduleNameString . unLoc . ideclName . unLoc

-- | Run given action with temporary 'DynFlags'.
withTmpDynFlags :: GhcMonad m => DynFlags -> m a -> m a
withTmpDynFlags dflags act =
  gbracket getDynFlags setDynFlags (\_ -> setDynFlags dflags >> act)
{-# INLINE withTmpDynFlags #-}

-- [guessOutputFile]
--
-- Following 'guessOutputFile' is mostly copied from "GhcMake.hs", it was not
-- exported. Modified to take [ModSummary] argument instead of getting the
-- module graph from session.

-- | If there is no -o option, guess the name of target executable
-- by using top-level source file name as a base.
guessOutputFile :: [ModSummary] -> Fnk ()
guessOutputFile !mod_graph = modifySession $ \env ->
    let dflags = hsc_dflags env
        -- Force mod_graph to avoid leaking env
        -- !mod_graph = hsc_mod_graph env
        mainModuleSrcPath :: Maybe String
        mainModuleSrcPath = do
            let isMain = (== mainModIs dflags) . ms_mod
            [ms] <- return (filter isMain mod_graph)
            ml_hs_file (ms_location ms)
        name = fmap dropExtension mainModuleSrcPath

        name_exe = do
#if defined(mingw32_HOST_OS)
          -- we must add the .exe extension unconditionally here, otherwise
          -- when name has an extension of its own, the .exe extension will
          -- not be added by DriverPipeline.exeFileName.  See #2248
          name' <- fmap (<.> "exe") name
#else
          name' <- name
#endif
          mainModuleSrcPath' <- mainModuleSrcPath
          -- #9930: don't clobber input files (unless they ask for it)
          if name' == mainModuleSrcPath'
            then throwGhcException . UsageError $
                 "default output name would overwrite the input file; " ++
                 "must specify -o explicitly"
            else Just name'
     in
     case outputFile dflags of
         Just _  -> env
         Nothing -> env { hsc_dflags = dflags { outputFile = name_exe } }

-- | Pretty print 'ModLocation'.
pprMLoc :: ModLocation -> SDoc
pprMLoc mloc =
  vcat
    [ text "ModLocation"
    , nest 2
           (braces
             (vcat
                (punctuate
                   comma
                   [ text "ml_hs_file =" <+>
                     case ml_hs_file mloc of
                       Nothing   -> text "Nothing"
                       Just file -> text "Just" <+> text file
                   , text "ml_hi_file =" <+> text (ml_hi_file mloc)
                   , text "ml_obj_file =" <+> text (ml_obj_file mloc)])))]

-- | Trace function for this module.
traceMake :: FnkEnv -> MsgDoc -> [MsgDoc] -> Fnk ()
traceMake fnk_env fn_name msgs0 =
  let msgs1 = (hcat [";;; [Language.Finkel.Make.", fn_name, "]:"] : msgs0)
  in  debugWhen fnk_env Fnk_trace_make msgs1


-- ------------------------------------------------------------------------
--
-- GHC version compatibility functions
--
-- ------------------------------------------------------------------------

-- ModuleGraph was an alias of [ModSummary] in ghc < 8.4.

extendMG' :: ModuleGraph -> ModSummary -> ModuleGraph
mapMG' :: (ModSummary -> ModSummary) -> ModuleGraph -> ModuleGraph
mgElemModule' :: ModuleGraph -> Module -> Bool
mkModuleGraph' :: [ModSummary] -> ModuleGraph

#if MIN_VERSION_ghc(8,4,0)
extendMG' = extendMG
mapMG' = mapMG
mgElemModule' = mgElemModule
mkModuleGraph' = mkModuleGraph
#else
extendMG' = flip (:)
mapMG' = map
mgElemModule' mg mdl = go mg
  where
    go []       = False
    go (ms:mss) = if ms_mod ms == mdl then True else go mss
mkModuleGraph' = id
#endif

-- | Label and wrap the given action with 'withTiming'.
timeIt :: String -> Fnk a -> Fnk a
#if MIN_VERSION_ghc(8,10,0)
timeIt label = withTimingD (text label) (const ())
#else
timeIt label = withTiming getDynFlags (text label) (const ())
#endif

preprocess' :: HscEnv -> (FilePath, Maybe Phase) -> IO (DynFlags, FilePath)
#if MIN_VERSION_ghc(8,8,0)
preprocess' hsc_env (path, mb_phase) =
  do et_result <- preprocess hsc_env path Nothing mb_phase
     case et_result of
       Left err   -> throwErrors err
       Right pair -> return pair
#else
preprocess' = preprocess
#endif
