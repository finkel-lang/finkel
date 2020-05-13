{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
-- | Make mode for Finkel compiler.
module Language.Finkel.Make
  ( make
  , simpleMake
  , initSessionForMake
  , buildHsSyn
  ) where

#include "Syntax.h"

-- base
import           Control.Monad                (unless, when)
import           Control.Monad.IO.Class       (MonadIO (..))
import           Data.List                    (find, foldl')
import           Data.Maybe                   (catMaybes, fromMaybe, isJust,
                                               maybeToList)

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
import           DriverPipeline               (compileOne', link, oneShot,
                                               preprocess)
import           DynFlags                     (DumpFlag (..), DynFlags (..),
                                               GeneralFlag (..), GhcLink (..),
                                               GhcMode (..), getDynFlags, gopt,
                                               gopt_set, gopt_unset, interpWays,
                                               isObjectTarget,
                                               parseDynamicFilePragma,
                                               thisPackage)
import           ErrUtils                     (dumpIfSet_dyn, mkErrMsg)
import           FastString                   (fsLit)
import           Finder                       (addHomeModuleToFinder,
                                               cannotFindModule,
                                               findImportedModule,
                                               findObjectLinkableMaybe,
                                               mkHomeModLocation)
import           GHC                          (setSessionDynFlags)
import           GHC_Hs                       (HsModule (..))
import           GHC_Hs_Dump                  (BlankSrcSpan (..), showAstData)
import           GHC_Hs_ImpExp                (ImportDecl (..))
import           GhcMake                      (topSortModuleGraph)
import           GhcMonad                     (GhcMonad (..), modifySession,
                                               withTempSession)
import           HeaderInfo                   (getOptionsFromFile)
import           HscStats                     (ppSourceStats)
import           HscTypes                     (FindResult (..),
                                               HomeModInfo (..),
                                               HomePackageTable,
                                               HsParsedModule (..), HscEnv (..),
                                               InteractiveContext (..),
                                               ModSummary (..), ModuleGraph,
                                               SourceModified (..), addToHpt,
                                               eltsHpt, isBootSummary,
                                               isObjectLinkable, linkableTime,
                                               lookupHpt, mi_boot, ms_mod_name,
                                               throwOneError)
import           Module                       (ModLocation (..), ModuleName,
                                               installedUnitIdEq, mkModule,
                                               mkModuleName, moduleName,
                                               moduleNameSlashes,
                                               moduleNameString, moduleUnitId)
import           Outputable                   (neverQualify, ppr, showPpr, text)
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
import           HscTypes                     (mkModuleGraph)
#endif

-- time
import           Data.Time                    (getCurrentTime)

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
make :: [(Located FilePath, Maybe Phase)]
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
      dflags2 | force_recomp = gopt_set dflags1 Opt_ForceRecomp
              | otherwise    = gopt_unset dflags1 Opt_ForceRecomp
  setDynFlags dflags2
  dflags3 <- getDynFlags

  debugFnk ";;; [Language.Finkel.Make.make] DynFlags:"
  dumpDynFlags dflags3

  -- Preserve the language extension values in initial dynflags to FnkEnv, to
  -- reset the language extension later, to keep fresh set of language extensios
  -- per module.
  let lexts = (language dflags3, extensionFlags dflags3)
      findIt (lpath, mb_phase) =
        fmap (\ts -> (ts, mb_phase)) (findTargetSource lpath)
  modifyFnkEnv (\e -> e {envDefaultLangExts = lexts})

  -- Decide the kind of sources of the inputs, inputs arguments could be file
  -- paths, or module names.
  sources <- mapM findIt infiles

  -- Do the compilation work.
  mod_summaries <- make' sources

  -- Update current module graph and link. Linking work is delegated to deriver
  -- pipelin's `link' function.
  let mgraph = mkModuleGraph' mod_summaries
      mgraph_flattened =
        flattenSCCs (topSortModuleGraph True mgraph Nothing)
  modifySession (\hsc_env -> hsc_env {hsc_mod_graph=mgraph})
  unless no_link (doLink mgraph_flattened)

-- | Calls 'GHC.setSessionDynFlags' to initialize session.
initSessionForMake :: Fnk ()
initSessionForMake = do
  -- Returned list of 'InstalledUnitId's are ignored.
  _preload <- getDynFlags >>= setSessionDynFlags

  -- Load modules names in FnkEnv to current interactive context.
  fnkc_env <- getFnkEnv
  let ctx_modules = envContextModules fnkc_env
  unless (null ctx_modules) (setContextModules ctx_modules)

  -- Debug information could be specified from environment variable and command
  -- line option.
  debug0 <- getFnkDebug
  let debug1 = envDebug fnkc_env
  putFnkEnv (fnkc_env {envDebug = debug0 || debug1})

-- | Simple make function returning compiled home module information. Intended
-- to be used in 'require' macro.
simpleMake :: Bool -> Located String -> Fnk [(ModuleName, HomeModInfo)]
simpleMake force_recomp lname = do
  make [(lname, Nothing)] False force_recomp Nothing
  let as_pair hmi = (moduleName (mi_module (hm_iface hmi)), hmi)
  fmap (map as_pair . eltsHpt . hsc_HPT) getSession

-- | Run given builder.
buildHsSyn :: Builder a -- ^ Builder to use.
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

-- | Compile 'TargetUnit' to 'ModSummary' and 'HsModule', then compile
-- to interface file, and object code.
--
-- Do macro expansion and get the Haskell `import' declarations from
-- parsed source contents. If the macro expanded result does not
-- contain imports from pending modules, compile to '*.hi' and
-- '*.o'. If the HsModule contained imports of pending module, add the
-- module to the pending modules.
--
make' :: [TargetUnit] -> Fnk [ModSummary]
make' pendings0 = do
  debugFnk
    (concat [ ";;; make'\n"
            , ";;;   total: " ++ show total ++ "\n"
            , ";;;   pendings0: " ++ show pendings0])
  timeIt "make' [Finkel]" (go [] total total [] pendings0)
  where
    go :: [ModSummary] -- ^ Accumulator.
       -> Int          -- ^ module n ...
       -> Int          -- ^ ... of m.
       -> [TargetUnit] -- ^ Ready to compile targets.
       -> [TargetUnit] -- ^ Pending targets.
       -> Fnk [ModSummary]

    -- No more modules to compile, return the accumulated ModSummary.
    go acc i _  _  _ | i <= 0 = return acc
    go acc _ _ [] [] = return acc

    -- Target modules are empty, ready to compile pending modules to read time
    -- ModSummary.  Partition the modules, make read time ModSummaries, then
    -- sort via topSortModuleGraph, and recurse.
    go acc i k [] pendings
      | all (isOtherSource . fst) pendings =
        -- All targets are other source, no need to worry about module
        -- dependency analysis.
        go acc i k pendings []
      | otherwise = do
        -- Mixed target sources in pending modules. Make read-time-mod-summaries
        -- from pending modules, make new graph, sort it, then recurse.
        let (readies', pendings') = (pendings, [])
        rt_mss <- mkReadTimeModSummaries readies'
        let pre_graph = mkModuleGraph' rt_mss
            graph = topSortModuleGraph True pre_graph Nothing
            readies'' = sortTargets (flattenSCCs graph) readies'
        go acc i k readies'' pendings'

    -- Compile ready-to-compile targets to ModSummary and HsModule. Input could
    -- be Finkel source code, Haskell source code, or something else. If Finkel
    -- source code or Haskell source code, get ModSummary to resolve the
    -- dependencies.
    go acc i k (target@(tsr,_mbp):summarised) pendings = do
      debugFnk (";;; make'.go: target=" ++ show target)

      -- Since Finkel make is not using 'DriverPipeline.runPipeline', setting
      -- 'DynFlags.dumpPrefix' manually.
      setDumpPrefix (targetSourcePath tsr)

      case tsr of
        FnkSource path _mn _form sp -> do
          mb_result <- compileToHsModule target
          case mb_result of
            Nothing                   -> failS "compileToHsModule"
            Just (hmdl, dflags, reqs) -> do
              summary <- mkModSummary (Just path) hmdl
              let summary' = summary {ms_hspp_opts = dflags}
                  imports = hsmodImports hmdl
                  import_names = map import_name imports
                  pending_names = map tsmn pendings
                  summarised_names = map tsmn summarised
                  not_yet_ready =
                    any (\m -> m `elem` pending_names ||
                               m `elem` summarised_names)
                        import_names

              debugFnk (";;; make'.go: imports=" ++ show import_names)

              -- Test whether imported modules are in pendings. If found, skip
              -- the compilation and add this module to the list of pending
              -- modules.
              --
              -- N.B. For Finkel source target, dependency modules passed to
              -- 'makeOne' are required modules.
              --
              if not_yet_ready
                 then go acc i k summarised (target:pendings)
                 else do
                   hsc_env <- getSession
                   let act = findRequiredModSummary hsc_env
                       act' = fmap catMaybes (mapM act reqs)
                   compileIfReady summary' (Just sp) imports act'

        HsSource path -> do
          mb_result <- compileToHsModule target
          case mb_result of
            Nothing             -> failS "compileToHsModule"
            Just (hmdl, dflags, _) -> do
              summary <- mkModSummary (Just path) hmdl
              let summary' = summary {ms_hspp_opts = dflags}
                  imports = hsmodImports hmdl
              compileIfReady summary' Nothing imports (return [])

        OtherSource _ -> do
          _ <- compileToHsModule target
          go acc i k summarised pendings

        where
          compileIfReady :: ModSummary
                         -> Maybe SPState
                         -> [HImportDecl]
                         -> Fnk [ModSummary]
                         -> Fnk [ModSummary]
          compileIfReady summary mb_sp imports getReqs = do
            hsc_env <- getSession
            is <- mapM (findNotCompiledImport hsc_env acc) imports
            let is' = catMaybes is
            debugFnk (";;; filtered imports = " ++ show is')
            if not (null is')
               then do
                 -- Imported modules are not fully compiled yet. Move this
                 -- module to the end of summarised modules and recurse.
                 let summarised' = is' ++ summarised ++ [target]
                     i' = i + length is'
                     k' = k + length is'
                 go acc i' k' summarised' pendings
               else do
                 -- Ready to compile this target unit. Compile it, add the
                 -- returned ModSummary to accumulator, and continue.
                 reqs <- getReqs
                 summary' <- makeOne i k mb_sp summary reqs
                 -- Alternative: use required modules and module from graph up
                 -- to this.
                 --
                 -- let graph_upto_this =
                 --       topSortModuleGraph True mgraph (Just mn)
                 --     mgraph = mkModuleGraph' (summary:acc)
                 --     mn = ms_mod_name summary
                 --     deps = flattenSCCs graph_upto_this ++ reqs
                 -- summary' <- makeOne i k mb_sp summary deps
                 go (summary':acc) (i-1) k summarised pendings

    tsmn (ts, _) =
      case ts of
        FnkSource _ mn _ _ -> mn
        HsSource path      -> asModuleName path
        _                  -> "module-name-unknown"
    total = length pendings0

-- | Check whether recompilation is required, and compile the 'HsModule'
-- when the codes or dependency modules were updated.
makeOne :: Int -> Int -> Maybe SPState -> ModSummary
        -> [ModSummary] -> Fnk ModSummary
makeOne i total mb_sp summary required_summaries = timeIt label go
  where
    label = "MakeOne [" ++ mname ++ "]"
    mname = moduleNameString (ms_mod_name summary)
    go = do
      -- Keep current DynFlags.
      dflags0 <- getDynFlags

      -- Use cached dynflags from ModSummary before type check. Note that the
      -- cached dynflags is always used, no matter whether the module is updated
      -- or not. This is to support loading already compiled module objects with
      -- language extensions not set in current dynflags.
      let dflags1 = ms_hspp_opts summary
      setDynFlags dflags1

      up_to_date <- checkUpToDate summary required_summaries
      debugFnk (";;; makeOne: up_to_date=" ++ show up_to_date)
      let midx = total - i + 1
          src_modified | up_to_date = SourceUnmodified
                       | otherwise  = SourceModified
      summary' <- doMakeOne midx total mb_sp summary src_modified

      -- Restore the original DynFlags.
      modifySession
        (\e -> e { hsc_dflags = dflags0
                 , hsc_IC = (hsc_IC e) {ic_dflags = dflags0}})

      return summary'

-- | Compile single module.
doMakeOne :: Int -- ^ Module index number.
          -> Int -- ^ Total number of modules.
          -> Maybe SPState -- ^ State returned from parser.
          -> ModSummary -- ^ Summary of module to compile.
          -> SourceModified -- ^ Source modified?
          -> Fnk ModSummary -- ^ Updated summary.
doMakeOne i total mb_sp ms src_modified = do
  debugFnk ";;; Entering doMakeOne"

  hsc_env <- getSession
  fnkc_env <- getFnkEnv

  let dflags = ms_hspp_opts ms
      hsc_env' = hsc_env {hsc_dflags = dflags}
      loc = ms_location ms
      mod_name = ms_mod_name ms
      tryGetTimeStamp = liftIO . modificationTimeIfExists
      messager = envMessager fnkc_env
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
            compileOne' Nothing (Just messager) hsc_env' ms i total
                        mb_old_iface mb_old_linkable src_modified'
          _ <- addHomeModuleToFinder hsc_env mod_name loc
          return home_mod_info)
  modifySession
    (\e -> e {hsc_HPT = addToHpt hpt0 mod_name home_mod_info})

  -- Dump the module contents as haskell source when dump option were set and
  -- this is the first time for compiling the target Module.
  when (envDumpHs fnkc_env || isJust (envHsDir fnkc_env))
       (case lookupHpt hpt0 mod_name of
          Nothing -> dumpModSummary mb_sp ms
          Just _  -> return ())

  -- Update the time stamp of generated obj and hi files.
  mb_obj_date <- tryGetTimeStamp (ml_obj_file loc)
  mb_iface_date <- tryGetTimeStamp (ml_hi_file loc)

  debugFnk ";;; Finished doMakeOne"
  return ms { ms_obj_date = mb_obj_date
            , ms_iface_date = mb_iface_date }

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

-- XXX: Won't detect recompilation with 'require'd modules.
checkUpToDate :: ModSummary -> [ModSummary] -> Fnk Bool
checkUpToDate ms dependencies =
  case ms_obj_date ms of
    -- No object code, return 'True' here, deletege bytecode check to
    -- "compileOne'".
    Nothing -> return True

    -- Found timestamp for object code.
    Just obj_date -> do
      hpt <- hsc_HPT <$> getSession
      let mn = ms_mod_name ms
          sccs_without_self = filter (\m -> mn /= ms_mod_name m)
                                     dependencies
          object_ok m
            | Just t <- ms_obj_date m = t >= ms_hs_date ms
                                        && same_as_prev t
            | otherwise = False
          same_as_prev t =
            case lookupHpt hpt mn of
              Just hmi | Just l <- hm_linkable hmi ->
                         isObjectLinkable l && t == linkableTime l
              _other -> True
          up_to_date = obj_date >= ms_hs_date ms &&
                       all object_ok sccs_without_self

      return up_to_date

-- -- | 'True' if the object code and interface file were up to date,
-- -- otherwise 'False'.
-- checkUpToDate :: ModSummary -> [ModSummary] -> Fnk Bool
-- checkUpToDate _summary required_summaries = do
--   -- XXX: TODO. Currently always returning 'True'.  Want to detect the
--   -- up-to-date-ness of modules containing 'require' of home package
--   -- module, and up-to-date-ness of the required modules.
--   hsc_env <- getSession
--   compiled_in_req <- fmap envCompiledInRequire getFnkEnv
--   let f (mname, _hmi) = "\n;;;   " ++ moduleNameString mname
--   debugFnk (concat (";;; checkUpToDate.compiled_in_req:"
--                     : case compiled_in_req of
--                        [] -> [" none."]
--                        _  ->  map f compiled_in_req))
--   _ <- mapM (checkRequiredIface hsc_env) required_summaries
--   return True
--
-- checkRequiredIface
--   :: HscEnv
--   -> ModSummary
--   -> Fnk (RecompileRequired, Maybe ModIface)
-- checkRequiredIface hsc_env summary = do
--   let hpt = hsc_HPT hsc_env
--       mb_old_iface =
--         case lookupHpt hpt (ms_mod_name summary) of
--           Nothing -> Nothing
--           Just hm_info | isBootSummary summary -> Just iface
--                        | not (mi_boot iface) -> Just iface
--                        | otherwise -> Nothing
--                        where
--                          iface = hm_iface hm_info
--   (recomp, mb_iface) <- liftIO (checkOldIface hsc_env summary
--                                               SourceUnmodified
--                                               mb_old_iface)
--   debugFnk
--     (concat [ ";;; checkRequiredIface\n"
--             , ";;;   ", moduleNameString (ms_mod_name summary), ": "
--             , case recomp of
--                 UpToDate             -> "up to date"
--                 MustCompile          -> "must compile"
--                 RecompBecause reason -> reason])
--   return (recomp, mb_iface)

-- | Search 'ModSummary' of required module.
findRequiredModSummary :: HscEnv
                       -> Located String
                       -> Fnk (Maybe ModSummary)
findRequiredModSummary hsc_env lmname = do
  -- Searching in reachable source paths before imported modules, because we
  -- want to use the source modified time from home package modules, to support
  -- recompilation of a module which requiring other home package modules.
  mb_ts <- findTargetSourceMaybe lmname
  case mb_ts of
    Just ts -> mkReadTimeModSummary (emptyTargetUnit ts)
    Nothing -> do
      -- The module name should be found somewhere else than current target
      -- sources. If not, complain what's missing.
      let mname' = mkModuleName mname
          mname = unLoc lmname
      fresult <- liftIO (findImportedModule hsc_env mname' Nothing)
      case fresult of
        Found {} -> return Nothing
        _        -> failS ("Cannot find required module " ++ mname)

-- | Find not compiled module.
findNotCompiledImport :: HscEnv       -- ^ Current hsc environment.
                      -> [ModSummary] -- ^ List of accumulated
                                      -- 'ModSummary'.
                      -> HImportDecl  -- ^ The target module name to
                                      -- find.
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
      debugFnk
        (concat [";;; Found " ++ show mloc ++ ", " ++
                  moduleNameString (moduleName mdl) ++ "\n"
                , ";;;   moduleUnitId=" ++
                  show (moduleUnitId mdl) ++ "\n"
                , ";;;   myInstalledUnitId=" ++
                  showPpr dflags myInstalledUnitId])
      case ml_hs_file mloc of
        Just path | takeExtension path `elem` [".hs"] ->
                    if moduleName mdl `elem` map ms_mod_name acc
                       then return Nothing
                       else do
                         ts <- findTargetSource (L (getLoc idecl) name)
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
            -- linkable. The 'UnitId' been set from REPL need to be exactly same
            -- as the 'UnitId' of package, including the hash part.
            --
            -- mb_ts <- findTargetSourceMaybe name
            -- return (fmap emptyTargetUnit mb_ts)
            fmap (fmap emptyTargetUnit)
                 (findTargetSourceMaybe (L (getLoc idecl) name))
          | otherwise -> return Nothing
        where
          inSameUnit =
            myInstalledUnitId `installedUnitIdEq` moduleUnitId mdl
          notInAcc =
            moduleName mdl `notElem` map ms_mod_name acc
    _              -> do
      let loc = getLoc idecl
      mb_ts <- findTargetSourceMaybe (L loc name)
      case mb_ts of
        Just ts -> return (Just (emptyTargetUnit ts))
        Nothing -> do
          let doc = cannotFindModule dflags mname findResult
              err = mkErrMsg dflags loc neverQualify doc
          throwOneError err
  where
    name = import_name idecl
    mname = mkModuleName name

-- | Make list of 'ModSummary' for read time dependency analysis.
mkReadTimeModSummaries :: [TargetUnit] -> Fnk [ModSummary]
mkReadTimeModSummaries =
  timeIt "mkReadTimeModSummaries" .
  fmap catMaybes .
  mapM mkReadTimeModSummary

-- | Make 'ModSummary' for read time dependency analysis.
--
-- The 'ms_textual_imps' field of 'ModSummary' made with this function contains
-- modules reffered by `require' keyword, not the modules referred by Haskell's
-- `import'. Purpose of this function is to resolve dependency of home package
-- modules for macro expansion.
mkReadTimeModSummary :: TargetUnit -> Fnk (Maybe ModSummary)
mkReadTimeModSummary (target, mbphase) =
  -- GHC.getModSummary is not ready at this point, since the module dependency
  -- graph is not yet created. Making possibly temporary ModSummary from target
  -- source.
  case target of
    HsSource file -> do
      mb_mdl <- compileToHsModule (target, mbphase)
      case mb_mdl of
        Nothing          -> return Nothing
        Just (mdl, _, _) -> fmap Just (mkModSummary (Just file) mdl)
    FnkSource file mn _form _sp -> do
      ms <- mkModSummary' (Just file) (mkModuleName mn) [] Nothing
      return (Just ms)
    OtherSource _ -> return Nothing

-- | Make 'ModSummary'. 'UnitId' is main unit.
mkModSummary :: GhcMonad m => Maybe FilePath -> HModule -> m ModSummary
mkModSummary mbfile mdl =
  let modName = case hsmodName mdl of
                  Just name -> unLoc name
                  Nothing   -> mkModuleName "Main"
      imports = map (ideclName . unLoc) (hsmodImports mdl)
      emptyAnns = (Map.empty, Map.empty)
      file = fromMaybe "<unknown>" mbfile
      r_s_loc = mkSrcLoc (fsLit file) 1 1
      r_s_span = mkSrcSpan r_s_loc r_s_loc
      pm = HsParsedModule
        { hpm_module = L r_s_span mdl
        , hpm_src_files = maybeToList mbfile
        , hpm_annotations = emptyAnns }
  in  mkModSummary' mbfile modName imports (Just pm)

-- | Make 'ModSummary' from source file, module name, and imports.
mkModSummary' :: GhcMonad m
              => Maybe FilePath -> ModuleName
              -> [Located ModuleName] -> Maybe HsParsedModule
              -> m ModSummary
mkModSummary' mbfile modName imports mb_pm = do
  dflags0 <- getDynFlags
  let fn = fromMaybe "anonymous" mbfile
      unitId = thisPackage dflags0
      mmod = mkModule unitId modName
      imported = map (\x -> (Nothing, x)) imports
      tryGetObjectDate path =
        if isObjectTarget (hscTarget dflags0)
           then modificationTimeIfExists path
           else return Nothing
  liftIO
    (do mloc <- mkHomeModLocation dflags0 modName fn
        hs_date <- maybe getCurrentTime getModificationUTCTime mbfile
        obj_date <- tryGetObjectDate (ml_obj_file mloc)
        iface_date <- modificationTimeIfExists (ml_hi_file mloc)
        dflags1 <-
          if isHsFile fn
             then do
               opts <- getOptionsFromFile dflags0 fn
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
                          , ms_hspp_file = fn
                          , ms_hspp_opts = dflags1
                          , ms_hspp_buf = Nothing })

-- | Dump the module contents of given 'ModSummary'.
dumpModSummary :: Maybe SPState -> ModSummary -> Fnk ()
dumpModSummary mb_sp ms = maybe (return ()) work (ms_parsed_mod ms)
  where
    work pm
      | isFnkFile orig_path = do
        contents <- gen pm
        fnkc_env <- getFnkEnv
        when (envDumpHs fnkc_env)
             (liftIO
                (do putStrLn (unwords [colons, orig_path, colons])
                    putStrLn ""
                    putStr contents))
        case envHsDir fnkc_env of
          Just dir -> doWrite dir contents
          Nothing  -> return ()
      | otherwise           = return ()
    doWrite dir contents = do
       let mname = moduleName (ms_mod ms)
           bname = takeBaseName orig_path
           file_name
             | looksLikeModuleName bname = moduleNameSlashes mname
             | otherwise                 = bname
           out_path = dir </> file_name <.> "hs"
           out_dir = takeDirectory out_path
       debugFnk (";;; Writing to " ++ out_path)
       liftIO (do createDirectoryIfMissing True out_dir
                  writeFile out_path contents)
    gen pm = genHsSrc sp (Hsrc (unLoc (hpm_module pm)))
    orig_path = ms_hspp_file ms
    sp = fromMaybe dummy_sp mb_sp
    dummy_sp = initialSPState (fsLit orig_path) 1 1
    colons = replicate 12 ';'

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

compileToHsModule :: TargetUnit
                  -> Fnk (Maybe (HModule, DynFlags, [Located String]))
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
                     -> Fnk (HModule, DynFlags, [Located String])
compileFnkModuleForm sp modname forms = do
  dflags0 <- getDynFlagsFromSPState sp
  hsc_env <- getSession

  -- Compile the form with file specific DynFlags and temporary session, to
  -- preserve modules imported in current context.
  let use_my_dflags e = e {hsc_dflags = dflags0}
  (mdl, reqs, compiled) <- withTempSession use_my_dflags act

  -- Add the compiled home modules to current session, if any. This fill avoid
  -- recompilation of required modules with "-fforce-recomp" option, which is
  -- required more than once.
  let hpt0 = hsc_HPT hsc_env
      hpt1 = addHomeModInfoIfMissing hpt0 compiled
  setSession (hsc_env {hsc_HPT = hpt1})

  debugFnk (";;; reqs=" ++ show (map unLoc reqs))
  return (mdl, dflags0, reverse reqs)
  where
    act = timeIt ("FinkelModule [" ++ modname ++ "]") $ do

      -- Reset current FnkEnv. No need to worry about managing interactive
      -- context and DynFlags, because this action is wrapped by
      -- 'withTempSession' above.
      resetFnkEnv

      mdl <- compileFnkModuleForm' forms
      fnkc_env <- getFnkEnv
      let required = envRequiredModuleNames fnkc_env
          compiled = envCompiledInRequire fnkc_env
      return (mdl, required, compiled)

-- -- | Compile 'HModule' from given list of codes.
compileFnkModuleForm' :: [Code] -> Fnk HModule
compileFnkModuleForm' form = do
  expanded <- withExpanderSettings (expands form)
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
addHomeModInfoIfMissing :: HomePackageTable
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
                            , envRequiredModuleNames = []
                            , envMakeDynFlags = Nothing})

compileHsFile :: FilePath -> Maybe Phase
               -> Fnk (HModule, DynFlags, [a])
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
  debugFnk (";;; Compiling other code: " ++ path)
  hsc_env <- getSession
  liftIO (oneShot hsc_env StopLn [(path, Nothing)])

-- | Link 'ModSummary's, when required.
doLink :: [ModSummary] -> Fnk ()
doLink mgraph = do
  guessOutputFile mgraph
  hsc_env <- getSession
  let dflags1 = hsc_dflags hsc_env
      main_mod = mainModIs dflags1
      root_has_Main = any ((== main_mod) . ms_mod) mgraph
      no_hs_main = gopt Opt_NoHsMain dflags1
      doLinking = root_has_Main ||
                  no_hs_main ||
                  ghcLink dflags1 == LinkDynLib ||
                  ghcLink dflags1 == LinkStaticLib
  linkResult <-
    liftIO (link (ghcLink dflags1) dflags1 doLinking (hsc_HPT hsc_env))
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

-- | Modify the order of 'TargetUnit' by referencing the order of given
-- 'ModSummary'.
sortTargets :: [ModSummary] -- ^ Referenced list of 'ModSummary'.
            -> [TargetUnit] -- ^ List of 'TargetUnit' to reorder.
            -> [TargetUnit] -- ^ Reordered result.
sortTargets summaries targets = foldr f [] summaries
  where
    f summary acc =
      let mloc = ms_location summary
          mb_path = ml_hs_file mloc
          byPath p a = targetSourcePath (fst a) == p
      in  case mb_path of
            Nothing   -> error ("sortTargets: no target " ++ show mloc)
            Just path ->
              case find (byPath path) targets of
                Nothing -> error ("sortTargets: no target " ++ path)
                Just tu -> tu:acc

-- | Label and wrap the given action with 'withTiming'.
timeIt :: String -> Fnk a -> Fnk a
#if MIN_VERSION_ghc(8,10,0)
timeIt label = withTimingD (text label) (const ())
#else
timeIt label = withTiming getDynFlags (text label) (const ())
#endif

-- | Get module name from import declaration.
import_name :: HImportDecl -> String
import_name = moduleNameString . unLoc . ideclName . unLoc

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

-- | GHC version compatibility helper function for creating
-- 'ModuleGraph' from list of 'ModSummary's.
mkModuleGraph' :: [ModSummary] -> ModuleGraph
#if MIN_VERSION_ghc(8,4,0)
mkModuleGraph' = mkModuleGraph
#else
mkModuleGraph' = id
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

-- | Show some fields in 'DynFlags'.
dumpDynFlags :: DynFlags -> Fnk ()
dumpDynFlags dflags =
  debugFnk
    (concat
       [ ";;; ghcLink=", show (ghcLink dflags), "\n"
       , ";;; ghcMode=", showPpr dflags (ghcMode dflags), "\n"
       , ";;; hscTarget=", show (hscTarget dflags), "\n"
       , ";;; ways=", show (ways dflags), "\n"
       , ";;; forceRecomp=", show (gopt Opt_ForceRecomp dflags), "\n"
       , ";;; interpWays=", show interpWays, "\n"
       , ";;; importPaths=", show (importPaths dflags)])
