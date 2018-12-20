{-# LANGUAGE BangPatterns, CPP #-}
-- | Make mode for skc.
module Language.SK.Make
  ( make
  , initSessionForMake
  , defaultSkEnv
  ) where

-- base
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO(..))
import Data.List (find, foldl')
import Data.Maybe (catMaybes, fromMaybe, isJust)

-- bytestring
import qualified Data.ByteString.Lazy.Char8 as BL

-- container
import Data.Graph (flattenSCCs)

-- directory
import System.Directory (createDirectoryIfMissing)

-- filepath
import System.FilePath ( dropExtension, splitExtension
                       , takeBaseName, takeDirectory, takeExtension
                       , (<.>), (</>))

-- ghc
import BasicTypes (SuccessFlag(..))
import DriverPhases (Phase(..))
import DriverPipeline (compileOne', link, oneShot, preprocess)
import DynFlags ( DynFlags(..), GeneralFlag(..), GhcLink(..)
                , GhcMode(..), getDynFlags, gopt, gopt_set
                , gopt_unset, interpWays )
import ErrUtils (mkErrMsg, withTiming)
import Exception (SomeException, gtry, tryIO)
import Finder ( addHomeModuleToFinder, cannotFindModule
              , findImportedModule, findObjectLinkableMaybe )
import GHC ( setSessionDynFlags )
import GhcMake (topSortModuleGraph)
import GhcMonad ( GhcMonad(..), getSessionDynFlags, modifySession
                , withTempSession )
import Outputable ( neverQualify, text, showPpr )
import HsImpExp (ImportDecl(..))
import HsSyn (HsModule(..))
import HscTypes ( FindResult(..), ModIface(..), ModSummary(..)
                , HomeModInfo(..), HomePackageTable, HsParsedModule(..)
                , HscEnv(..), InteractiveContext(..)
                , ModuleGraph, SourceModified(..)
                , addToHpt, eltsHpt, lookupHpt, ms_mod_name
                , throwOneError
#if MIN_VERSION_ghc(8,4,0)
                , mkModuleGraph, extendMG
#endif
                )
import Module ( ModLocation(..), ModuleName, installedUnitIdEq
              , mkModuleName, moduleName, moduleNameSlashes
              , moduleNameString, moduleUnitId )
import Panic (GhcException(..), throwGhcException)
import SrcLoc (getLoc, mkRealSrcLoc, noSrcSpan)
import StringBuffer (stringToStringBuffer)
import Util (getModificationUTCTime, looksLikeModuleName)

import qualified Parser as GHCParser
import qualified Lexer as GHCLexer

-- ghc-paths
import GHC.Paths (libdir)

-- internal
import Language.SK.Builder
import Language.SK.Emit
import Language.SK.Expand
import Language.SK.Form
import Language.SK.Lexer
import Language.SK.Run
import Language.SK.SKC
import Language.SK.TargetSource


-- ---------------------------------------------------------------------
--
-- Exported make interface
--
-- ---------------------------------------------------------------------

-- [Requiring home package module]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- The problem in dependency resolution when requiring home package
-- module is, we need module imports list to make ModSummary, but
-- modules imports could not be obtained unless the source code is macro
-- expanded. However, macro-expansion may use macros from other home
-- package modules, which are not loaded to GHC session yet.
--
-- Currently, compilation is done with recursively calling 'make'
-- function from 'require' macro, during macro-expansion.
--
-- Once these dependency resolution works were tried with custom user
-- hooks in cabal setup script. However, as of Cabal version 1.24.2,
-- building part of some modules from contents of cabal configuration
-- file were not so easy. Though when cabal support multiple libraraies,
-- situation might change.

-- | SK variant of @ghc --make@.
make :: [(FilePath, Maybe Phase)] -- ^ List of input file and phase
     -> Bool -- ^ Skip linking when 'True'.
     -> Bool -- ^ Force recompilation when 'True'.
     -> Maybe FilePath -- ^ Output file, if any.
     -> Skc ()
make infiles no_link force_recomp mb_output = do

  -- Setting ghcMode as done in ghc's "Main.hs".
  --
  -- Also setting force recompilation field from argument, since ghc
  -- running in OneShot mode instead of CompManager mode until this
  -- point. Some of the dump flags will turn force recompilation flag
  -- on. Ghc does this switching off of recompilation checker in
  -- DynFlags.{setDumpFlag',forceRecompile}.
  dflags0 <- getSessionDynFlags
  let dflags1 = dflags0 { ghcMode = CompManager
                        , outputFile = mb_output }
      dflags2 | force_recomp = gopt_set dflags1 Opt_ForceRecomp
              | otherwise    = gopt_unset dflags1 Opt_ForceRecomp
  _ <- setSessionDynFlags dflags2
  dflags3 <- getSessionDynFlags

  debugSkc
    (concat
       [ ";;; make:\n"
       , ";;;   ghcLink=", show (ghcLink dflags3), "\n"
       , ";;;   ghcMode=", showPpr dflags3 (ghcMode dflags3), "\n"
       , ";;;   hscTarget=", show (hscTarget dflags3), "\n"
       , ";;;   ways=", show (ways dflags3), "\n"
       , ";;;   forceRecomp=", show (gopt Opt_ForceRecomp dflags3), "\n"
       , ";;;   interpWays=", show interpWays])

  -- Preserve the language extension values in initial dynflags to
  -- SkEnv, to reset the language extension later, to keep fresh set of
  -- language extensios per module.
  let lexts = (language dflags3, extensionFlags dflags3)
  modifySkEnv (\e -> e {envDefaultLangExts = lexts})

  -- Decide the kind of sources of the inputs, inputs arguments could be
  -- file paths, or module names.
  sources <- mapM findTargetSource infiles

  -- Do the compilation work.
  mod_summaries <- make' [] sources

  -- Update current module graph for linker. Linking work is delegated
  -- to deriver pipelin's `link' function.
  let mgraph = mkModuleGraph' mod_summaries
      mgraph_flattened =
        flattenSCCs (topSortModuleGraph True mgraph Nothing)
  unless no_link (doLink mgraph_flattened)

-- | Calls 'GHC.setSessionDynFlags' to initialize session.
initSessionForMake :: Skc ()
initSessionForMake = do
  -- Returned list of 'InstalledUnitId's are ignored.
  _ <- getSessionDynFlags >>= setSessionDynFlags

  -- Load modules names in SkEnv to current interactive context.
  sk_env <- getSkEnv
  let ctx_modules = envContextModules sk_env
  unless (null ctx_modules) (setContextModules ctx_modules)

  -- Debug information could be specified from environment variable and
  -- command line option.
  debug0 <- getSkcDebug
  let debug1 = envDebug sk_env
  putSkEnv (sk_env {envDebug = debug0 || debug1})

-- | Simple make function returning compiled home module
-- information. Intended to be used for 'envMake' field in 'SkEnv'.
simpleMake :: Bool -> String -> Skc [(ModuleName, HomeModInfo)]
simpleMake recomp name = do
  make [(name, Nothing)] True recomp Nothing
  hsc_env <- getSession
  let as_pair hmi = (moduleName (mi_module (hm_iface hmi)), hmi)
  return (map as_pair (eltsHpt (hsc_HPT hsc_env)))

-- | Default 'SkEnv'.
defaultSkEnv :: SkEnv
defaultSkEnv = emptySkEnv
  { envMacros         = specialForms
  , envDefaultMacros  = specialForms
  , envMake           = Just simpleMake
  , envLibDir         = Just libdir }


-- ---------------------------------------------------------------------
--
-- Internal of make
--
-- ---------------------------------------------------------------------

-- | Unit for compilation target.
--
-- Simply a 'TargetSource' maybe paired with 'Phase'.
type TargetUnit = (TargetSource, Maybe Phase)

-- | Compile 'TargetUnit' to 'ModSummary' and 'HsModule', then compile
-- to interface file, and object code.
--
-- Do macro expansion and get the Haskell `import' declarations from
-- parsed source contents. If the macro expanded result does not
-- contain imports from pending modules, compile to '*.hi' and
-- '*.o'. If the HsModule contained imports of pending module, add the
-- module to the pending modules.
--
make' :: [TargetUnit] -> [TargetUnit] -> Skc [ModSummary]
make' readys0 pendings0 = do
  debugSkc
    (concat [ ";;; make'\n"
            , ";;;   total: " ++ show total ++ "\n"
            , ";;;   readys0: " ++ show readys0 ++ "\n"
            , ";;;   pendings0: " ++ show pendings0])
  timeIt "make' [sk]" (go [] total total readys0 pendings0)
  where
    go :: [ModSummary] -- ^ Accumulator.
       -> Int          -- ^ module n ...
       -> Int          -- ^ ... of m.
       -> [TargetUnit] -- ^ Ready to compile targets.
       -> [TargetUnit] -- ^ Pending targets.
       -> Skc [ModSummary]

    -- No more modules to compile, return the accumulated ModSummary.
    go acc i _  _  _ | i <= 0 = return acc
    go acc _ _ [] [] = return acc

    -- Target modules are empty, ready to compile pending modules to
    -- read time ModSummary.  Partition the modules, make read time
    -- ModSummaries, then sort via topSortModuleGraph, and recurse.
    go acc i k [] pendings
      | all (isOtherSource . fst) pendings =
         -- All targets are other source, no need to worry about module
         -- dependency analysis.
         go acc i k pendings []
      | otherwise = do
         -- Mixed target sources in pending modules. Make
         -- read-time-mod-summaries from pending modules, make new
         -- graph, sort it, then recurse.
         let (readies', pendings') = (pendings, [])
         rt_mss <- mkReadTimeModSummaries readies'
         let pre_graph = mkModuleGraph' rt_mss
             graph = topSortModuleGraph True pre_graph Nothing
             readies'' = sortTargets (flattenSCCs graph) readies'
         go acc i k readies'' pendings'

    -- Compile ready-to-compile targets to ModSummary and
    -- HsModule. Input could be SK source code, Haskell source code, or
    -- something else. If SK source code or Haskell source code, get
    -- ModSummary to resolve the dependencies.
    go acc i k (target@(tsr,_mbp):summarised) pendings = do

      -- Since skc make is not using 'DriverPipeline.runPipeline',
      -- setting 'DynFlags.dumpPrefix' manually.
      setDumpPrefix (targetSourcePath tsr)

      case tsr of
        SkSource path _mn _form sp -> do
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
                  notYetReady =
                    any (\m -> m `elem` pending_names ||
                               m `elem` summarised_names)
                        import_names

              debugSkc (concat [ ";;; target=", show target, "\n"
                               , ";;; imports=", show import_names])

              -- Test whether imported modules are in pendings. If
              -- found, skip the compilation and add this module to the
              -- list of pending modules.
              --
              -- N.B. For SK source target, dependency modules passed to
              -- 'makeOne' contains imported modules and required
              -- modules.
              --
              if notYetReady
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
                         -> Skc [ModSummary]
                         -> Skc [ModSummary]
          compileIfReady summary mb_sp imports getReqs = do
            hsc_env <- getSession
            is <- mapM (findUnCompiledImport hsc_env acc) imports
            let is' = catMaybes is
            debugSkc (";;; filtered imports = " ++ show is')
            if not (null is')
               then do
                 -- Imported modules are not fully compiled yet. Move
                 -- this module to the end of summarised modules and
                 -- recurse.
                 let summarised' = is' ++ summarised ++ [target]
                     i' = i + length is'
                     k' = k + length is'
                 go acc i' k' summarised' pendings
               else do
                 -- Ready to compile this target unit. Compile it, add
                 -- the returned ModSummary to accumulator, and
                 -- continue.
                 reqs <- getReqs
                 let graph_upto_this =
                       topSortModuleGraph True mgraph (Just mn)
                     mgraph = mkModuleGraph' (summary:acc)
                     mn = ms_mod_name summary
                     deps = flattenSCCs graph_upto_this ++ reqs
                 summary' <- makeOne i k mb_sp summary deps
                 go (summary':acc) (i-1) k summarised pendings

    tsmn (ts, _) =
      case ts of
        SkSource _ mn _ _ -> mn
        HsSource path     -> asModuleName path
        _                 -> "module-name-unknown"
    total = length readys0 + length pendings0

-- | Check whether recompilation is required, and compile the 'HsModule'
-- when the codes or dependency modules were updated.
makeOne :: Int -> Int -> Maybe SPState -> ModSummary
        -> [ModSummary] -> Skc ModSummary
makeOne i total mb_sp ms graph_upto_this = timeIt label go
  where
    label = "MakeOne [" ++ mname ++ "]"
    mname = moduleNameString (ms_mod_name ms)
    go = do
      -- Keep current DynFlags.
      dflags <- getDynFlags

      -- Use cached dynflags from ModSummary before type check. Note
      -- that the cached dynflags is always used, no matter whether the
      -- module is updated or not. This is to support loading already
      -- compiled module objects with language extensions not set in
      -- current dynflags.
      setDynFlags (ms_hspp_opts ms)

      up_to_date <- checkUpToDate ms graph_upto_this
      let midx = total - i + 1
          src_modified | up_to_date = SourceUnmodified
                       | otherwise  = SourceModified
      ms' <- doMakeOne midx total mb_sp ms src_modified

      -- Update module graph with new ModSummary, and restore the
      -- original DynFlags.
      modifySession
        (\e -> e { hsc_mod_graph = extendMG' (hsc_mod_graph e) ms'
                 , hsc_dflags = dflags
                 , hsc_IC = (hsc_IC e) {ic_dflags=dflags}})

      return ms'

-- | Compile single module.
doMakeOne :: Int -- ^ Module index number.
          -> Int -- ^ Total number of modules.
          -> Maybe SPState -- ^ State returned from parser.
          -> ModSummary -- ^ Summary of module to compile.
          -> SourceModified -- ^ Source modified?
          -> Skc ModSummary -- ^ Updated summary.
doMakeOne i total mb_sp ms src_modified = do
  debugSkc ";;; Entering doMakeOne"

  hsc_env <- getSession
  sk_env <- getSkEnv

  let dflags_orig = hsc_dflags hsc_env
      dflags = ms_hspp_opts ms
      hsc_env' = hsc_env {hsc_dflags = dflags}
      loc = ms_location ms
      mod_name = ms_mod_name ms
      tryGetTimeStamp x = liftIO (tryIO (getModificationUTCTime x))
      e2mb x = case x of
                 Right a -> Just a
                 Left _  -> Nothing
      messager = envMessager sk_env
      hpt0 = hsc_HPT hsc_env

  -- Compile the module with linkable if exist, and add the returned
  -- module to finder.
  mb_linkable <- liftIO (findObjectLinkableMaybe (ms_mod ms) loc)
  home_mod_info <-
    liftIO (compileOne' Nothing (Just messager) hsc_env' ms
                        i total Nothing mb_linkable src_modified)
  _ <- liftIO (addHomeModuleToFinder hsc_env mod_name loc)
  modifySession
    (\e -> e {hsc_HPT = addToHpt hpt0 mod_name home_mod_info})

  -- Dump the module contents as haskell source when dump option were
  -- set and this is the first time for compiling the target Module.
  when (envDumpHs sk_env || isJust (envHsDir sk_env))
       (case lookupHpt hpt0 mod_name of
          Nothing -> dumpModSummary mb_sp ms
          Just _  -> return ())

  -- Update the time stamp of generated obj and hi files.
  mb_obj_date <- e2mb <$> tryGetTimeStamp (ml_obj_file loc)
  mb_iface_date <- e2mb <$> tryGetTimeStamp (ml_hi_file loc)
  setDynFlags dflags_orig

  return ms { ms_obj_date = mb_obj_date
            , ms_iface_date = mb_iface_date }

-- [Avoiding Recompilation]
-- ~~~~~~~~~~~~~~~~~~~~~~~~
--
-- See below for details of how GHC avoid recompilation:
--
--   https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/RecompilationAvoidance
--
-- The logic used in below function `checkUpToDate' is almost same as in
-- 'GhcMake.checkStability', but for object codes only, and the
-- arguments are different.
--
-- Other than this, most recompilation checks are done in
-- "DriverPipeLine.compileOne'" function, which is called in
-- "doMakeOne".

-- | 'True' if the object code and interface file were up to date,
-- otherwise 'False'.
checkUpToDate :: ModSummary -> [ModSummary] -> Skc Bool
checkUpToDate ms dependencies
  | Nothing <- ms_iface_date ms = return False
  | otherwise =
    case ms_obj_date ms of
      Nothing       -> return False
      Just obj_date -> do
        let mn = ms_mod_name ms
            sccs_without_self = filter (\m -> mn /= ms_mod_name m)
                                       dependencies
            dep_ok = maybe False (obj_date >=) . ms_obj_date
            up_to_date = obj_date >= ms_hs_date ms &&
                         all dep_ok sccs_without_self
        return up_to_date

-- | Find 'TargetSource' from command line argument. This function
-- throws 'SkException' when the target source was not found.
findTargetSource :: (String, a) -> Skc (TargetSource, a)
findTargetSource (modNameOrFilePath, a) = do
  dflags <- getSessionDynFlags
  mb_inputPath <-
    findFileInImportPaths (importPaths dflags) modNameOrFilePath
  let detectSource path
        | isSkFile path =
          do contents <- liftIO (BL.readFile path)
             (forms, sp) <- parseSexprs (Just path) contents
             let modName = asModuleName modNameOrFilePath
             return (SkSource path modName forms sp, a)
        | isHsFile path = return (HsSource path, a)
        | otherwise = return (OtherSource path, a)
  case mb_inputPath of
    Just path -> detectSource path
    Nothing   -> do
      let err = mkErrMsg dflags noSrcSpan neverQualify doc
          doc = text ("cannot find target source: " ++ modNameOrFilePath)
      throwOneError err

-- | Like 'findTargetSource', but the result wrapped in 'Maybe'.
findTargetSourceMaybe :: (String, a) -> Skc (Maybe (TargetSource, a))
findTargetSourceMaybe (modName, a) = do
  et_ret <- gtry (findTargetSource (modName, a))
  case et_ret of
    Right found -> return (Just found)
    Left _err   -> let _err' = _err :: SomeException
                   in  return Nothing

-- | Search 'ModSummary' of required module.
findRequiredModSummary :: HscEnv -> String -> Skc (Maybe ModSummary)
findRequiredModSummary hsc_env mname = do
  -- Searching in reachable source paths before imported modules,
  -- because we want to use the source modified time from home package
  -- modules, to support recompilation of a module which requiring other
  -- home package modules.
  mb_tu <- findTargetSourceMaybe (mname, Nothing)
  case mb_tu of
    Just tu -> mkReadTimeModSummary tu
    Nothing -> do
      -- The module name should be found somewhere else than current
      -- target sources. If not, complain what's missing.
      let mname' = mkModuleName mname
      fresult <- liftIO (findImportedModule hsc_env mname' Nothing)
      case fresult of
        Found {} -> return Nothing
        _        -> failS ("Cannot find required module " ++ mname)

-- | Find not compiled module.
findUnCompiledImport :: HscEnv       -- ^ Current hsc environment.
                     -> [ModSummary] -- ^ List of accumulated
                                     -- 'ModSummary'.
                     -> HImportDecl  -- ^ The target module name to
                                     -- find.
                     -> Skc (Maybe TargetUnit)
findUnCompiledImport hsc_env acc idecl = do
  findResult <- liftIO (findImportedModule hsc_env mname Nothing)
  dflags <- getSessionDynFlags
  let myInstalledUnitId = thisInstalledUnitId dflags
  case findResult of
    -- Haskell module returned by `Finder.findImportedModule' may not
    -- compiled yet. If the source code has Haskell file extension,
    -- checking whether the module is listed in accumulator containing
    -- compiled modules.
    Found mloc mdl -> do
      debugSkc
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
                       else Just <$> findTargetSource (name, Nothing)
        _ | inSameUnit && notInAcc ->
            -- Workaround for loading home package modules when
            -- working with cabal package from REPL.
            -- 'Finder.findImportedModule' uses hard coded source file
            -- extensions in `Finder.findInstalledHomeModule' to find
            -- Haskell source codes of home package module, which will
            -- not find SK source files. When looking up modules in
            -- home package as dependency, looking up in accumurated
            -- ModSummary list to avoid using the modules found in
            -- already compiled linkable package. The `linkable'
            -- mentioned here are those 'libXXX.{a,dll,so}' artifact
            -- files for library package components.
            --
            -- This workaround needs the 'UnitId' from REPL session to
            -- be set via "-this-unit-id" DynFlag option, to avoid
            -- loading modules from linkable. The 'UnitId' been set
            -- from REPL need to be exactly same as the 'UnitId' of
            -- package, including the hash part.
            --
            findTargetSourceMaybe (name, Nothing)
          | otherwise -> return Nothing
        where
          inSameUnit =
            myInstalledUnitId `installedUnitIdEq` moduleUnitId mdl
          notInAcc =
            moduleName mdl `notElem` map ms_mod_name acc
    _              -> do
      mb_tu <- findTargetSourceMaybe (name, Nothing)
      case mb_tu of
        tu@(Just _) -> return tu
        Nothing     -> do
          let loc = getLoc idecl
              doc = cannotFindModule dflags mname findResult
              err = mkErrMsg dflags loc neverQualify doc
          throwOneError err
  where
    name = import_name idecl
    mname = mkModuleName name

-- | Make list of 'ModSummary' for read time dependency analysis.
mkReadTimeModSummaries :: [TargetUnit] -> Skc [ModSummary]
mkReadTimeModSummaries =
  timeIt "mkReadTimeModSummaries" .
  fmap catMaybes .
  mapM mkReadTimeModSummary

-- | Make 'ModSummary' for read time dependency analysis.
--
-- The 'ms_textual_imps' field of 'ModSummary' made with this function
-- contains modules reffered by `require' keyword, not the modules
-- referred by Haskell's `import'. Purpose of this function is to
-- resolve dependency of home package modules for macro expansion.
mkReadTimeModSummary :: TargetUnit -> Skc (Maybe ModSummary)
mkReadTimeModSummary (target, mbphase) =
  -- GHC.getModSummary is not ready at this point, since the module
  -- dependency graph is not yet created. Making possibly temporary
  -- ModSummary from target source.
  case target of
    HsSource file -> do
      mb_mdl <- compileToHsModule (target, mbphase)
      case mb_mdl of
        Nothing          -> return Nothing
        Just (mdl, _, _) -> fmap Just (mkModSummary (Just file) mdl)
    SkSource file mn _form _sp -> do
      ms <- mkModSummary' (Just file) (mkModuleName mn) [] Nothing
      return (Just ms)
    OtherSource _ -> return Nothing

compileToHsModule :: TargetUnit
                  -> Skc (Maybe (HModule, DynFlags, [String]))
compileToHsModule (tsrc, mbphase) =
  case tsrc of
    SkSource _ mn form sp -> Just <$> compileSkModuleForm' sp mn form
    HsSource path         -> Just <$> compileHsFile path mbphase
    OtherSource path      -> compileOtherFile path >> return Nothing

-- | Wrapper for 'compileSkModuleForm', to use fresh set of modules,
-- language extensions, and macros in 'SkEnv'. Returns tuple of compiled
-- module and 'Dynflags' to update 'ModSummary'.
compileSkModuleForm' :: SPState -> String -> [Code]
                     -> Skc (HModule, DynFlags, [String])
compileSkModuleForm' sp modname forms = do
  dflags0 <- getDynFlagsFromSPState sp
  hsc_env <- getSession

  -- Compile the form with file specific DynFlags and temporary session,
  -- to preserve modules imported in current context.
  let use_my_dflags e = e {hsc_dflags = dflags0}
  (mdl, reqs, compiled) <- withTempSession use_my_dflags act

  -- Add the compiled home modules to current sessio, if any. This fill
  -- avoid recompilation of required modules with "-fforce-recomp"
  -- option, which is required more than once.
  let hpt0 = hsc_HPT hsc_env
      hpt1 = addHomeModInfoIfMissing hpt0 compiled
  setSession (hsc_env {hsc_HPT = hpt1})

  debugSkc (";;; reqs=" ++ show reqs)
  return (mdl, dflags0, reverse reqs)
  where
    act = timeIt ("SkModule [" ++ modname ++ "]") $ do

      -- Reset SkEnv, no need to worry about managing interactive
      -- context and DynFlags because this action is wrapped by
      -- 'withTempSession' above.
      resetSkEnv

      mdl <- compileSkModuleForm forms
      sk_env <- getSkEnv
      let required = envRequiredModuleNames sk_env
          compiled = envCompiledInRequire sk_env
      return (mdl, required, compiled)

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

-- | Reset macros and required modules in current SkEnv.
resetSkEnv :: Skc ()
resetSkEnv =
  modifySkEnv (\ske -> ske { envMacros = envDefaultMacros ske
                           , envRequiredModuleNames = []})

compileHsFile :: FilePath -> Maybe Phase
               -> Skc (HModule, DynFlags, [a])
compileHsFile source mbphase = do
  hsc_env <- getSession
  (dflags, source') <- liftIO (preprocess hsc_env (source, mbphase))
  contents <- liftIO (readFile source')
  let location = mkRealSrcLoc (fsLit source) 1 1
      sbuf = stringToStringBuffer contents
      parseState = GHCLexer.mkPState dflags sbuf location
  case GHCLexer.unP GHCParser.parseModule parseState of
    GHCLexer.POk _ m    -> return (unLoc m, dflags, [])
    GHCLexer.PFailed {} -> failS ("Parser error with " ++ source)

compileOtherFile :: FilePath -> Skc ()
compileOtherFile path = do
  debugSkc (";;; Compiling other code: " ++ path)
  hsc_env <- getSession
  liftIO (oneShot hsc_env StopLn [(path, Nothing)])

-- | Link 'ModSummary's, when required.
doLink :: [ModSummary] -> Skc ()
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
    Failed    -> failS "Error during linking"
    Succeeded -> return ()

-- | Set 'dumpPrefix' from file path.
setDumpPrefix :: FilePath -> Skc ()
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
timeIt :: String -> Skc a -> Skc a
timeIt label = withTiming getDynFlags (text label) (const ())

-- | Get module name from import declaration.
import_name :: HImportDecl -> String
import_name = moduleNameString . unLoc . ideclName . unLoc

-- | Dump the module contents of given 'ModSummary'.
dumpModSummary :: Maybe SPState -> ModSummary -> Skc ()
dumpModSummary mb_sp ms = maybe (return ()) work (ms_parsed_mod ms)
  where
    work pm
      | isSkFile orig_path = do
        contents <- gen pm
        sk_env <- getSkEnv
        when (envDumpHs sk_env) (liftIO (putStr contents))
        case envHsDir sk_env of
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
       debugSkc (";;; Writing to " ++ out_path)
       liftIO (do createDirectoryIfMissing True out_dir
                  writeFile out_path contents)
    gen pm = genHsSrc sp (Hsrc (unLoc (hpm_module pm)))
    orig_path = ms_hspp_file ms
    sp = fromMaybe dummy_sp mb_sp
    dummy_sp = initialSPState (fsLit orig_path) 1 1

-- [guessOutputFile]
--
-- Following 'guessOutputFile' is mostly copied from "GhcMake.hs", it
-- was not exported. Modified to take [ModSummary] argument instead of
-- getting the module graph from session.

-- | If there is no -o option, guess the name of target executable
-- by using top-level source file name as a base.
guessOutputFile :: [ModSummary] -> Skc ()
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
         Just _ -> env
         Nothing -> env { hsc_dflags = dflags { outputFile = name_exe } }

-- | GHC version compatibility helper function for creating
-- 'ModuleGraph' from list of 'ModSummary's.
mkModuleGraph' :: [ModSummary] -> ModuleGraph
#if !MIN_VERSION_ghc(8,4,0)
mkModuleGraph' = id
#else
mkModuleGraph' = mkModuleGraph
#endif

-- | GHC version compatibility helper function for extending
-- 'ModuleGraph' with 'ModSummary'.
extendMG' :: ModuleGraph -> ModSummary -> ModuleGraph
#if !MIN_VERSION_ghc(8,4,0)
extendMG' mg ms = ms : mg
#else
extendMG' = extendMG
#endif
