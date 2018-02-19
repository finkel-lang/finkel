{-# LANGUAGE BangPatterns, CPP #-}
-- | Make mode for skc.
module Language.SK.Make
  ( asModuleName
  , make
  , initSessionForMake
  , TargetSource(..)
  ) where

-- base
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Char (isUpper)
import Data.List (find)
import Data.Maybe (catMaybes, fromMaybe)
import System.IO (fixIO)

-- bytestring
import qualified Data.ByteString.Lazy as BL

-- container
import Data.Graph (flattenSCCs)

-- ghc
import qualified Parser as GHCParser
import qualified Lexer as GHCLexer
import qualified Maybes as Maybes
import ErrUtils (withTiming)
import Outputable (text)
import HscMain (dumpIfaceStats)
import HscTypes (ModIface, ModDetails)
import TcIface (typecheckIface)
import TcRnMonad (initIfaceLoad)

-- directory
import System.Directory (doesFileExist)

-- filepath
import System.FilePath ( dropExtension, pathSeparator
                       , replaceExtension , splitExtension, splitPath
                       , takeExtension, (<.>), (</>))

-- internal
import Language.SK.Form
import Language.SK.GHC
import Language.SK.Lexer
import Language.SK.Run
import Language.SK.SKC

-- ---------------------------------------------------------------------
--
-- Exported main interface
--
-- ---------------------------------------------------------------------

-- [Requiring home package module]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- The problem in dependency resolution when requiring home package
-- module is, we need module imports list to make ModSummary, but
-- modules imports could not be obtained unless the source code is macro
-- expanded. However, macroexpansion may use macros from required
-- modules.
--
-- Currently, compilation is done with firstly partitioning the input SK
-- sources to modules which containing `require' syntax of home package
-- modules, and which doesn't.
--
-- Once the dependency resolution work were tried with custom user hooks
-- in cabal setup script. However, as of Cabal version 1.24.2, building
-- part of some modules from contents of cabal configuration file were
-- not so easy. Though when cabal support multiple libraraies, situation
-- might change.

-- | SK variant of @ghc --make@.
make :: [(FilePath, Maybe Phase)] -- ^ List of input file and phase
     -> Bool -- ^ Skip linking when 'True'.
     -> Maybe FilePath -- ^ Output file, if any.
     -> Skc ()
make inputs no_link mb_output = do
  -- Setting ghcMode as done in ghc's "Main.hs".
  dflags <- getDynFlags
  setDynFlags (dflags { ghcMode = CompManager
                      , outputFile = mb_output })
  debugIO
    (do putStrLn ";;; make:"
        putStrLn (";;;   ghcLink=" ++ show (ghcLink dflags))
        putStrLn (";;;   hscTarget=" ++ show (hscTarget dflags))
        putStrLn (";;;   ways=" ++ show (ways dflags)))

  -- Preserve the language extension values in initial dynflags to
  -- SkEnv, to reset the language extension later, to keep fresh set of
  -- language extensios per module.
  let lexts = (language dflags, extensionFlags dflags)
  modifySkEnv (\e -> e {envDefaultLangExts = lexts})

  -- Decide the kind of sources of the inputs.
  sources <- mapM findTargetSource inputs

  -- Assuming modules names were passed as arguments, inputs could be
  -- file paths, or module names.
  let to_compile = map fst inputs

  -- Do the compilation work.
  mod_summaries <- make' to_compile [] sources

  hpt2 <- hsc_HPT <$> getSession
  debugIO $ putStrLn (showSDoc dflags $ pprHPT hpt2)

  -- Update current module graph for linker. Linking work is delegated
  -- to deriver pipelin's `link' function.
  let mgraph_flattened =
        flattenSCCs (topSortModuleGraph True mod_summaries Nothing)
  unless no_link (doLink mgraph_flattened)

-- | Calls 'GHC.setSessionDynFlags' to initialize session.
initSessionForMake :: Skc ()
initSessionForMake = do
  -- Returned list of 'InstalledUnitId's are ignored.
  _ <- getSessionDynFlags >>= setSessionDynFlags
  return ()

-- ---------------------------------------------------------------------
--
-- Internal
--
-- ---------------------------------------------------------------------

-- | Data type to represent target source.
data TargetSource
  = SkSource FilePath String [Code] SPState
  -- ^ SK source. Holds file path of the source code, original string
  -- input, parsed form data, and required module names.
  | HsSource FilePath
  -- ^ Haskell source with file path of the source code.
  | OtherSource FilePath
  -- ^ Other source with file path of other contents.
  deriving (Eq)

instance Show TargetSource where
  show s = case s of
    SkSource path mdl _ sp ->
      concat ["SkSource ", show path, " ", mdl, " "
             , show (requiredModuleNames sp) ]
    HsSource path -> "HsSource " ++ show path
    OtherSource path -> "OtherSource " ++ show path

-- | Unit for compilation target.
--
-- Simply a 'TargetSource' maybe paired with 'Phase'.
type TargetUnit = (TargetSource, Maybe Phase)

targetSourcePath :: TargetSource -> FilePath
targetSourcePath mt =
  case mt of
    SkSource path _ _ _ -> path
    HsSource path -> path
    OtherSource path -> path

isOtherSource :: TargetSource -> Bool
isOtherSource ts =
  case ts of
    OtherSource{} -> True
    _             -> False

-- | Make temporally ModSummary for target sources without `require' of
-- home package modules.
partitionRequired :: [String] -> [TargetUnit]
                  -> ([TargetUnit], [TargetUnit])
partitionRequired homePkgModules = foldr f ([],[])
  where
    f (target@(source,_)) (ready,pending) =
      case source of
        SkSource _ _ _ sp
          | any (`elem` homePkgModules) (requiredModuleNames sp)
          -> (ready,target:pending)
        _ -> (target:ready, pending)

-- | Compile 'TargetUnit' to 'ModSummary' and 'HsModule', then compile
-- to interface file, and object code.
--
-- Do macro expansion and get the Haskell `import' declarations from
-- parsed source contents. If the macro expanded result does not
-- contain imports from pending modules, compile to '*.hi' and
-- '*.o'. If the HsModule contained imports of pending module, add the
-- module to the pending modules.
--
make' :: [String] -> [TargetUnit] -> [TargetUnit] -> Skc [ModSummary]
make' not_yet_compiled readys0 pendings0 = do
  debugIO (do putStrLn ";;; make'"
              putStrLn (";;;   nycs: " ++ show not_yet_compiled)
              putStrLn (";;;   total: " ++ show total)
              putStrLn (";;;   readys0: " ++ show readys0)
              putStrLn (";;;   pendings0: " ++ show pendings0))
  timeIt "make' [sk]"
         (go [] total total not_yet_compiled readys0 pendings0)
  where
    -- No more modules to compile, return the accumulated ModSummary.
    go :: [ModSummary] -> Int -> Int -> [String] -> [TargetUnit]
       -> [TargetUnit] -> Skc [ModSummary]
    go acc i _  _  _  _ | i <= 0 = return acc
    go acc _ _ []  _  _ = return acc
    go acc _ _  _ [] [] = return acc

    -- Compile ready-to-compile targets to ModSummary and
    -- HsModule. Input could be SK source code, Haskell source code, or
    -- something else. If SK source code or Haskell source code, get
    -- ModSummary to resolve the dependencies.
    go acc i k nycs (target@(tsr,_mbp):summarised) pendings = do

      -- Since skc make is not using 'DriverPipeline.runPipeline',
      -- setting 'DynFlags.dumpPrefix' manually.
      setDumpPrefix (targetSourcePath tsr)

      case tsr of
        SkSource path _mn _form sp -> do
          Just (hmdl, dflags) <- compileToHsModule target
          summary <- mkModSummary (Just path) hmdl
          let summary' = summary {ms_hspp_opts = dflags}
              imports = map import_name (hsmodImports hmdl)
          debugIO (putStrLn (concat [ ";;; target=", show target
                                    , " imports=", show imports]))

          -- Test whether imported modules are in pendings. If found,
          -- skip the compilation and add this module to the list of
          -- pending modules.
          --
          -- N.B. For SK source target, dependency modules passed to
          -- 'makeOne' contains imported modules and required modules.
          --
          let notYetReady =
                any (\m -> m `elem` map (skmn . fst) pendings ||
                           m `elem` map (skmn . fst) summarised)
                     imports
          if notYetReady
             then go acc i k nycs summarised (target:pendings)
             else do
               let act = mapM (getModSummary' . mkModuleName)
                              (requiredModuleNames sp)
                   act' = catMaybes <$> act
               compileIfReady summary' hmdl imports act'

        HsSource path -> do
          Just (hmdl, dflags) <- compileToHsModule target
          summary <- mkModSummary (Just path) hmdl
          let imports = map import_name (hsmodImports hmdl)
              summary' = summary {ms_hspp_opts = dflags}
          debugIO (putStrLn (concat [";;; target=", show target
                                    ," imports=", show imports]))
          compileIfReady summary' hmdl imports (return [])

        OtherSource _ -> do
          _ <- compileToHsModule target
          go acc i k nycs summarised pendings

        where
          compileIfReady summary hmdl imports getReqs = do
            hsc_env <- getSession
            is <- mapM (findImported hsc_env acc summarised) imports
            debugIO (putStrLn (";;; is = " ++ show is))
            let is' = catMaybes is
            if not (null is')
               then do
                 -- Imported modules are not fully compiled yet. Move
                 -- this module to the end of summarised modules and
                 -- recurse.
                 let summarised' = is' ++ summarised ++ [target]
                     i' = i + length is'
                     k' = k + length is'
                 go acc i' k' nycs summarised' pendings
               else do
                 -- Ready to compile this target unit. Compile it, add
                 -- the returned ModSummary to accumulator, and
                 -- continue.
                 reqs <- getReqs
                 -- XXX: Test whether required modules are all ready to
                 -- compile.
                 let graph_upto_this =
                       topSortModuleGraph True (summary:acc) (Just mn)
                     mn = ms_mod_name summary
                     mNameString = moduleNameString mn
                     nycs' = filter (/= mNameString) nycs
                     deps = flattenSCCs graph_upto_this ++ reqs
                 summary' <- makeOne i k summary hmdl deps
                 go (summary':acc) (i-1) k nycs' summarised pendings

    -- Ready to compile pending modules to read time ModSummary.
    -- Partition the modules, make read time ModSummaries, then sort via
    -- topSortModuleGraph, and recurse.
    go acc i k nycs [] pendings
      | all (isOtherSource . fst) pendings =
         -- All targets are other source, no need to worry about module
         -- dependency analysis.
         go acc i k nycs pendings []
      | otherwise = do
         let (readies', pendings') = partitionRequired nycs pendings
         rt_mss <- mkReadTimeModSummaries readies'
         let graph = topSortModuleGraph True rt_mss Nothing
             readies'' = sortTargets (flattenSCCs graph) readies'
         go acc i k nycs readies'' pendings'

    import_name = moduleNameString . unLoc . ideclName . unLoc
    skmn t = case t of
               SkSource _ mn _ _ -> mn
               _ -> "module-name-unknown"
    total = length readys0 + length pendings0

-- | Check whether recompilation is required, and compile the 'HsModule'
-- when the codes or dependency modules were updated.
makeOne :: Int -> Int -> ModSummary
        -> HsModule RdrName -> [ModSummary] -> Skc ModSummary
makeOne i total ms hmdl graph_upto_this = timeIt label go
  where
    label = "MakeOne [" ++ mname ++ "]"
    mname = moduleNameString (ms_mod_name ms)
    go = do
      -- Keep current DynFlag.
      dflags <- getDynFlags

      -- Use cached dynflags from ModSummary before type check. Note
      -- that the cached dynflags is always used, no matter whether the
      -- module is updated or not. This is to support loading already
      -- compiled module objects with language extensions not set in
      -- current dynflags.
      setDynFlags (ms_hspp_opts ms)
      up_to_date <- checkUpToDate ms graph_upto_this
      ms' <- if up_to_date
               then dontMakeOne ms hmdl
               else doMakeOne (total - i + 1) total ms hmdl

      -- Update module graph with new ModSummary, and restore the
      -- original DynFlags.
      modifySession
        (\e -> e { hsc_mod_graph = ms' : hsc_mod_graph e
                 , hsc_dflags = dflags
                 , hsc_IC = (hsc_IC e) {ic_dflags=dflags}})

      return ms'

-- | Compile single module.
doMakeOne :: Int -> Int -> ModSummary
          -> HsModule RdrName -> Skc ModSummary
doMakeOne i total ms hmdl = do
  dflags_orig <- getDynFlags
  let dflags = ms_hspp_opts ms
      p x = showSDoc dflags (ppr x)
      loc = ms_location ms
      tryGetTimeStamp x = liftIO (tryIO (getModificationUTCTime x))
      e2mb x = case x of
                 Right a -> Just a
                 Left _  -> Nothing

  silent <- fmap envSilent getSkEnv
  unless silent
    (liftIO
       (putStrLn
          (concat [ "; [", show i, "/", show total,  "] compiling "
                  , p (ms_mod_name ms)
                  , " (", fromMaybe "unknown input" (ml_hs_file loc)
                  , ", ", ml_obj_file loc, ")"
                  ])))

  tc <- tcHsModule (Just (ms_hspp_file ms)) True hmdl
  ds <- desugarModule tc
  _ds' <- loadModule ds
  hsc_env <- getSession
  _m <- liftIO (addHomeModuleToFinder hsc_env (ms_mod_name ms) loc)

  -- Update the time stamp of generated obj and hi files.
  mb_obj_date <- e2mb <$> tryGetTimeStamp (ml_obj_file loc)
  mb_iface_date <- e2mb <$> tryGetTimeStamp (ml_hi_file loc)
  setDynFlags dflags_orig
  return ms { ms_obj_date = mb_obj_date
            , ms_iface_date = mb_iface_date }

dontMakeOne :: ModSummary -> HsModule RdrName -> Skc ModSummary
dontMakeOne ms hmdl = do
  hsc_env <- getSession
  let mn = ms_mod_name ms
      loc = ms_location ms

  -- Load linkable and interface file, if exist.
  mb_linkable <-
    case ms_obj_date ms of
      Just t -> do
        l <- liftIO (findObjectLinkable (ms_mod ms) (ml_obj_file loc) t)
        return (Just l)
      Nothing -> return Nothing
  (_msg, mb_mbs_hifile) <-
    liftIO (runTcInteractive hsc_env
                             (readIface (ms_mod ms) (ml_hi_file loc)))

  -- When linkable and interface file were found, make HomeModInfo with
  -- simple ModDetails value. Otherwise, do the compilation work and get
  -- a HomeModInfo.
  hmi <-
    case mb_mbs_hifile of
      Just (Maybes.Succeeded hi) -> do
        -- Found interface file. Doing similar work done in
        -- 'HscMain.hscInrementalCompile'. See note [Knot-tying
        -- typecheckIface] in "compiler/iface/TcIface.hs".
        liftIO (fixIO (\hmi' ->
          do let hsc_env' =
                   hsc_env { hsc_HPT = addToHpt (hsc_HPT hsc_env)
                                                (ms_mod_name ms)
                                                hmi'}
             details <- genModDetails hsc_env' hi
             return HomeModInfo { hm_details = details
                                , hm_iface = hi
                                , hm_linkable = mb_linkable }))
      _ -> do
        -- No interface file found, generating from parsed HsModule.
        tcm <- tcHsModule (Just (ms_hspp_file ms)) False hmdl
        let (tcg, _details) = tm_internals_ tcm
        liftIO (compileOne' (Just tcg) (Just batchMsg) hsc_env ms
                            1 1 Nothing mb_linkable
                            SourceUnmodifiedAndStable)

  let hpt' = addToHpt (hsc_HPT hsc_env) mn hmi
  _ <- setSession hsc_env {hsc_HPT = hpt'}
  _ <- liftIO (addHomeModuleToFinder hsc_env mn loc)
  return ms

-- | Same as 'HscMain.genModDetails' but writing here again, because the
-- function is not exported ...
genModDetails :: HscEnv -> ModIface -> IO ModDetails
genModDetails hsc_env old_iface = do
  new_details <- {-# SCC "tcRnIface" #-}
    initIfaceLoad hsc_env (typecheckIface old_iface)
  dumpIfaceStats hsc_env
  return new_details


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

-- | 'True' if the object code and interface file were up to date,
-- otherwise 'False'.
checkUpToDate :: ModSummary -> [ModSummary] -> Skc Bool
checkUpToDate ms dependencies
  | Nothing <- ms_obj_date ms = return False
  | Nothing <- ms_iface_date ms = return False
  | otherwise = do
    let Just obj_date = ms_obj_date ms
        mn = ms_mod_name ms
        sccs_without_self =
          filter (\m -> mn /= ms_mod_name m) dependencies
        dep_ok ms_dep =
          case ms_obj_date ms_dep of
            Just ot -> obj_date >= ot
            Nothing -> False
        up_to_date = obj_date >= ms_hs_date ms &&
                     all dep_ok sccs_without_self
    return up_to_date

-- | Find 'TargetSource' from command line argument.
findTargetSource :: (String, a) -> Skc (TargetSource, a)
findTargetSource (modName, a) = do
  dflags <- getSessionDynFlags
  inputPath <- findFileInImportPaths (importPaths dflags) modName
  let detectSource path
        | isSkFile path =
          do contents <- liftIO (BL.readFile path)
             (forms, sp) <- parseSexprs (Just path) contents
             let modName' = asModuleName modName
             return (SkSource path modName' forms sp, a)
        | isHsFile path = return (HsSource path, a)
        | otherwise = return (OtherSource path, a)
  detectSource inputPath

-- | Find imported module.
findImported :: HscEnv -- ^ Current hsc environment.
             -> [ModSummary] -- ^ List of accumulated 'ModSummary'.
             -> [TargetUnit] -- ^ Pendingmodules.
             -> String -- ^ Module name to find.
             -> Skc (Maybe TargetUnit)
findImported hsc_env acc pendings name
  | isPending = return Nothing
  | otherwise = do
    findResult <-
      liftIO (findImportedModule hsc_env (mkModuleName name) Nothing)
    dflags <- getSessionDynFlags
    let myInstalledUnitId = thisInstalledUnitId dflags
    case findResult of
      -- Haskell module returned by `Finder.findImportedModule' may not
      -- compiled yet. If the source code has Haskell file extension,
      -- checking whether the module is listed in accumulator containing
      -- compiled modules.
      Found loc mdl    -> do
        debugIO
          (do putStrLn (";;; Found " ++ show loc ++ ", " ++
                        moduleNameString (moduleName mdl))
              putStrLn (";;; moduleUnitId=" ++ show (moduleUnitId mdl))
              putStrLn (";;; myInstalledUnitId=" ++
                        showPpr dflags myInstalledUnitId))
        case ml_hs_file loc of
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
              handleSkException
                (const (return Nothing))
                (Just <$> findTargetSource (name, Nothing))
            | otherwise -> return Nothing
          where
            inSameUnit =
              myInstalledUnitId `installedUnitIdEq` moduleUnitId mdl
            notInAcc =
              moduleName mdl `notElem` map ms_mod_name acc
      NoPackage {}     -> failS ("No Package: " ++ name)
      FoundMultiple {} -> failS ("Found multiple modules for " ++ name)
      NotFound {}      -> Just <$> findTargetSource (name, Nothing)
   where
     isPending = name `elem` [n | (SkSource _ n _ _, _) <- pendings]

-- | Variant of 'getModSummary' wrapped with 'Maybe'.
getModSummary' :: ModuleName -> Skc (Maybe ModSummary)
getModSummary' name = ghandle handler (Just <$> getModSummary name)
  where
    handler :: GhcApiError -> Skc (Maybe ModSummary)
    handler _ghcApiError = return Nothing

-- | Make list of 'ModSummary' for read time dependency analysis.
mkReadTimeModSummaries :: [TargetUnit] -> Skc [ModSummary]
mkReadTimeModSummaries =
  timeIt "mkReadTimeModSummaries" .
  fmap catMaybes .
  mapM mkReadTimeModSummary

-- | Make 'ModSummary' for read type dependency analysis.
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
      Just (hsmdl, _) <- compileToHsModule (target, mbphase)
      fmap Just (mkModSummary (Just file) hsmdl)
    SkSource file mn _form sp -> do
      let modName = mkModuleName mn
          imports = map (noLoc . mkModuleName) (requiredModuleNames sp)
      ms <- mkModSummary' (Just file) modName imports Nothing
      return (Just ms)
    OtherSource _ -> return Nothing

compileToHsModule :: TargetUnit
                  -> Skc (Maybe (HsModule RdrName, DynFlags))
compileToHsModule (tsrc, mbphase) =
  case tsrc of
    SkSource _ mn form sp -> Just <$> compileSkModuleForm' sp mn form
    HsSource path         -> Just <$> compileHsFile path mbphase
    OtherSource path      -> compileOtherFile path >> return Nothing

-- | Wrapper for 'compileSkModuleForm', to use fresh set of modules,
-- language extensions, and macros in 'SkEnv'. Returns tuple of compiled
-- module and 'Dynflags' to update 'ModSummary'.
compileSkModuleForm' :: SPState -> String -> [Code]
                     -> Skc (HsModule RdrName, DynFlags)
compileSkModuleForm' sp mn forms = do
  dflags <- getDynFlagsFromSPState sp
  mdl <- withTempSession (\e -> e {hsc_dflags = dflags}) act
  return (mdl, dflags)
  where
    act = do
      -- Reset macros in current context.
      resetEnvMacros

      contextModules <- envContextModules <$> getSkEnv
      let ii = IIDecl . simpleImportDecl . mkModuleNameFS
      setContext (map (ii . fsLit) contextModules)
      timeIt ("SkModule [" ++ mn ++ "]")
             (compileSkModuleForm forms)

resetEnvMacros :: Skc ()
resetEnvMacros =
  modifySkEnv (\ske -> ske {envMacros = envDefaultMacros ske})

compileHsFile :: FilePath -> Maybe Phase
               -> Skc (HsModule RdrName, DynFlags)
compileHsFile source mbphase = do
  hsc_env <- getSession
  (dflags, source') <- liftIO (preprocess hsc_env (source, mbphase))
  contents <- liftIO (readFile source')
  let location = mkRealSrcLoc (fsLit source) 1 1
      sbuf = stringToStringBuffer contents
      parseState = GHCLexer.mkPState dflags sbuf location
  case GHCLexer.unP GHCParser.parseModule parseState of
    GHCLexer.POk _ m     -> return (unLoc m, dflags)
    GHCLexer.PFailed _ _ -> failS ("Parser error with " ++ source)

compileOtherFile :: FilePath -> Skc ()
compileOtherFile path = do
  debugIO (putStrLn ("Compiling other code: " ++ path))
  hsc_env <- getSession
  liftIO (oneShot hsc_env StopLn [(path, Nothing)])

findFileInImportPaths :: [FilePath]
                      -> String
                      -> Skc FilePath
findFileInImportPaths dirs modName = do
  -- Current approach for source code lookup is search for file with
  -- '*.sk' suffix first. If found return it, otherwise search file with
  -- '*.hs' suffix.
  --
  -- This searching strategy can used when compiling cabal package
  -- containing mixed codes with '*.sk' and '*.hs' suffixes.
  --
  let suffix = takeExtension modName
      moduleFileName = moduleNameSlashes (mkModuleName modName)
      moduleFileName'
        | suffix `elem` [".sk", ".hs", ".c"] = modName
        | otherwise = moduleFileName <.> "sk"
      search ds =
        case ds of
          [] -> failS ("Cannot find source for: " ++ modName)
          d:ds' -> do
            -- Extension not yet sure for `aPath'.
            let aPath = d </> moduleFileName'
                hsPath = replaceExtension aPath ".hs"
            exists <- liftIO (doesFileExist aPath)
            if exists
               then return aPath
               else do
                 exists' <- liftIO (doesFileExist hsPath)
                 if exists'
                    then return hsPath
                    else search ds'
      dirs' | "." `elem` dirs = dirs
            | otherwise     = dirs ++ ["."]
  debugIO (putStrLn (";;; moduleName: " ++ show modName))
  found <- search dirs'
  debugIO (putStrLn (";;; File found: " ++ found))
  return found

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

sortTargets :: [ModSummary] -> [TargetUnit] -> [TargetUnit]
sortTargets summaries targets = foldr f [] summaries
  where
    f summary acc =
      let mloc = ms_location summary
          path = ml_hs_file mloc
          byPath p a = targetSourcePath (fst a) == p
      in  case path of
            Nothing -> error ("sortTargets: no target " ++ show mloc)
            Just path' ->
              case find (byPath path') targets of
                Nothing -> error ("sortTargets: no target " ++ path')
                Just target -> target:acc

timeIt :: String -> Skc a -> Skc a
timeIt label = withTiming getDynFlags (text label) (const ())

isSkFile :: FilePath -> Bool
isSkFile path = takeExtension path == ".sk"

isHsFile :: FilePath -> Bool
isHsFile path = takeExtension path `elem` [".hs", ".lhs"]

asModuleName :: String -> String
asModuleName name
   | looksLikeModuleName name = name
   | otherwise = map slash_to_dot (concat names)
   where
     names = dropWhile (not . isUpper . head)
                       (splitPath (dropExtension name))
     slash_to_dot c = if c == pathSeparator then '.' else c

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
