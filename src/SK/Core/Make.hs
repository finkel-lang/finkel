{-# LANGUAGE BangPatterns, CPP #-}
-- | Make mode for skc.
module SK.Core.Make
  ( make
  ) where

-- base
import Control.Monad (foldM_, mapAndUnzipM, unless)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Graph (flattenSCCs)
import Data.List (find)
import Data.Maybe (catMaybes, fromMaybe)

-- ghc
import qualified Parser as GHCParser
import qualified Lexer as GHCLexer

-- directory
import System.Directory (doesFileExist)

-- filepath
import System.FilePath ( dropExtension
                       , replaceExtension
                       , takeExtension
                       , (<.>), (</>))

-- internal
import SK.Core.Form
import SK.Core.GHC
import SK.Core.Lexer
import SK.Core.Run
import SK.Core.SKC

-- From ghc, currently unused.
--
-- import HscTypes (FindResult(..))
-- import Finder (findHomeModule)

import Module (moduleNameString)

-- ---------------------------------------------------------------------
--
-- Exported main interface
--
-- --------------------------------------------------------------------------

-- [Requiring home package module]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- The problem in dependency resolution when requiring home package
-- module is, we need module imports list to make ModSummary, but
-- modules imports could not be obtained unless the source code is macro
-- expanded. However, macroexpansion may use macros from required
-- modules.
--
-- Once the dependency resolution work were tried with custom user hooks
-- in cabal setup script. However, as of Cabal version 1.24.2, building
-- part of some modules from contents of cabal configuration file were
-- not so easy. Though when cabal support multiple libraraies, situation
-- might change.  Partition target sources to modules which containing
-- `require' syntax of home package modules, and which doesn't.

-- | SK variant of @ghc --make@.
make :: [(FilePath, Maybe Phase)] -- ^ List of input file and phase
     -> Bool -- ^ Skip linking when 'True'.
     -> Maybe FilePath -- ^ Output file, if any.
     -> Skc ()
make inputs no_link mb_output = do

  -- Setting ghcMode as done in ghc's "Main.hs".
  dflags <- getSessionDynFlags
  _ <- setSessionDynFlags (dflags { ghcMode = CompManager
                                  , outputFile = mb_output })

  -- Decide the kind of sources of the inputs.
  sources <- mapM findTargetSource inputs

  let partition (source,mbp) (r,p) =
        case source of
          SkSource _ _ _ reqs
            | any (`elem` homePkgModules) reqs -> (r,(source,mbp):p)
          _ -> ((source,mbp):r, p)
      homePkgModules = map fst inputs
      (ready, pendings) = foldr partition ([],[]) sources

  debugIO (do putStrLn (";;; ready: " ++ show ready)
              putStrLn (";;; pending: " ++ show pendings))

  -- Making temporally ModSummary for target sources without `require'
  -- of home package modules.
  req_mss <- mkReadTimeModSummaries ready

  let graph1 = topSortModuleGraph True req_mss Nothing
      targets1 = sortTargets (flattenSCCs graph1) ready

  debugIO (do putStrLn ";;; targets1: "
              mapM_ print targets1)
  -- mod_summaries <- go [] targets1 pendings
  mod_summaries <- make' targets1 pendings

  -- (mb_summaries, mb_modules) <- mapAndUnzipM compileInput sources

  -- let mgraph = topSortModuleGraph True (catMaybes mb_summaries) Nothing
  --     mgraph_flattened = flattenSCCs mgraph
  --     total = length mgraph
  --     modules = catMaybes mb_modules
  --     work i msum = do
  --       let name = moduleName (ms_mod msum)
  --       case findHsModuleByModName name modules of
  --         Just hsmdl -> makeOne i total msum hsmdl >> return (i + 1)
  --         Nothing -> return i

  -- Update current module graph for linker. Linking work is delegated
  -- to deriver pipelin's `link' function.
  let mgraph_flattened =
        flattenSCCs (topSortModuleGraph True mod_summaries Nothing)
  modifySession (\env -> env {hsc_mod_graph = mgraph_flattened})
  unless no_link (doLink mgraph_flattened)


-- ---------------------------------------------------------------------
--
-- Internal
--
-- ---------------------------------------------------------------------

-- | Data type to differentiate target sources.
data TargetSource
  -- XXX: Original input string in SkSource is assumed as module name at
  -- the moment, but it could be a file path.
  = SkSource FilePath String [LCode] [String]
  -- ^ SK source. Holds file path of the source code, original string
  -- input, parsed form data, and required module names.
  | HsSource FilePath
  -- ^ Haskell source with file path of the source code.
  | OtherSource FilePath
  -- ^ Other source with file path of other contents.
  deriving (Eq)

instance Show TargetSource where
  show s = case s of
    SkSource path mdl _ reqs ->
      concat ["SkSource ", show path, " ", mdl, " ", show reqs]
    HsSource path -> "HsSource " ++ show path
    OtherSource path -> "OtherSource " ++ show path

type TargetUnit = (TargetSource, Maybe Phase)

targetSourcePath :: TargetSource -> FilePath
targetSourcePath mt =
  case mt of
    SkSource path _ _ _ -> path
    HsSource path -> path
    OtherSource path -> path

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
make' readys pendings = go 1 [] readys pendings
  where
    -- Compile to ModSummary and HsModule. Input could be SK source
    -- code or something else. If SK source code or Haskell source
    -- code, get ModSummary to resolve the dependencies.
    go i acc (target@(tsr,mbp):summarised) pending = do
      case tsr of
        SkSource path _mn form _reqs -> do
          hmdl <- compileSkModuleForm form
          summary <- mkModSummary (Just path) hmdl
          let imports = map importName (hsmodImports hmdl)
          if any (`elem` map (skmn . fst) pending) imports
             then go i acc summarised (target:pending)
             else do
               makeOne i total summary hmdl
               go (i + 1) (summary:acc) summarised pending
        HsSource _ -> do
          (Just summary, Just hmdl) <- compileInput (tsr,mbp)
          makeOne i total summary hmdl
          go (i + 1) (summary:acc) summarised pending
        OtherSource _ -> do
          go i acc summarised pending

    -- Ready to compile pending modules. Get ModSummary and HsModule,
    -- resolve dependency with `topSortModuleGraph', then compile to
    -- object code.
    go i acc [] pending = do
      (mb_p_mss, mb_hmdls) <- mapAndUnzipM compileInput pending
      let p_mss = catMaybes mb_p_mss
          hmdls = catMaybes mb_hmdls
          graph2 = topSortModuleGraph True p_mss Nothing
          work j msum = do
            let name = moduleName (ms_mod msum)
            case findHsModuleByModName name hmdls of
              Just hmdl -> makeOne j total msum hmdl >> return (j+1)
              Nothing   -> return j
      foldM_ work i (flattenSCCs graph2)
      return (p_mss ++ acc)

    importName ld = moduleNameString (unLoc (ideclName (unLoc ld)))
    skmn t = case t of
               SkSource _ mn _ _ -> mn
               _ -> "module-name-unknown"
    total = length readys + length pendings

findTargetSource :: (String, a) -> Skc (TargetSource, a)
findTargetSource (modName, a) = do
  dflags <- getSessionDynFlags
  inputPath <- findFileInImportPaths (importPaths dflags) modName
  let detectSource path
        | isSkFile path =
          do contents <- liftIO (readFile path)
             (forms, sp) <- parseSexprs (Just path) contents
             let reqs = requiredModuleNames sp
             return (SkSource path modName forms reqs, a)
        | isHsFile path = return (HsSource path, a)
        | otherwise = return (OtherSource path, a)
  detectSource inputPath

-- | Make list of 'ModSummary' for read time dependency analysis.
mkReadTimeModSummaries :: [TargetUnit] -> Skc [ModSummary]
mkReadTimeModSummaries = fmap catMaybes . mapM mkMS

-- | Make 'ModSummary' for read type dependency analysis.
--
-- The 'ms_textual_imps' field of 'ModSummary' made with this function
-- contains modules reffered by `require' keyword, not the modules
-- referred by Haskell's `import'. Purpose of this function is to
-- resolve dependency of home package modules for macro expansion.
mkMS :: TargetUnit -> Skc (Maybe ModSummary)
mkMS (target, mbphase) =
  -- GHC.getModSummary is not ready at this point, since the module
  -- dependency graph is not yet created. Making possibly temporary
  -- ModSummary from target source.
  case target of
    HsSource file -> do
      Just hsmdl <- compileToHsModule (target, mbphase)
      fmap Just (mkModSummary (Just file) hsmdl)
    SkSource file mn _form reqs -> do
      let modName = mkModuleName mn
          imports = map (noLoc . mkModuleName) reqs
      fmap Just (mkModSummary' (Just file) modName imports)
    OtherSource _ -> return Nothing

compileInput :: TargetUnit
             -> Skc (Maybe ModSummary, Maybe (HsModule RdrName))
compileInput (tsrc, mbphase) = do
  mb_module <- compileToHsModule (tsrc, mbphase)
  mb_summary <-
    case mb_module of
      Nothing -> return Nothing
      Just m -> Just <$> mkModSummary (Just (targetSourcePath tsrc)) m
  return (mb_summary, mb_module)

compileToHsModule :: TargetUnit
                  -> Skc (Maybe (HsModule RdrName))
compileToHsModule (tsrc, mbphase) =
  case tsrc of
    SkSource _ _ form _ -> Just <$> compileSkModuleForm form
    HsSource path -> do
      (mdl, _) <- compileHsFile path mbphase
      return (Just mdl)
    OtherSource path -> compileOtherFile path >> return Nothing

isSkFile :: FilePath -> Bool
isSkFile path = takeExtension path == ".sk"

isHsFile :: FilePath -> Bool
isHsFile path = takeExtension path `elem` [".hs", ".lhs"]

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
  liftIO (oneShot hsc_env (startPhase path) [(path, Just StopLn)])

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
      dirs' | elem "." dirs = dirs
            | otherwise     = dirs ++ ["."]
  debugIO (putStrLn ("moduleName: " ++ show modName))
  found <- search dirs'
  debugIO (putStrLn ("File found: " ++ found))
  return found

-- Failed attempt to avoid recompilation. Haskell source code cache
-- management not working properly. See below for details of how GHC
-- avoid recompilation:
--
--   https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/RecompilationAvoidance
--
-- makeOne :: GhcMonad m => Int -> Int -> ModSummary
--         -> HsModule RdrName -> m ()
-- makeOne i total ms hmdl = do
--   hsc_env <- getSession
--   findResult <- liftIO (findHomeModule hsc_env (ms_mod_name ms))
--   case findResult of
--     Found _ _ -> return ()
--     -- The `makeOne' function defined below.
--     _ -> makeOne' i total ms hmdl

-- | Compile single module.
makeOne :: GhcMonad m => Int -> Int -> ModSummary
        -> HsModule RdrName -> m ()
makeOne i total ms hmdl = do
  dflags <- getSessionDynFlags
  let p x = showSDoc dflags (ppr x)
      loc = ms_location ms
  liftIO
    (putStrLn
       (concat [ ";;; [", show i, "/", show total,  "] compiling "
               , p (ms_mod_name ms)
               , " (", fromMaybe "unknown input" (ml_hs_file loc)
               , ", ", ml_obj_file loc, ")"
               ]))
  tc <- tcHsModule (Just (ms_hspp_file ms)) True hmdl
  ds <- desugarModule tc
  _ds' <- loadModule ds
  hsc_env <- getSession
  _m <- liftIO (addHomeModuleToFinder hsc_env
                                      (ms_mod_name ms)
                                      (ms_location ms))
  return ()

findHsModuleByModName :: ModuleName -> [HsModule a]
                      -> Maybe (HsModule a)
findHsModuleByModName name = find f
  where
    -- Using module name "Main" for look up key when module name was not
    -- specified in given HsModule.
    f mdl = name == maybe mainModName unLoc (hsmodName mdl)
    mainModName = mkModuleName "Main"

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

-- [guessOutputFile]
--
-- Following 'guessOutputFile' is mostly copied from "GhcMake.hs", it
-- was not exported. Modified to take [ModSummary] argument instead of
-- getting the module graph from session.

-- | If there is no -o option, guess the name of target executable
-- by using top-level source file name as a base.
guessOutputFile :: GhcMonad m => [ModSummary] -> m ()
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
