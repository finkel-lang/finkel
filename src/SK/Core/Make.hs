{-# LANGUAGE BangPatterns, CPP #-}
-- | Make mode for skc.
module SK.Core.Make
  ( make
  ) where

-- base
import Control.Monad (foldM_, mapAndUnzipM, when)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Graph (flattenSCCs)
import Data.List (find, nub)
import Data.Maybe (catMaybes)

-- ghc
import qualified Parser as GHCParser
import qualified Lexer as GHCLexer

-- ghc-boot
import qualified GHC.LanguageExtensions as LangExt

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


-- ---------------------------------------------------------------------
--
-- Exported main interface
--
-- --------------------------------------------------------------------------

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
  let dup_compileTimeImports = foldr add_reqs [] sources
      add_reqs (targetSource,_) acc =
        case targetSource of
          SkSource _ _ _ reqs -> reqs ++ acc
          _ -> acc
      compileTimeImports = nub dup_compileTimeImports

  when (not (null compileTimeImports))
       (liftIO (putStrLn (";;; required modules: " ++
                         show compileTimeImports)))

  -- If required modules are found in argument, compile them first.
  let lkupReqs (source,mbphase) acc =
        case source of
          SkSource _ mn _ _
            | elem mn compileTimeImports ->
              (source, mbphase) : acc
          _ -> acc
      requiredHomePkgMods = foldr lkupReqs [] sources
  when (not (null requiredHomePkgMods))
       (liftIO (putStrLn (";;; required home package modules: "
                           ++ show requiredHomePkgMods)))
  (req_summaries, req_modules)
   <- mapAndUnzipM compileInput requiredHomePkgMods
  let mms = zip (catMaybes req_summaries) (catMaybes req_modules)
      mmtotal = length mms
      mk i (ms,mm) = makeOne i mmtotal ms mm >> return (i+1)
  foldM_ mk 1 mms

  -- Compile to ModSummary and HsModule. Input could be SK source code
  -- or something else. If SK source code or Haskell source code,
  -- get ModSummary to resolve the dependencies.
  (mb_summaries, mb_modules) <- mapAndUnzipM compileInput sources

  -- Analyze dependency, then update current module graph.
  let mgraph = topSortModuleGraph True (catMaybes mb_summaries) Nothing
      mgraph_flattened = flattenSCCs mgraph
      total = length mgraph
      modules = catMaybes mb_modules
      work i msum = do
        let name = moduleName (ms_mod msum)
        case findHsModuleByModName name modules of
          Just hsmdl -> makeOne i total msum hsmdl >> return (i + 1)
          Nothing -> return i

  modifySession (\env -> env {hsc_mod_graph = mgraph_flattened})
  debugIO (mapM_ (putStrLn . showSDoc dflags . ppr) mgraph)

  -- Compile to '*.hi' and '*.o' files.
  foldM_ work 1 mgraph_flattened

  -- Delegate linking work to driver pipeline's "link".
  when (not no_link) (doLink mgraph_flattened)


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
    HsSource path -> "HsSource " ++ path
    OtherSource path -> "OtherSource " ++ path

targetSourcePath :: TargetSource -> FilePath
targetSourcePath mt =
  case mt of
    SkSource path _ _ _ -> path
    HsSource path -> path
    OtherSource path -> path

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

compileInput :: (TargetSource, Maybe Phase)
              -> Skc (Maybe ModSummary, Maybe (HsModule RdrName))
compileInput (tsrc, mbphase) = do
  mb_module <- compileToHsModule (tsrc, mbphase)
  mb_summary <-
    case mb_module of
      Nothing -> return Nothing
      Just m -> Just <$> mkModSummary (Just (targetSourcePath tsrc)) m
  return (mb_summary, mb_module)

compileToHsModule :: (TargetSource, Maybe Phase)
                  -> Skc (Maybe (HsModule RdrName))
compileToHsModule (tsrc, mbphase) =
  case tsrc of
    SkSource _ _ form _ -> Just <$> compileSkModuleForm form
    HsSource path -> Just <$> compileHsFile path mbphase
    OtherSource path -> compileOtherFile path >> return Nothing

isSkFile :: FilePath -> Bool
isSkFile path = takeExtension path == ".sk"

isHsFile :: FilePath -> Bool
isHsFile path = elem suffix [".hs", ".lhs"]
   where suffix = takeExtension path

compileHsFile :: FilePath -> Maybe Phase -> Skc (HsModule RdrName)
compileHsFile source mbphase = do
  (source', dflags) <- maybePreprocess source mbphase
  contents <- liftIO (readFile source')
  let location = mkRealSrcLoc (fsLit source) 1 1
      sbuf = stringToStringBuffer contents
      parseState = GHCLexer.mkPState dflags sbuf location
  case GHCLexer.unP GHCParser.parseModule parseState of
    GHCLexer.POk _ m     -> return (unLoc m)
    GHCLexer.PFailed _ _ -> failS ("Parser error with " ++ source)

maybePreprocess :: FilePath -> Maybe Phase -> Skc (FilePath, DynFlags)
maybePreprocess source mbphase = do
  hsc_env <- getSession
  let dflags0 = hsc_dflags hsc_env
  src_opts <- liftIO (getOptionsFromFile dflags0 source)
  (dflags1,_,_) <- liftIO (parseDynamicFilePragma dflags0 src_opts)
  source' <-
    if xopt LangExt.Cpp dflags1
       then liftIO (compileFile hsc_env
                                (HsPp HsSrcFile)
                                (source, mbphase))
       else return source
  return (source', dflags1)

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
        | elem suffix [".sk", ".hs", ".c"] = modName
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
               , " (", maybe "unknown input" id (ml_hs_file loc)
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
