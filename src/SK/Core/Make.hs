{-# LANGUAGE BangPatterns, CPP #-}
-- | Make mode for skc.
module SK.Core.Make
  ( make
  ) where

-- base
import Control.Monad (unless)
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
  -- Updating ghcMode, as done in ghc's "Main.hs".
  dflags <- getSessionDynFlags
  _ <- setSessionDynFlags (dflags { ghcMode = CompManager
                                  , outputFile = mb_output })

  -- Decide the kind of sources of the inputs.
  sources <- mapM findTargetSource inputs

  -- XXX: Assuming modules names were passed as arguments, but inputs
  -- could be file paths. Problem with SK source is that module name
  -- could not be snured until parsing the file contents as HsModule,
  -- but module parsing might use compiled code of other files.
  let homePkgModules = map fst inputs
  mod_summaries <- make' homePkgModules [] sources

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

-- | Make temporally ModSummary for target sources without `require' of
-- home package modules.
partitionRequired :: [String] -> [TargetUnit]
                  -> ([TargetUnit], [TargetUnit])
partitionRequired homePkgModules = foldr f ([],[])
  where
    f (source,mbp) (r,p) =
      case source of
        SkSource _ _ _ reqs
          | any (`elem` homePkgModules) reqs -> (r,(source,mbp):p)
        _ -> ((source,mbp):r, p)

-- | Compile 'TargetUnit' to 'ModSummary' and 'HsModule' with resolving
-- dependencies, then compile to interface file and object code.
make' :: [String] -> [TargetUnit] -> [TargetUnit] -> Skc [ModSummary]
make' not_yet_compiled readys0 pendings0 =
  -- XXX: Quite a lot of assumptions made with input argument format
  -- passed from Cabal.
  go [] total not_yet_compiled readys0 pendings0
  where
    -- No more modules to compile, return the accumulated ModSummary.
    go acc 0  _ _  _ = return acc
    go acc _ [] _  _ = return acc
    go acc _ _ [] [] = return acc

    -- Compile ready-to-compile-targets to ModSummary and
    -- HsModule. Input could be SK source code or something else. If SK
    -- source code or Haskell source code, get ModSummary to resolve the
    -- dependencies.
    go acc i nycs (target@(tsr,mbp):summarised) pendings = do
      case tsr of
        SkSource path mn form _reqs -> do
          hmdl <- compileSkModuleForm form
          summary <- mkModSummary (Just path) hmdl
          let imports = map importName (hsmodImports hmdl)
          if any (`elem` map (skmn . fst) pendings) imports
             then go acc i nycs summarised (target:pendings)
             else do
               makeOne (total - i + 1) total summary hmdl
               let nycs' = filter (/= mn) nycs
               go (summary:acc) (i - 1) nycs' summarised pendings
        HsSource _ -> do
          (Just summary, Just hmdl) <- compileInput (tsr,mbp)
          makeOne (total - i + 1) total summary hmdl
          let mName = moduleNameString (moduleName (ms_mod summary))
              nycs' = filter (/= mName) nycs
          go (summary:acc)  (i - 1) nycs' summarised pendings
        OtherSource _ -> do
          go acc i nycs summarised pendings

    -- Ready to compile pending modules to read-time-ModSummary.
    -- Partition the modules, make read time ModSummaries, then sort via
    -- topSortModuleGraph, and recurse.
    go acc i nycs [] pendings = do
      debugIO (putStrLn (";;; nycs: " ++ show nycs))
      let (readies', pendings') = partitionRequired nycs pendings
      r_mss <- mkReadTimeModSummaries readies'
      let graph = topSortModuleGraph True r_mss Nothing
          readies'' = sortTargets (flattenSCCs graph) readies'
      go acc i nycs readies'' pendings'

    -- Auxiliary
    importName = moduleNameString . unLoc . ideclName . unLoc
    skmn t = case t of
               SkSource _ mn _ _ -> mn
               _ -> "module-name-unknown"
    total = length readys0 + length pendings0

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
      Just m -> do
        summary <- mkModSummary (Just (targetSourcePath tsrc)) m
        case tsrc of
          SkSource _ _ _ reqs -> do
            let mkImport req = (Nothing, noLoc (mkModuleName req))
                imps = ms_textual_imps summary ++ map mkImport reqs
                summary' = summary {ms_textual_imps=imps}
            return (Just summary')
          _ -> return (Just summary)
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

-- XXX: See below for details of how GHC avoid recompilation:
--
--   https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/RecompilationAvoidance
--

-- | Compile single module.
makeOne :: GhcMonad m => Int -> Int -> ModSummary -> HsModule RdrName
        -> m ()
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
