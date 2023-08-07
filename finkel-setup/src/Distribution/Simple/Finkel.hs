-- | Module exporting utilities to work with cabal's @Setup.hs@ script.
{-# LANGUAGE CPP #-}
module Distribution.Simple.Finkel
  (
  -- * Main functions
    fnkMain
  , finkelMakeMain
  , fnkMainWith

  -- * UserHooks
  , fnkHooksWith

  -- * Suffix handler
  , finkelPPHandler

   -- * Reexport from Cabal
  , UserHooks
  , defaultMainWithHooks
  ) where

-- base
import           Control.Exception                  (bracket_)
import           Control.Monad                      (foldM, mapAndUnzipM, when)
import           Data.Foldable                      (toList)
import           Data.Function                      (on)
import           Data.List                          (unionBy)

#if !MIN_VERSION_base(4,13,0)
import           Data.Monoid                        (Monoid (..))
#endif

-- filepath
import           System.FilePath                    ((<.>), (</>))

-- Cabal
import           Distribution.ModuleName            (toFilePath)
import           Distribution.PackageDescription
import           Distribution.Simple
import           Distribution.Simple.BuildPaths     (autogenComponentModulesDir)
import           Distribution.Simple.Configure      (configure,
                                                     findDistPrefOrDefault)
import           Distribution.Simple.Haddock        (haddock)
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.PreProcess
import           Distribution.Simple.Program
import           Distribution.Simple.Program.GHC
import           Distribution.Simple.Program.Types
import           Distribution.Simple.Register       (internalPackageDBPath)
import           Distribution.Simple.Setup
import           Distribution.Simple.Utils          (installDirectoryContents)
import           Distribution.Utils.NubList

#if MIN_VERSION_Cabal(3,5,0)
import           Distribution.Utils.Path            (getSymbolicPath)
#endif

#if MIN_VERSION_Cabal(2,4,0)
import           Distribution.Types.ExposedModule
#else
import           Distribution.InstalledPackageInfo
#endif

import qualified Distribution.Simple.Setup          as Setup
import qualified Distribution.Verbosity             as Verbosity

-- directory
import           System.Directory                   (createDirectoryIfMissing,
                                                     doesDirectoryExist,
                                                     doesFileExist, findFile,
                                                     getTemporaryDirectory,
                                                     removeDirectoryRecursive,
                                                     removeFile)


-- --------------------------------------------------------------------
--
-- Main functions
--
-- ------------------------------------------------------------------------

-- | Main function using /fkc/ executable.
--
-- This acton uses the /fkc/ executable found on system when building
-- a package.
fnkMain :: IO ()
fnkMain = rawFnkMain "fkc" [] False

-- | Main function using /finkel/ executable with /make/ subcommand.
--
-- This action uses the /finkel/ executable found on system when
-- building a package.
finkelMakeMain :: IO ()
finkelMakeMain = rawFnkMain "finkel" ["make"] False

-- | Main function with given executable name and arguments passed to
-- the executable.
fnkMainWith :: String   -- ^ Executable name.
            -> [String] -- ^ Args passed to the executable.
            -> IO ()
fnkMainWith exe args = rawFnkMain exe args False

-- | Run main using 'fnkHooksWith' and given executable.
rawFnkMain :: String   -- ^ Executable
           -> [String] -- ^ Argument passed to the executable.
           -> Bool     -- ^ Debug flag
           -> IO ()
rawFnkMain exec args debug =
  defaultMainWithHooks (fnkHooksWith exec args debug)


-- ---------------------------------------------------------------------
--
-- UserHooks
--
-----------------------------------------------------------------------

-- | Make user hooks from compiler executable and extra arguments to the
-- executable.
fnkHooksWith :: FilePath -- ^ Compiler executable.
             -> [String] -- ^ Extra arguments to the executable.
             -> Bool     -- ^ Debug flag.
             -> UserHooks
fnkHooksWith exec args debug = simpleUserHooks
  { hookedPreProcessors = finkelPPHandler : knownSuffixHandlers
  , confHook            = fnkConfHookWith exec args debug
  , haddockHook         = fnkHaddockHooks
  }


-- ---------------------------------------------------------------------
--
-- Auxiliary
--
-- ---------------------------------------------------------------------

-- | Preprocessor suffix handler to merely register files with @"*.fnk"@
-- files.
finkelPPHandler :: PPSuffixHandler
finkelPPHandler = ("fnk", doNothingPP)
  where
    doNothingPP _ _ _ = PreProcessor
      { platformIndependent = True
#if MIN_VERSION_Cabal(3,8,0)
      , ppOrdering = unsorted
#endif
      , runPreProcessor = mkSimplePreProcessor (\_ _ _ -> return ())
      }

fnkConfHookWith :: FilePath -- ^ Path to Finkel compiler.
                -> [String] -- ^ Extra default args to the Finkel
                            -- compiler.
                -> Bool     -- ^ Flag for debug.
                -> (GenericPackageDescription, HookedBuildInfo)
                -> ConfigFlags
                -> IO LocalBuildInfo
fnkConfHookWith fnk extra_args debug (pkg_descr, hbi) cflags = do
  lbi <- configure (pkg_descr, hbi) cflags
  return (overrideGhcAsFnk fnk extra_args debug lbi)

-- | Update @ghc@ program in 'LocalBuildInfo'.
overrideGhcAsFnk :: FilePath -- ^ Path to Finkel compiler.
                 -> [String] -- ^ Extra default args.
                 -> Bool     -- ^ Debug flag.
                 -> LocalBuildInfo
                 -> LocalBuildInfo
overrideGhcAsFnk fnk extra_args debug lbi = lbi'
  where
    lbi' = lbi {withPrograms = updateProgram ghc (withPrograms lbi)}
    ghc =
      case lookupProgram (simpleProgram "ghc") (withPrograms lbi) of
        Just ghc_orig ->
          ghc_orig {
              programLocation = FoundOnSystem fnk,
              programDefaultArgs =
                extra_args ++ programDefaultArgs ghc_orig,
              programOverrideArgs =
                programOverrideArgs ghc_orig ++ finkelflags
          }
        Nothing ->
          (simpleConfiguredProgram "ghc" (FoundOnSystem fnk)) {
            programDefaultArgs = extra_args,
            programOverrideArgs = finkelflags
          }
    finkelflags = debugs
    debugs = ["--fnk-debug"|debug]

-- | Haddock hooks for Finkel. Generates and cleans up Haskell source
-- codes from Finkel files during documentation generation.
fnkHaddockHooks :: PackageDescription
                -> LocalBuildInfo
                -> UserHooks
                -> HaddockFlags
                -> IO ()
fnkHaddockHooks pd lbi hooks flags = do
  (acquires, cleanups) <- mapAndUnzipM gen_hs_sources clbis
  bracket_ (sequence_ acquires)
           (sequence_ cleanups)
           (haddock pd lbi pps flags)
  where
    pps = allSuffixHandlers hooks
    clbis = toList (componentGraph lbi)
    gen_hs_sources clbi = do
      let name = componentLocalName clbi
          comp = getComponent pd name
          bi = componentBuildInfo comp
          cflags = configFlags lbi
          verbosity = case configVerbosity cflags of
                        Setup.Flag v -> v
                        NoFlag       -> Verbosity.normal
          autogen_dir = autogenComponentModulesDir lbi clbi
          pkg_dbs = withPackageDB lbi
          pkgs = componentIncludes clbi
          hs_src_dirs = hsSourceDirs bi
          other_mods = otherModules bi

      distPref <- findDistPrefOrDefault (configDistPref cflags)

      let internal_pkg_db =
            SpecificPackageDB (internalPackageDBPath lbi distPref)

      (hs_mods, hs_insts, hs_files) <-
         case comp of
           CLib {} -> do
             let ms = componentExposedModules clbi
                 is = componentInstantiatedWith clbi
                 ms' = foldr f [] ms
                   where
                     f em acc =
                       case exposedReexport em of
                         Nothing -> exposedName em : acc
                         Just _  -> acc
             return (ms' ++ other_mods, is, [])
           CExe exe -> do
             let path = modulePath exe
             return (other_mods, [], [path])
           _ -> return (other_mods, [], [])

      let opts dir = mempty
            { ghcOptMode             = flag GhcModeMake
            , ghcOptExtra            = optExtras dir
            , ghcOptInputFiles       = toNubListR hs_files
            , ghcOptInputModules     = toNubListR hs_mods
            , ghcOptSourcePathClear  = flag True
            , ghcOptSourcePath       = toNubListR hs_src_dirs'
            , ghcOptInstantiatedWith = hs_insts
            , ghcOptPackageDBs       = pkg_dbs ++ [internal_pkg_db]
            , ghcOptPackages         = toNubListR pkgs
            , ghcOptHideAllPackages  = flag True
            , ghcOptNoLink           = flag True
            }
          hs_src_dirs' = map getSymbolicPath hs_src_dirs ++ [autogen_dir]
          flag = Setup.Flag
          cmpl = compiler lbi
          platform = hostPlatform lbi
          accumulateGeneratedFile acc m = do
            let p = toFilePath m
                hs_src_dir_paths = map getSymbolicPath hs_src_dirs
            mb_found <- findFile hs_src_dir_paths (p <.> "fnk")
            case mb_found of
              Just _found -> do
                let dest = autogen_dir </> p <.> "hs"
                return (dest:acc)
              Nothing    -> return acc

          -- Using package name as prefix of temporary directory, to support
          -- concurrent build of packages.
          makeTemporaryDirectory = do
            tmpdir <- getTemporaryDirectory
            let dir = tmpdir </> pre </> cmp_name
                cmp_name =
                  remove_quotes $ replace_spaces (showComponentName name)
                replace_spaces = map space_to_underscore
                space_to_underscore c =
                  case c of
                    ' ' -> '_'
                    _   -> c
                remove_quotes = filter (/= '\'')
                pre = "fnk_haddock_hooks" </> pkg_name
                pkg_name = unPackageName (pkgName (package pd))
            createDirectoryIfMissing True dir
            return dir

      gen_files <- foldM accumulateGeneratedFile [] hs_mods
      tmpdir <- makeTemporaryDirectory

      let ghc = simpleProgram "ghc"
          acquire =
            case lookupProgram ghc (withPrograms lbi) of
              Just prog | not (null gen_files) -> do
                runGHC verbosity prog cmpl platform (opts tmpdir)
                installDirectoryContents verbosity tmpdir autogen_dir
              _                                -> return ()
          clean path = do
            exist <- doesFileExist path
            when exist (do when (Verbosity.normal < verbosity)
                                (putStrLn ("Removing: " ++ path))
                           removeFile path)
            exist_dir <- doesDirectoryExist tmpdir
            when exist_dir $
              removeDirectoryRecursive tmpdir
          cleanup = mapM_ clean gen_files

      return (acquire, cleanup)

-- | Optional arguments passed to ghc, for writing Haskell source code
-- files from Finkel source code files.
#if MIN_VERSION_Cabal(2,4,0)
optExtras :: FilePath -> [String]
optExtras = optExtras'
#else
optExtras :: FilePath -> NubListR String
optExtras = toNubListR . optExtras'
#endif
  where
    optExtras' :: FilePath -> [String]
#if MIN_TOOL_VERSION_ghc(9,2,0)
    -- In ghc >= 9.2, "-fbyte-code" creates '*.o' object files. Using
    -- "-fno-code" instead of bytecode.
    optExtras' odir = ["-v0", "-fno-code", "--fnk-hsdir=" ++ odir]
#else
    optExtras' odir = ["-v0", "-fbyte-code", "--fnk-hsdir=" ++ odir]
#endif

-- | Same as the one used in "Distribution.Simple".
allSuffixHandlers :: UserHooks -> [PPSuffixHandler]
allSuffixHandlers hooks =
  overridesPP (hookedPreProcessors hooks) knownSuffixHandlers
    where overridesPP = unionBy ((==) `on` fst)

#if !MIN_VERSION_Cabal(3,5,0)
getSymbolicPath :: a -> a
getSymbolicPath = id
#endif
