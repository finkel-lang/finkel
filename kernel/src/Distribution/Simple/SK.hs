-- | Module exporting utilities to work with cabal's @Setup.hs@ script.
{-# LANGUAGE CPP #-}
module Distribution.Simple.SK
  (
  -- * UserHooks
    skkcHooks
  , skkcDebugHooks
  , sk2hsHooks
  , stackSk2hsHooks

  -- * Auxiliary building block functions
  , skcHooksWith
  , sk2hsProgram
  , stackSk2hsProgram
  , skcBuildHooksWith

   -- * Reexport from Cabal
  , UserHooks
  , defaultMainWithHooks
  ) where

-- base
import Control.Exception (bracket_)
import Control.Monad (mapAndUnzipM, when)
import Data.Foldable (toList)
import Data.Function (on)
import Data.List (unionBy)
import Data.Monoid (Monoid(..))

-- filepath
import System.FilePath ((<.>), (</>))

-- Cabal
import Distribution.ModuleName (toFilePath)
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.Build (build)
import Distribution.Simple.BuildPaths (autogenComponentModulesDir)
import Distribution.Simple.Configure (configure, findDistPrefOrDefault)
import Distribution.Simple.Haddock (haddock)
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Program.Types
import Distribution.Simple.PreProcess
import Distribution.Simple.Register (internalPackageDBPath)
import Distribution.Simple.Setup
import Distribution.Simple.Program.GHC
import Distribution.Utils.NubList

#if MIN_VERSION_Cabal(2,4,0)
import Distribution.Types.ExposedModule
#else
import Distribution.InstalledPackageInfo
#endif

-- directory
import System.Directory (doesFileExist, removeFile)

import qualified Distribution.Simple.Setup as Setup
import qualified Distribution.Verbosity as Verbosity


-- ---------------------------------------------------------------------
--
-- UserHooks
--
-----------------------------------------------------------------------

-- | A UserHooks to compile SK codes with "skkc" executable found on
-- system.
skkcHooks :: UserHooks
skkcHooks = skcHooksWith "skkc" [] False

-- | UserHooks almost same as'skcHooks', but with SK debug flag turned
-- on.
skkcDebugHooks :: UserHooks
skkcDebugHooks = skcHooksWith "skkc" [] True

-- | Make user hooks from compiler executable and extra arguments to the
-- executable.
skcHooksWith :: FilePath -- ^ Compiler executable.
             -> [String] -- ^ Extra arguments to the executable.
             -> Bool     -- ^ Debug flag.
             -> UserHooks
skcHooksWith exec args debug = simpleUserHooks
  { hookedPreProcessors = [registerSkPPHandler]
  , confHook            = skcConfHookWith exec args debug
  , haddockHook         = skcHaddockHooks
  }

-- | Hooks to preprocess @"*.sk"@ files with "skkc" found on system.
sk2hsHooks :: UserHooks
sk2hsHooks = simpleUserHooks {
    hookedPreProcessors = [("sk", mkSk2hsPP sk2hsProgram)]
  }

-- | Hooks to preprocess @"*.sk"@ files with "skkc" via "stack".
stackSk2hsHooks :: UserHooks
stackSk2hsHooks = simpleUserHooks {
    hookedPreProcessors = [("sk", mkSk2hsPP stackSk2hsProgram)]
  }


-- ---------------------------------------------------------------------
--
-- Auxiliary
--
-- ---------------------------------------------------------------------

-- | Preprocessor suffix handler to merely register files with @"*.sk"@
-- files.
registerSkPPHandler :: PPSuffixHandler
registerSkPPHandler = ("sk", doNothingPP)
  where
    doNothingPP _ _ _ = PreProcessor
      { platformIndependent = True
      , runPreProcessor = mkSimplePreProcessor (\_ _ _ -> return ())
      }

skcConfHookWith :: FilePath -- ^ Path to sk compiler.
                -> [String] -- ^ Extra default args to sk compiler.
                -> Bool     -- ^ Flag for debug.
                -> (GenericPackageDescription, HookedBuildInfo)
                -> ConfigFlags
                -> IO LocalBuildInfo
skcConfHookWith skc extra_args debug (pkg_descr, hbi) cflags = do
  lbi <- configure (pkg_descr, hbi) cflags
  return (overrideGhcAsSkc skc extra_args debug lbi)

-- | Build hooks to replace the executable path of "ghc" with "skc"
-- found on system.
skcBuildHooksWith :: FilePath -- ^ Path to sk compiler.
                  -> Bool     -- ^ Debug flag.
                  -> PackageDescription
                  -> LocalBuildInfo
                  -> UserHooks
                  -> BuildFlags
                  -> IO ()
skcBuildHooksWith skc debug pkg_descr lbi hooks flags =
  build pkg_descr lbi' flags (allSuffixHandlers hooks)
    where
      lbi' = overrideGhcAsSkc skc [] debug lbi

-- | Update @ghc@ program in 'LocalBuildInfo'.
overrideGhcAsSkc :: FilePath -- ^ Path to sk compiler.
                 -> [String] -- ^ Extra default args.
                 -> Bool     -- ^ Debug flag.
                 -> LocalBuildInfo
                 -> LocalBuildInfo
overrideGhcAsSkc skc extra_args debug lbi = lbi'
  where
    lbi' = lbi {withPrograms = updateProgram ghc (withPrograms lbi)}
    ghc =
      case lookupProgram (simpleProgram "ghc") (withPrograms lbi) of
        Just ghc_orig ->
          ghc_orig {
              programLocation = FoundOnSystem skc,
              programDefaultArgs =
                extra_args ++ programDefaultArgs ghc_orig,
              programOverrideArgs =
                programOverrideArgs ghc_orig ++ skflags
          }
        Nothing ->
          (simpleConfiguredProgram "ghc" (FoundOnSystem skc)) {
            programDefaultArgs = extra_args,
            programOverrideArgs = skflags
          }
    skflags = debugs
    debugs = ["--sk-debug"|debug]

-- | Haddock hooks for sk. Generates and cleans up haskell source codes
-- from sk files during documentation generation.
skcHaddockHooks :: PackageDescription
                -> LocalBuildInfo
                -> UserHooks
                -> HaddockFlags
                -> IO ()
skcHaddockHooks pd lbi hooks flags = do
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

      let opts = mempty
            { ghcOptMode             = flag GhcModeMake
            , ghcOptExtra            = optExtras autogen_dir
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
          hs_src_dirs' = hs_src_dirs ++ [autogen_dir]
          flag = Setup.Flag
          cmpl = compiler lbi
          platform = hostPlatform lbi
          gen_files = map (\m -> autogen_dir </> m)
                          (map (\m -> toFilePath m <.> "hs") hs_mods
                           ++ hs_files)
          ghc = simpleProgram "ghc"
          acquire =
            case lookupProgram ghc (withPrograms lbi) of
              Just prog -> runGHC verbosity prog cmpl platform opts
              Nothing   -> return ()
          clean path = do
            exist <- doesFileExist path
            when exist (removeFile path)
          cleanup = mapM_ clean gen_files

      return (acquire, cleanup)

-- | Optional arguments passed to ghc, for writing Haskell source code
-- files from sk source code files.
#if MIN_VERSION_Cabal(2,4,0)
optExtras :: FilePath -> [String]
optExtras = optExtras'
#else
optExtras :: FilePath -> NubListR String
optExtras = toNubListR . optExtras'
#endif
  where
    optExtras' :: FilePath -> [String]
    optExtras' odir = ["-fbyte-code", "--sk-hsdir=" ++ odir]

-- | Same as the one used in "Distribution.Simple".
allSuffixHandlers :: UserHooks -> [PPSuffixHandler]
allSuffixHandlers hooks =
  overridesPP (hookedPreProcessors hooks) knownSuffixHandlers
    where overridesPP = unionBy ((==) `on` fst)

-- | Make simple preprocessor from configured program.
mkSk2hsPP :: ConfiguredProgram -> BuildInfo -> LocalBuildInfo
          -> ComponentLocalBuildInfo -> PreProcessor
mkSk2hsPP program _ _ _ = PreProcessor
  { platformIndependent = True
  , runPreProcessor = mkSimplePreProcessor (mkSk2hs program)
  }
  where
    mkSk2hs prog infile outfile verbosity =
      runProgram verbosity prog ["-o", outfile, infile]

-- Preprocessor arguments
-- ~~~~~~~~~~~~~~~~~~~~~~
--
-- Passing "--sk-no-typecheck" option, since preprocessor cannot get
-- type information from other modules in target package.

sk2hsProgram :: ConfiguredProgram
sk2hsProgram = sk2hs {programDefaultArgs = args}
  where
    sk2hs = simpleConfiguredProgram "skkc" (FoundOnSystem "skkc")
    args = ["--sk-hsrc", "--sk-no-typecheck"]

stackSk2hsProgram :: ConfiguredProgram
stackSk2hsProgram = stack {programDefaultArgs = args}
  where
    stack = simpleConfiguredProgram "stack" (FoundOnSystem "stack")
    args = ["exec", "--", "skkc", "--sk-hsrc", "--sk-no-typecheck"]
