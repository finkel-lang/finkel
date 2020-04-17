-- | Custom setup script to pass command line options to happy and alex.
--
-- Passing "--ghc" option to alex and happy manually. When stack add
-- supports for passing command line options to arbitrary program used
-- during build, this script could be removed.
--
module Main where

-- base
import Data.Char                             (isSpace)
import Data.Function                         (on)
import Data.List                             (unionBy)

-- Cabal
import Distribution.Simple                   (UserHooks (..),
                                              defaultMainWithHooks,
                                              simpleUserHooks)
import Distribution.Simple.Build             (build)
import Distribution.Simple.BuildPaths        (autogenComponentModulesDir)
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PreProcess        (knownSuffixHandlers)
import Distribution.Simple.Program           (getDbProgramOutput, ghcProgram)
import Distribution.Simple.Program.Db        (lookupProgram, updateProgram)
import Distribution.Simple.Program.Types     (ConfiguredProgram (..),
                                              ProgramLocation (..), programPath,
                                              simpleConfiguredProgram)
import Distribution.Simple.Setup             (ConfigFlags (..), fromFlag)
import Distribution.Simple.Utils             (createDirectoryIfMissingVerbose)
import Distribution.Types.PackageDescription (PackageDescription)
import Distribution.Verbosity                (Verbosity)

-- filepath
import System.FilePath                       ((</>))

-- | Main function for setup.
--
-- Setup some addicional flags for /alex/ and /happy/, and emit C
-- header files with C macros for all components.
main :: IO ()
main = defaultMainWithHooks myHooks
  where
    myHooks = simpleUserHooks {buildHook = myBuildHooks
                              ,postConf  = myPostConf}

    myBuildHooks pkg_descr lbi hooks flags =
      build pkg_descr lbi' flags (allSuffixHandlers hooks)
        where
          lbi' = lbi {withPrograms =
                        updateProgram happy
                          (updateProgram alex (withPrograms lbi))}
          alex = alex' { programOverrideArgs = ["--ghc"] }
          alex' = simpleConfiguredProgram "alex" (FoundOnSystem "alex")
          happy = happy' { programOverrideArgs = ["-a", "-g", "-c"]
                           -- Happy can take `--strict' flag, which adds
                           -- strictness to happy parser.
                           --
                           -- ["-a", "-c", "-g", "--strict"]
                         }
          happy' =
            simpleConfiguredProgram "happy" (FoundOnSystem "happy")

    allSuffixHandlers hooks =
      overridesPP (hookedPreProcessors hooks) knownSuffixHandlers
        where overridesPP = unionBy ((==) `on` fst)

    myPostConf _args flags pkg_descr lbi =
      writeFinkelKernelConfig pkg_descr lbi flags

writeFinkelKernelConfig :: PackageDescription -> LocalBuildInfo
                        -> ConfigFlags -> IO ()
writeFinkelKernelConfig pkg_descr lbi flags = do
  -- FINKEL_KERNEL_LIBDIR and FINKEL_KERNEL_GHC are obtained with
  -- similar way done in "ghc-paths" package.
  let verbosity = fromFlag (configVerbosity flags)
  libdir0 <- getLibDir verbosity lbi
  let bdpref = configDistPref flags
      bdpref_path = fromFlag bdpref
      def name str = "#define " ++ name ++ ' ':show str
      libdir1 = reverse (dropWhile isSpace (reverse libdir0))
      ghc = case lookupProgram ghcProgram (withPrograms lbi) of
              Just p  -> programPath p
              Nothing -> error "ghc was not found"
      config_h =
        ["/* Auto generated by Setup.hs */"
        ,""
        ,"/* Path for inplace package lookup */"
        ,def "FINKEL_KERNEL_CONFIG_DISTPREF" bdpref_path
        ,""
        ,"/* Path for the GHC library directory */"
        ,def "FINKEL_KERNEL_LIBDIR" libdir1
        ,""
        ,"/* Path for the GHC executable */"
        ,def "FINKEL_KERNEL_GHC" ghc
        ]
      gen comp clbi =
        let autogen_dir = autogenComponentModulesDir lbi clbi
            dest_path = autogen_dir </> "finkel_kernel_config.h"
            work =
              do createDirectoryIfMissingVerbose verbosity True
                                                 autogen_dir
                 writeFile dest_path (unlines config_h)
        in case comp of
             CLib _  -> work
             CTest _ -> work
             _       -> return ()

  withAllComponentsInBuildOrder pkg_descr lbi gen

-- | Function to get GHC library directory path.
getLibDir :: Verbosity -> LocalBuildInfo -> IO FilePath
getLibDir verbosity lbi =
  getDbProgramOutput verbosity ghcProgram (withPrograms lbi)
                     ["--print-libdir"]
