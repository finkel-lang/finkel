-- | Custom setup script to pass command line options to happy and alex.
--
-- Passing "--ghc" option to alex and happy manually. When stack add
-- supports for passing command line options to arbitrary program used
-- during build, this script could be removed.
--
module Main where

-- base
import Data.Function (on)
import Data.List (unionBy)

-- Cabal
import Distribution.Simple ( defaultMainWithHooks
                           , simpleUserHooks
                           , UserHooks(..))
import Distribution.Simple.Build (build)
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PreProcess (knownSuffixHandlers)
import Distribution.Simple.Program.Db (updateProgram)
import Distribution.Simple.Program.Types
  ( ConfiguredProgram(..)
  , ProgramLocation(..)
  , simpleConfiguredProgram )


main :: IO ()
main = defaultMainWithHooks myHooks
  where
    myHooks = simpleUserHooks {buildHook = myBuildHooks}
    myBuildHooks pkg_descr lbi hooks flags =
      build pkg_descr lbi' flags (allSuffixHandlers hooks)
        where
          lbi' = lbi {withPrograms =
                        updateProgram happy
                          (updateProgram alex (withPrograms lbi))}
          alex = alex' { programOverrideArgs = ["--ghc"] }
          alex' = simpleConfiguredProgram "alex" (FoundOnSystem "alex")
          happy = happy' { programOverrideArgs = ["-a", "-c", "-g"]
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
