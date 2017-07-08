-- | Module exporting utilities to work with cabal's @Setup.hs@ script.

module SK.Core.Setup
  (
  -- * Reexport from Cabal
    defaultMainWithHooks

  -- * UserHooks
  , skcHooks
  , sk2hsHooks
  , stackSk2hsHooks
  , registerSkHooks

  -- * Auxiliary building block functions
  , registerSkPPHandler
  , sk2hsProgram
  , stackSk2hsProgram
  , skcBuildHooks
  ) where

-- base
import Data.Function (on)
import Data.List (unionBy)

-- Cabal
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.Build (build)
import Distribution.Simple.Haddock (haddock)
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Program.Types
import Distribution.Simple.PreProcess
import Distribution.Simple.Setup


-- ---------------------------------------------------------------------
--
-- UserHooks
--
-----------------------------------------------------------------------

-- | A UserHooks to compile SK codes with "skc" executable found on
-- system.
skcHooks :: UserHooks
skcHooks = simpleUserHooks
  { hookedPreProcessors = [registerSkPPHandler]
  , buildHook = skcBuildHooks
  , haddockHook = stackSkHaddockHooks
  }

-- | Hooks to register @"*.sk"@ files.
registerSkHooks :: UserHooks
registerSkHooks = simpleUserHooks {
   hookedPreProcessors = [registerSkPPHandler]
 }

-- | Hooks to preprocess @"*.sk"@ files with "skc" found on system.
sk2hsHooks :: UserHooks
sk2hsHooks = simpleUserHooks {
    hookedPreProcessors = [("sk", mkSk2hsPP sk2hsProgram)]
  }

-- | Hooks to preprocess @"*.sk"@ files with "skc" via "stack".
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
    doNothingPP _ _ = PreProcessor
      { platformIndependent = True
      , runPreProcessor = mkSimplePreProcessor (\_ _ _ -> return ())
      }

-- | Build hooks to replace the executable path of "ghc" with "skc"
-- found on system.
skcBuildHooks :: PackageDescription -> LocalBuildInfo
              -> UserHooks -> BuildFlags -> IO ()
skcBuildHooks pkg_descr lbi hooks flags =
  build pkg_descr lbi' flags (allSuffixHandlers hooks)
    where
      lbi' = lbi {withPrograms = updateProgram ghc (withPrograms lbi)}
      ghc = simpleConfiguredProgram "ghc" (FoundOnSystem "skc")

-- | Haddock hooks using @stack exec skc@ for preprocessing ".sk"
-- files.
stackSkHaddockHooks :: PackageDescription -> LocalBuildInfo
                    -> UserHooks -> HaddockFlags -> IO ()
stackSkHaddockHooks p l h = haddock p l pps
  where
    pps = ("sk", mkSk2hsPP stackSk2hsProgram) : allSuffixHandlers h

-- | Same as the one used in "Distribution.Simple".
allSuffixHandlers :: UserHooks -> [PPSuffixHandler]
allSuffixHandlers hooks =
  overridesPP (hookedPreProcessors hooks) knownSuffixHandlers
    where overridesPP = unionBy ((==) `on` fst)

-- | Make simple preprocessor from configured program.
mkSk2hsPP :: ConfiguredProgram -> BuildInfo -> LocalBuildInfo
          -> PreProcessor
mkSk2hsPP program _ _ = PreProcessor
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
    sk2hs = simpleConfiguredProgram "skc" (FoundOnSystem "skc")
    args = ["--sk-hsrc", "--sk-no-typecheck"]

stackSk2hsProgram :: ConfiguredProgram
stackSk2hsProgram = stack {programDefaultArgs = args}
  where
    stack = simpleConfiguredProgram "stack" (FoundOnSystem "stack")
    args = ["exec", "skc", "--", "--sk-hsrc", "--sk-no-typecheck"]
