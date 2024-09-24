{-# LANGUAGE CPP #-}

-- Module for testing the finkel plugin.

module PluginTest (pluginTests) where

#include "ghc_modules.h"

-- base
import Control.Monad          (void)
import System.Info            (os)

#if MIN_VERSION_ghc(9,6,0) || !MIN_VERSION_ghc(9,4,0)
import Control.Exception      (SomeException (..))
#endif

-- filepath
import System.Environment     (getExecutablePath)
import System.FilePath        ((</>))

-- ghc
import GHC
import GHC_Driver_Env         (HscEnv (..))

-- hspec
import Test.Hspec

-- finkel-kernel
import Language.Finkel.Fnk    (getLibDirFromGhc)
import Language.Finkel.Make   (clearGlobalSession)
import Language.Finkel.Plugin (plugin, setFinkelPluginWithArgs)

-- Internal
import TestAux


-- ------------------------------------------------------------------------
--
-- Plugin tests
--
-- ------------------------------------------------------------------------

pluginTests :: FnkSpec
pluginTests =
  -- Clearing global session for macro expansion with `clearGlobalSession'. If
  -- not cleared, when this Plugin tests were ran after Make tests, nested
  -- required home modules (the test with p11.hs) will show a compilation error.
  beforeAll_ (removeArtifacts pdir >> clearGlobalSession) $
    describe "run compiler as ghc plugin" $ do
      compile [] ["--verbose=3"] "p01.hs"
      compile [] [] "p02.hs"
      compile ["-optF", "--warn-interp=False"] ["--verbose=3"] "p03.hs"
#if !MIN_VERSION_ghc(9,4,0)
      compile ["-optF", "--ignore"] ["--ignore"] "p04.hs"
#endif
      compile ["-ddump-parsed-ast"] [] "p05.hs"
      compile ["-optF", "--warn-interp=False"] [] "p06.hs"
      compile [] [] "p08.hs"

      compile ["-v", "-optF", "--warn-interp=False"] ["--verbose=0"] "p09.hs"
      compile ["-v", "-optF", "--warn-interp=False"] ["--verbose=3"] "p09.hs"
      compile ["-v", "-optF", "--warn-interp=False"] ["--verbose=3"] "p10.hs"
      compile ["-v", "-optF", "--warn-interp=False"] ["--verbose=3"] "p11.hs"

      -- Failures
      compileWithFailedFlag [] [] "p07.hs"
      compileAndFail [] ["--help"] "p01.hs"
      compileAndFail [] ["--pragma"] "p01.hs"

compile :: [String] -> [String] -> String -> FnkSpec
compile ghc_args plugin_args basename = do
  let act io = do
        success_flag <- io
        succeeded success_flag `shouldBe` True
      pending_in_win = ["p03.hs", "p06.hs", "p09.hs", "p10.hs", "p11.hs"]
  if os == "mingw32" && basename `elem` pending_in_win
    then it ("should compile " ++ basename) $ \_ ->
           pendingWith "Windows not supported yet"
    else compile' "successfully" act ghc_args plugin_args basename

compileWithFailedFlag :: [String] -> [String] -> String -> FnkSpec
compileWithFailedFlag =
  compile' "fail to" (\io -> do
                         success_flag <- io
                         succeeded success_flag `shouldBe` False)

compileAndFail :: [String] -> [String] -> String -> FnkSpec
compileAndFail =
#if MIN_VERSION_ghc(9,6,0) || !MIN_VERSION_ghc(9,4,0)
  compile' "fail to" (\io -> io `shouldThrow` \(SomeException _) -> True)
#else
  compileWithFailedFlag
#endif

-- Compile source code file. The test executable can act as preprocessor, used
-- to compile Finkel source codes.
compile'
  :: String -> (IO SuccessFlag -> IO ()) -> [String] -> [String] -> String
  -> FnkSpec
compile' msg wrap ghc_args plugin_args basename =
  let title =
        "compile " ++ basename ++ ", ghc args: " ++
        show ghc_args ++ ", plugin args: " ++ show plugin_args
  in  describe title $ do
    it ("should " ++ msg ++ " compile " ++ basename) $ \ftr -> do
      libdir <- getLibDirFromGhc
      _me <- getExecutablePath
      let act = quietly $ runGhc (Just libdir) $ do
            hsc_env1 <- getSession

            let dflags0 = hsc_dflags hsc_env1
                fnk_args = ["-F", "-pgmF", _me, "-i" ++ pdir]
                args = map noLoc (ftr_pkg_args ftr ++ fnk_args ++ ghc_args)

#if MIN_VERSION_ghc(9,2,0)
            logger <- getLogger
            (dflags1, _, _) <- parseDynamicFlags logger dflags0 args
#else
            (dflags1, _, _) <- parseDynamicFlags dflags0 args
#endif
            void (setSessionDynFlags dflags1)

            setFinkelPluginWithArgs plugin plugin_args

#if MIN_VERSION_ghc(9,4,0)
            t <- guessTarget (pdir </> basename) Nothing Nothing
#else
            t <- guessTarget (pdir </> basename) Nothing
#endif
            setTargets [t]
            load LoadAllTargets

      wrap act

pdir :: FilePath
pdir = "test" </> "data" </> "plugin"

