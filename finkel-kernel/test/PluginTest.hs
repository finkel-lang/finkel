{-# LANGUAGE CPP #-}

-- Module for testing the finkel plugin.

module PluginTest (pluginTests) where

-- Requires StaticPlugin, which is supported from ghc 8.8.0.
#if !MIN_VERSION_ghc(8,8,0)
-- hspec
import Test.Hspec

-- Internal
import TestAux

pluginTests :: FnkSpec
pluginTests = describe "run compiler as ghc plugin" $
  it "is not supported" $ \_ -> pendingWith "GHC version is < 8.8.0"
#else

#include "ghc_modules.h"

-- base
import Control.Exception      (SomeException (..))
import Control.Monad          (void)
import System.Info            (os)

-- filepath
import System.Environment     (getExecutablePath)
import System.FilePath        ((</>))

-- ghc
import GHC
import GHC_Driver_Env         (HscEnv (..))
import GHC_Plugins            (PluginWithArgs (..), StaticPlugin (..))

-- hspec
import Test.Hspec

-- finkel-kernel
import Language.Finkel.Fnk    (getLibDirFromGhc)
import Language.Finkel.Plugin (plugin)

-- Internal
import TestAux


-- ------------------------------------------------------------------------
--
-- Plugin tests
--
-- ------------------------------------------------------------------------

pluginTests :: FnkSpec
pluginTests =
  beforeAll_ (removeArtifacts pdir) $
    describe "run compiler as ghc plugin" $ do
      compile [] ["--verbose=3"] "p01.hs"
      compile [] [] "p02.hs"
      compile [] ["--verbose=3"] "p03.hs"
      compile ["-optF", "--ignore"] ["--ignore"] "p04.hs"
      compile ["-ddump-parsed-ast"] [] "p05.hs"
      compile [] [] "p06.hs"
      compile [] [] "p08.hs"

#if !MIN_VERSION_ghc(9,2,0)
      -- XXX: Test code with macros not yet working in ghc >= 9.2.0. Required
      -- module is compiled, but macros in the required module were not added to
      -- FnkEnv.
      compile ["-v"] ["--verbose=0"] "p09.hs"
      compile ["-v"] ["--verbose=3"] "p09.hs"
      compile [] ["--verbose=3"] "p10.hs"
#endif

      -- Failures
      compileWithFailedFlag [] [] "p07.hs"
      compileAndFail [] ["--help"] "p01.hs"
      compileAndFail [] ["--pragma"] "p01.hs"

compile :: [String] -> [String] -> String -> FnkSpec
compile ghc_args plugin_args basename = do
  let act io = do
        success_flag <- io
        succeeded success_flag `shouldBe` True
      pending_in_win = ["p03.hs", "p06.hs", "p09.hs", "p10.hs"]
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
  compile' "fail to" (\io -> io `shouldThrow` \(SomeException _) -> True)

-- Compile source code file. The test executable can act as preprocessor, used
-- to compile Finkel source codes.
compile'
  :: String -> (IO SuccessFlag -> IO ()) -> [String] -> [String] -> String
  -> FnkSpec
compile' msg wrap ghc_args plugin_args basename =
  describe ("compile " ++ basename ++ " with ghc plugin") $ do
    it ("should " ++ msg ++ " compile " ++ basename) $ \ftr -> do
      libdir <- getLibDirFromGhc
      me <- getExecutablePath
      let act = quietly $ runGhc (Just libdir) $ do
            hsc_env <- getSession

            let sp = StaticPlugin (PluginWithArgs plugin plugin_args)
                fnk_args =["-F", "-pgmF", me
                          ,"-i" ++ pdir] ++
                          ghc_args
                args = map noLoc (ftr_pkg_args ftr ++ fnk_args)
                dflags0 = hsc_dflags hsc_env
#if MIN_VERSION_ghc(9,2,0)
            logger <- getLogger
            (dflags1, _, _) <- parseDynamicFlags logger dflags0 args
#else
            (dflags1, _, _) <- parseDynamicFlags dflags0 args
#endif
            void (setSessionDynFlags dflags1)

            hsc_env2 <- getSession
            setFinkelPlugin hsc_env2 sp

            t <- guessTarget (pdir </> basename) Nothing
            setTargets [t]
            load LoadAllTargets

      wrap act

setFinkelPlugin :: GhcMonad m => HscEnv -> StaticPlugin -> m ()
setFinkelPlugin hsc_env sp =
#if MIN_VERSION_ghc(9,2,0)
  void (setSession (hsc_env {hsc_static_plugins = [sp]}))
#else
  void (setSessionDynFlags ((hsc_dflags hsc_env) {staticPlugins = [sp]}))
#endif

pdir :: FilePath
pdir = "test" </> "data" </> "plugin"

#endif /* ghc >= 8.6.0 */
