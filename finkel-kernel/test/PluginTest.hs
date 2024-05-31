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
import Control.Monad          (void)
import System.Info            (os)

#if MIN_VERSION_ghc(9,6,0)
import Control.Monad.IO.Class (MonadIO(..))
#endif

#if (MIN_VERSION_ghc(9,6,0) || !MIN_VERSION_ghc(9,4,0))
import Control.Exception      (SomeException (..))
#endif

-- filepath
import System.Environment     (getExecutablePath)
import System.FilePath        ((</>))

-- ghc
import GHC
import GHC_Driver_Env         (HscEnv (..))
import GHC_Plugins            (PluginWithArgs (..), StaticPlugin (..))

#if MIN_VERSION_ghc(9,6,0)
import GHC.Runtime.Loader     (initializeSessionPlugins)
#endif

#if MIN_VERSION_ghc(9,4,0)
import GHC_Plugins            (Plugins(..), emptyPlugins)
#endif

-- hspec
import Test.Hspec

-- finkel-kernel
import Language.Finkel.Fnk    (getLibDirFromGhc)
import Language.Finkel.Plugin (plugin)

#if MIN_VERSION_ghc(9,6,0)
import Language.Finkel        (defaultFnkEnv)
import Language.Finkel.Hooks  (finkelHooks)
#endif

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
      compile ["-optF", "--warn-interp=False"] ["--verbose=3"] "p03.hs"
#if !MIN_VERSION_ghc(9,6,0)
      compile ["-optF", "--ignore"] ["--ignore"] "p04.hs"
#endif
      compile ["-ddump-parsed-ast"] [] "p05.hs"
      compile ["-optF", "--warn-interp=False"] [] "p06.hs"
      compile [] [] "p08.hs"

      compile ["-v", "-optF", "--warn-interp=False"] ["--verbose=0"] "p09.hs"
      compile ["-v", "-optF", "--warn-interp=False"] ["--verbose=3"] "p09.hs"
      compile ["-v", "-optF", "--warn-interp=False"] ["--verbose=3"] "p10.hs"

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
            let sp = StaticPlugin (PluginWithArgs plugin plugin_args)

            hsc_env1 <- getSession

            let dflags0 = hsc_dflags hsc_env1

#if MIN_VERSION_ghc(9,6,0)
            let fnk_args = ["-i" ++ pdir]
#else
            let fnk_args = ["-F", "-pgmF", _me, "-i" ++ pdir]
#endif
                args = map noLoc (ftr_pkg_args ftr ++ fnk_args ++ ghc_args)

#if MIN_VERSION_ghc(9,2,0)
            logger <- getLogger
            (dflags1, _, _) <- parseDynamicFlags logger dflags0 args
#else
            (dflags1, _, _) <- parseDynamicFlags dflags0 args
#endif
            void (setSessionDynFlags dflags1)

#if MIN_VERSION_ghc(9,6,0)
            hsc_env2 <- getSession
            hsc_env3 <- liftIO $ finkelHooks "PluginTest" defaultFnkEnv
                                             plugin_args hsc_env2
            void (setSession hsc_env3)
            initializeSessionPlugins
#endif

            hsc_env4 <- getSession
            setFinkelPlugin hsc_env4 sp

#if MIN_VERSION_ghc(9,4,0)
            t <- guessTarget (pdir </> basename) Nothing Nothing
#else
            t <- guessTarget (pdir </> basename) Nothing
#endif
            setTargets [t]
            load LoadAllTargets

      wrap act

setFinkelPlugin :: GhcMonad m => HscEnv -> StaticPlugin -> m ()
setFinkelPlugin hsc_env sp =
#if MIN_VERSION_ghc(9,4,0)
  void (setSession (hsc_env {hsc_plugins=emptyPlugins {staticPlugins=[sp]}}))
#elif MIN_VERSION_ghc(9,2,0)
  void (setSession (hsc_env {hsc_static_plugins = [sp]}))
#else
  void (setSessionDynFlags ((hsc_dflags hsc_env) {staticPlugins = [sp]}))
#endif

pdir :: FilePath
pdir = "test" </> "data" </> "plugin"

#endif /* ghc >= 8.6.0 */
