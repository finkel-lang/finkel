;;; -*- mode: finkel -*-

(module PluginTest)

;; hspec
(import Test.Hspec)

;;; Compile time home package modules

(:require Finkel.Core)
(:eval-when-compile
  (import Finkel.Prelude))

;;; Imports

;; base
(import Control.Monad (void))
(import Control.Monad.IO.Class ((MonadIO ..)))

;; ghc
(import GHC
        ((LoadHowMuch ..) DynFlags (GhcMonad ..)
         guessTarget parseDynamicFlags load setTargets noLoc runGhc
         succeeded setSessionDynFlags))

;; filepath
(import System.FilePath (</>))

;; finkel-kernel
(import Language.Finkel.Fnk (getLibDirFromGhc))
(import Language.Finkel.Plugin (setFinkelPluginWithArgs))

;; Internal
(import Finkel.Core.Plugin (plugin))

(cond-expand
  [(<= 902 :ghc)
   (import GHC.Driver.Env.Types ((HscEnv ..)))]
  [(<= 900 :ghc)
   (import GHC.Driver.Types ((HscEnv ..)))]
  [otherwise
   (import HscTypes ((HscEnv ..)))])

;;; Tests

(defn (:: pluginTests Spec)
  (describe "plugin"
    (compile "c01.hs")))

(defn (:: compile (-> String Spec))
  [file]
  (lept [go (do (<- hsc-env0-b getSession)
                (void (setSessionDynFlags (hsc-dflags hsc-env0-b)))

                (<- hsc-env2 getSession)
                (lept [pp-args (cond-expand
                                 [(<= 906 :ghc) []]
                                 [otherwise ["-F" "-pgmF" "fnkpp"]])
                       fnk-args (++ pp-args ["-fno-code" (++ "-i" pdir)])])
                (<- dflags1 (parseDynFlags hsc-env2 fnk-args))
                (void (setSessionDynFlags dflags1))

                (setFinkelPluginWithArgs plugin [])

                (<- t (cond-expand
                        [(<= 904 :ghc)
                         (guessTarget (</> pdir file) Nothing Nothing)]
                        [otherwise
                         (guessTarget (</> pdir file) Nothing)]))
                (setTargets [t])
                (load LoadAllTargets))]
    (it (++ "should compile " file)
      (do (<- libdir getLibDirFromGhc)
          (<- success-flag (runGhc (Just libdir) go))
          (shouldBe (succeeded success-flag) True)))))

(defn (:: pdir FilePath)
  (</> "test" "data" "plugin"))

(defn (:: parseDynFlags (=> (MonadIO m) (-> HscEnv [String] (m DynFlags))))
  [hsc-env args]
  (do (cond-expand
        [(<= 902 :ghc)
         (<- (, df _ _) (parseDynamicFlags (hsc-logger hsc-env)
                                           (hsc-dflags hsc-env)
                                           (map noLoc args)))]
        [otherwise
         (<- (, df _ _) (parseDynamicFlags (hsc-dflags hsc-env)
                                           (map noLoc args)))])
      (pure df)))

