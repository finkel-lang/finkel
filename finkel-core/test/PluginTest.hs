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
(import Language.Finkel.Hooks (finkelHooks))

;; Internal
(import Finkel.Core.Plugin (plugin coreFnkEnv))

(cond-expand
  [(<= 904 :ghc)
   (:begin
     (import GHC.Driver.Env.Types ((HscEnv ..)))
     (import GHC.Driver.Plugins ((PluginWithArgs ..) (StaticPlugin ..)
                                 (Plugins ..) emptyPlugins)))]
  [(<= 902 :ghc)
   (:begin
     (import GHC.Driver.Env.Types ((HscEnv ..)))
     (import GHC.Driver.Plugins ((PluginWithArgs ..) (StaticPlugin ..))))]
  [(<= 900 :ghc)
   (:begin
     (import GHC.Driver.Session ((DynFlags ..)))
     (import GHC.Driver.Types ((HscEnv ..)))
     (import GHC.Driver.Plugins ((PluginWithArgs ..) (StaticPlugin ..))))]
  [otherwise
   (:begin
     (import HscTypes ((HscEnv ..)))
     (import DynFlags ((DynFlags ..)))
     (import Plugins ((PluginWithArgs ..) (StaticPlugin ..))))])

;;; Tests

(defn (:: pluginTests Spec)
  (describe "plugin"
    (compile "c01.hs")))

(defn (:: compile (-> String Spec))
  [file]
  (lept [sp (StaticPlugin (PluginWithArgs plugin []))
         go (do (<- hsc-env0
                  (>>= getSession
                       (. liftIO (finkelHooks "PluginTest" coreFnkEnv []))))
                (setSession hsc-env0)

                (<- hsc-env0-b getSession)

                (void (setSessionDynFlags (hsc-dflags hsc-env0-b)))
                (<- hsc-env1 getSession)

                (setFinkelPlugin hsc-env1 sp)

                (<- hsc-env2 getSession)
                (lept [pp-args (cond-expand
                                 [(<= 906 :ghc) []]
                                 [otherwise ["-F" "-pgmF" "fnkpp"]])
                       fnk-args (++ pp-args ["-fno-code" (++ "-i" pdir)])])
                (<- dflags1 (parseDynFlags hsc-env2 fnk-args))
                (void (setSessionDynFlags dflags1))

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

(defn (:: setFinkelPlugin
        (=> (GhcMonad m) (-> HscEnv StaticPlugin (m ()))))
  [hsc-env sp]
  (cond-expand
    [(<= 904 :ghc)
     (void (setSession (hsc-env {(= hsc-plugins
                                   (emptyPlugins
                                    {(= staticPlugins [sp])}))})))]
    [(<= 902 :ghc)
     (void (setSession (hsc-env {(= hsc-static-plugins [sp])})))]
    [otherwise
     (void (setSessionDynFlags
            ((hsc-dflags hsc-env) {(= staticPlugins [sp])})))]))

(defn (:: parseDynFlags
        (=> (MonadIO m) (-> HscEnv [String] (m DynFlags))))
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

