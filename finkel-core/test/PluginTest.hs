;;; -*- mode: finkel -*-

(module PluginTest)

;; hspec
(import Test.Hspec)

;;; Compile time home package modules

(:require Finkel.Core)
(:eval-when-compile
  (import Finkel.Prelude))

;;; Tests

(cond-expand
  [(<= 808 :ghc)
   (:begin

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

     ;; Internal
     (import Finkel.Core.Plugin (plugin))

     (cond-expand
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

     (defn (:: pluginTests Spec)
       (describe "plugin"
         (compile "c01.hs")))

     (defn (:: compile (-> String Spec))
       [file]
       (lept [sp (StaticPlugin (PluginWithArgs plugin []))
              go (do (<- hsc-env0 getSession)
                     (void (setSessionDynFlags (hsc-dflags hsc-env0)))

                     (<- hsc-env1 getSession)
                     (setFinkelPlugin hsc-env1 sp)

                     (<- hsc-env2 getSession)
                     (lept [fnk-args ["-F" "-pgmF" "fnkpp" "-fno-code"
                                      (++ "-i" pdir)]])
                     (<- dflags1 (parseDynFlags hsc-env2 fnk-args))
                     (void (setSessionDynFlags dflags1))

                     (<- t (guessTarget (</> pdir file) Nothing))
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
           (pure df))))]

  [otherwise
   (defn (:: pluginTests Spec)
     (describe "plugin"
       (it "compile file via plugin"
         (pendingWith "unsupported ghc version"))))])

