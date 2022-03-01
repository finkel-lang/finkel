;;; -*- mode: finkel -*-

(:doc "GHC plugin for compiling Finkel source codes.")

(module Finkel.Core.Plugin
  plugin)

;;; finkel-kernel
(import Language.Finkel (defaultFnkEnv))
(import Language.Finkel.Fnk ((FnkEnv ..) (FnkInvokedMode ..)
                             makeEnvMacros mergeMacros))
(import Language.Finkel.Plugin (pluginWith))
(import Language.Finkel.SpecialForms (specialForms))

;;; Internal
(import Finkel.Core)
(import Finkel.Core.Internal.Ghc)

;;; Compile time modules

(:require Finkel.Core)

(:eval-when-compile
  (import Prelude)
  (import Language.Finkel)
  (import Finkel.Core.Internal.Stage2))

;;; The plugin function

(:doc "The plugin to compile Finkel source code.

This plugin could not be loaded before the /downsweep/ phase of the ghc
compilation manager, need other way to parse the module header to resolve the
home package module dependencies.")
(:: plugin Plugin)
(= plugin
  (macrolet [(core-macros ()
               `[,@(map (\mac `(, ,mac ,(make-symbol mac)))
                        (exported-macros Finkel.Core))] )]
    (let ((= myFnkEnv (defaultFnkEnv {(= envMacros myMacros)
                                      (= envDefaultMacros myMacros)
                                      (= envInvokedMode GhcPluginMode)}))
          (= coreMacros (makeEnvMacros (core-macros)))
          (= myMacros (mergeMacros specialForms coreMacros)))
      (pluginWith "Finkel.Plugin" myFnkEnv))))
