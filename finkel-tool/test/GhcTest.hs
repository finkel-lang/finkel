;;; -*- mode: finkel -*-
%p(LANGUAGE TypeApplications)

(defmodule GhcTest
  (export ghcTests)
  (import-when [:compile]
    ;; Internal
    (Finkel.Prelude))
  (import
   ;; base
   (Control.Exception [(SomeException ..) try])

   ;; hspec
   (Test.Hspec)

   ;; finkel-kernel
   (Language.Finkel)
   (Language.Finkel.Fnk [FnkEnv])

   ;; finkel-tool
   (Finkel.Tool.Internal.Macro.Ghc)))

(defn (:: ghcTests Spec)
  imports-from-ghc-test)

(defn (:: expand-form (-> Macro Code Code Expectation))
  (expand-form-with-env defaultFnkEnv shouldBe))

(defn (:: expand-form-with-env (-> FnkEnv
                                   (-> Code Code Expectation)
                                   Macro Code Code Expectation))
  [fnk-env test macro in-form out-form]
  (>>= (try (runFnk (macroFunction macro in-form) fnk-env))
       (either (. expectationFailure (show @ SomeException))
               (flip test out-form))))

(defn (:: imports-from-ghc-test Spec)
  (describe "imports-from-ghc-test"
    (it "should return import declarations"
      (expand-form
       imports-from-ghc
       '(imports-from-ghc
         (GHC.Driver.Env [(HscEnv ..)])
         (GHC.Types.SourceText [(SourceText ..)])
         (GHC.Utils.Outputable [SDoc]))
       (cond-expand
         [(<= 902 :ghc)
          '(:begin
            (import GHC.Driver.Env ((HscEnv ..)))
            (import GHC.Types.SourceText ((SourceText ..)))
            (import GHC.Utils.Outputable (SDoc)))]
         [(<= 900 :ghc)
          '(:begin
            (import GHC.Driver.Types ((HscEnv ..)))
            (import GHC.Types.Basic ((SourceText ..)))
            (import GHC.Utils.Outputable (SDoc)))]
         [otherwise
          '(:begin
            (import HscTypes ((HscEnv ..)))
            (import BasicTypes ((SourceText ..)))
            (import Outputable (SDoc)))])))))
