;;; -*- mode: finkel -*-
(defmodule Doc
  (export main)
  (import
   ;; hspec
   (Test.Hspec (hspec))

   ;; Internal
   (qualified Doc.FinkelExecutable)
   (qualified Doc.BuildingPackage)
   (qualified Doc.Macros)
   (qualified Doc.LanguageSyntax)))

(defn (:: main (IO ()))
  (hspec (do Doc.FinkelExecutable.spec
             Doc.BuildingPackage.spec
             Doc.Macros.spec
             Doc.LanguageSyntax.spec)))
