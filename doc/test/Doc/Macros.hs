;;; -*- mode: finkel -*-
;;;; Test codes for "macros.rst"

(defmodule Doc.Macros
  (export spec)
  (import
   ;; base
   (Data.Version [makeVersion])
   (System.Info [os])

   ;; filepath
   (System.FilePath [</>])

   ;; hspec
   (Test.Hspec [(Spec) beforeAll_ describe])

   ;; Internal
   (Doc.TestAux)))

(defn (:: spec Spec)
  (lept [dir (</> "include" "macros")
         is-osx-or-win (|| (== os "darwin") (== os "mingw32"))
         skips [(, "begin.console"
                   (\version _
                     (> (makeVersion [8 8 0]) version)))
                (, "quasiquote.console"
                   (\version _
                     (|| is-osx-or-win
                         (<= (makeVersion [9 4 0]) version))))
                (, "quasiquote904.console"
                   (\version _
                     (|| is-osx-or-win
                         (< version (makeVersion [9 4 0])))))]]
    (beforeAll_
     (remove-compiled [(</> dir "quasiquote")
                       (</> dir "require")
                       (</> dir "RequireMe")])
     (describe "macros in finkel"
       (run-console-tests dir skips)))))
