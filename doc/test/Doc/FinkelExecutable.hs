;;; -*- mode: finkel -*-
;;;; Test codes for "finkel-executable.rst".

(defmodule Doc.FinkelExecutable
  (export spec)
  (import
   ;; base
   (Data.Version [makeVersion])
   (System.Info [os])

   ;; filepath
   (System.FilePath [</>])

   ;; hspc
   (Test.Hspec [(Spec) before_ describe])

   ;; Internal
   (Doc.TestAux)))

(defn (:: spec Spec)
  (lept [dir (</> "include" "finkel-executable")
         const2 (\ x _ _ x)
         is-osx (== os "darwin")
         is-win (== os "mingw32")
         ghc904 (makeVersion [9 4 0])
         skips
         [(, "finkel-help-make.console" (const2 is-win))
          (, "hello.console"
             (\version _
               (|| is-osx is-win
                   (< ghc904 version))))
          (, "hello904.console"
             (\version _
               (|| is-osx is-win
                   (< version ghc904))))
          (, "hello-prof.console"
             (\ version _build-tool
               ;; XXX: Always skipping
               (|| is-win
                   ;; Skipping in ghc >= 9.0 ...
                   (<= (makeVersion [9 0]) version))))]]
    (before_
     (remove-compiled [(</> dir "hello")])
     (describe "using the finkel executable"
       (run-console-tests dir skips)))))
