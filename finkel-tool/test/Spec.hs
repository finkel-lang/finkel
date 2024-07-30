;;; -*- mode: finkel -*-

(defmodule Main
  (import
   ;; hspec
   (Test.Hspec)

   ;; Internal
   (CLITest)
   (GhcTest)
   (MainTest)
   (ReplMacroTest)
   (ReplTest)
   (TestAux)))

(defn (:: main (IO ()))
  (do (<- etf makeEvalTestFns)
      (hspec (do (afterAll-
                  (etf-cleanup etf)
                  (do (describe "CLITest" cliTests)
                      (describe "GhcTest" ghcTests)
                      (describe "MainTest" mainTests)
                      (describe "ReplTest" (replTests etf))
                      (describe "ReplMacroTest" (replMacroTests etf))))
                 (listenTests etf)))))

