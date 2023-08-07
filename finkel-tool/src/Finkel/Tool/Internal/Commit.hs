;;; -*- mode: finkel -*-
;;;; Module containing function to get git commit ID

(:require Finkel.Core)

(defmodule Finkel.Tool.Internal.Commit
  (export get-git-commit)
  (import
   ;; base
   (Control.Exception [(SomeException ..) catch])
   (System.Exit [(ExitCode ..)])

   ;; process
   (System.Process [readProcess readProcessWithExitCode])))

(defn (:: is-dirty (IO Bool))
  (case-do (readProcessWithExitCode "git" ["diff" "--quiet"] [])
    (, ExitSuccess _ _) (return False)
    _ (return True)))

(defn (:: get-git-commit (IO (Maybe String)))
  (catch (case-do (fmap lines (readProcess "git"
                                           ["rev-parse" "--short=7" "HEAD"]
                                           []))
           (: hash _) (do (<- dirty is-dirty)
                          (return (Just (++ hash (if dirty "-dirty" "")))))
           _ (return Nothing))
    (\ (SomeException _) (return Nothing))))
