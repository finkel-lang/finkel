;;; -*- mode: finkel -*-
;;; Commands

(defmodule Finkel.Tool.Command
  (export commands)
  (import
   (Finkel.Tool.Command.Eval)
   (Finkel.Tool.Command.Help)
   (Finkel.Tool.Command.Make)
   (Finkel.Tool.Command.Repl)
   (Finkel.Tool.Command.Run)
   (Finkel.Tool.Command.Sdist)
   (Finkel.Tool.Command.Version)
   (Finkel.Tool.Internal.CLI)))

(defn (:: commands [Command])
  "Available commands in the `finkel' executable."
  [(Command "eval" "evaluate given form" evalMain)
   (Command "help" "show help information" (helpMain commands))
   (Command "make" "compile source codes" makeMain)
   (Command "repl" "start interactive REPL" replMain)
   (Command "run" "run function in module" runMain)
   (Command "sdist" "create source tarballs" sdistMain)
   (Command "version" "show version" versionMain)])
