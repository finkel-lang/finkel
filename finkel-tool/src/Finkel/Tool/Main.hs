;;; -*- mode: finkel -*-
;;; Main entry point

(defmodule Finkel.Tool.Main
  (export defaultMain)
  (import
   ;; base
   (System.Environment [getArgs])

   ;; Internal
   (Finkel.Tool.Command)
   (Finkel.Tool.Command.Help)
   (Finkel.Tool.Internal.CLI)
   (Finkel.Tool.Internal.Exception)))

(defn (:: defaultMain (IO ()))
  "Main entry point function for the executable."
  (finkel-tool-exception-handler
   (lefn [(go [name rest]
            (maybe (show-usage commands)
                   (flip cmd-act rest)
                   (find-command commands name)))]
     (case-do getArgs
       [] (go "repl" [])
       (: name rest) (go name rest)))))
