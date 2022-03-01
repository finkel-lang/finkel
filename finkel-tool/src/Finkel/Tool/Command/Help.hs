;;; -*- mode: finkel -*-
;;; Help utility for Finkel  tool.

(:require Finkel.Core)

(defmodule Finkel.Tool.Command.Help
  (export
   ;; Help command
   helpMain
   show-usage)
  (import
   ;; base
   (Control.Monad.IO.Class [(MonadIO ..)])
   (Data.Foldable [maximumBy])
   (Data.Function [on])
   (System.Environment [getProgName])

   ;; Internal
   (Finkel.Tool.Internal.CLI)))

(defn (:: helpMain (=> (CLI m) (-> [Command] [String] (m ()))))
  "Main function for help command."
  [cmds args]
  (case args
    (: name _ ) (| ((<- (Just cmd) (find-command cmds name))
                    (liftIO (cmd-act cmd ["--help"]))))
    _ (show-usage cmds)))

(defn (:: show-usage (=> (CLI m) (-> [Command] (m ()))))
  "Show usage message generated from given commands."
  [cmds]
  (lefn [(max-len
           (length (maximumBy (on compare length) (fmap cmd-name cmds))))
         (pad [n str]
           (++ str (replicate (- n (length str)) #'\SP)))
         (descr [n cmd]
           (concat ["  " (pad n (cmd-name cmd))
                    "  " (cmd-descr cmd)]))]
    (do (<- name (liftIO getProgName))
        (putString
         (unlines
          (++ [(concat ["USAGE:\n\n   " name " <command> [arguments]"])
               ""
               (concat ["Run \"" name " help <command>\""
                        " for more information."])
               ""
               "COMMANDS:"
               ""]
              (fmap (descr max-len) cmds)))))))
