;;; -*- mode: finkel -*-
;;; Module for showing versions.

(defmodule Finkel.Tool.Command.Version
  (export versionMain)
  (require
   ;; finkel-core
   (Finkel.Tool.Internal.Macro.Ghc))
  (import-when [:compile :load]
    ;; base
    (Control.Monad.IO.Class [(MonadIO ..)]))
  (import-when [:compile]
    ;; finkel-core
    (Finkel.Prelude)

    ;; Internal
    (Finkel.Tool.Internal.Commit))
  (import
   ;; base
   (Data.Version [showVersion])
   (System.Console.GetOpt
    [(ArgDescr ..) (ArgOrder ..) (OptDescr ..) getOpt usageInfo])
   (System.Environment [getProgName])
   (System.Info [arch os])

   ;; finkel-core
   (qualified Paths_finkel_core)

   ;; Internal
   (Finkel.Tool.Internal.CLI)))

(imports-from-ghc
 (GHC.Settings.Config [cProjectVersion]))


;;; Exported

(defn (:: versionMain (=> (CLI m) (-> [String] (m ()))))
  "Main function for version sub command."
  [args]
  (case (getOpt Permute version-descrs args)
    (, opts _ []) (show-version (foldr const VersionMessage opts))
    (, _ _ es)  (do (putString (concat es))
                    print-version-help)))


;;; Internal

(data VersionMode
  VersionMessage
  VersionNumeric
  VersionHelp)

(defn (:: version-descrs [OptDescr VersionMode])
  [(Option [#'n] ["numeric"]
           (NoArg VersionNumeric)
           "show numeric version")
   (Option [#'h] ["help"]
           (NoArg VersionHelp)
           "show this help and exit")])

(defn (:: show-version (=> (CLI m) (-> VersionMode (m ()))))
  [mode]
  (case mode
    VersionMessage print-version-message
    VersionNumeric print-version-numeric
    VersionHelp print-version-help))

(defn (:: print-version-message (=> (CLI m) (m ())))
  (macroletM ((get-commit-id _
                (case-do (liftIO get-git-commit)
                  (Just str) (return (toCode (: #'- str)))
                  Nothing (return '""))))
    (do (<- name (liftIO getProgName))
        (putString
         (++ name " " finkel-version (get-commit-id) " " arch "-" os "\n"
             "compiled with ghc " cProjectVersion)))))

(defn (:: print-version-numeric (=> (CLI m) (m ())))
  (putString finkel-version))

(defn (:: print-version-help (=> (CLI m) (m ())))
  (do (<- name (liftIO getProgName))
      (putString
       (unlines
        [(concat ["USAGE: " name " version [OPTIONS]"])
         ""
         "Show version information."
         ""
         (usageInfo "OPTIONS:\n" version-descrs)]))))

(defn (:: finkel-version String)
  (showVersion Paths_finkel_core.version))
