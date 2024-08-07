;;; -*- mode: finkel -*-
;;;; | Simple Finkel REPL.
;;;;
;;;; This implementation uses two threads: one for reading and printing,
;;;; and another for evaluating and modifying the FnkEnv. Using `MVar'
;;;; containing `Code' to communicate between the threads. This design
;;;; shall be easier to support reading forms from other sources than
;;;; line oriented user input, e.g. network sockets.

(defmodule Finkel.Tool.Command.Repl
  (export replMain repl-env)
  (import-when [:compile]
    ;; finkel-core
    (Finkel.Prelude))
  (import
   ;; base
   (Control.Exception [throwIO])
   (Control.Monad [mplus])
   (Control.Monad.IO.Class [(MonadIO ..)])
   (System.Console.GetOpt
    [(ArgDescr ..) (ArgOrder ..) (OptDescr ..) getOpt usageInfo])
   (System.Environment [getProgName])

   ;; finkel-kernel
   (Language.Finkel)
   (Language.Finkel.Fnk [(FnkEnv ..) EnvMacros makeEnvMacros mergeMacros])
   (Language.Finkel.Options [fromFnkEnvOptions fnkEnvOptionsUsage])
   (Language.Finkel.SpecialForms [specialForms])

   ;; finkel-core
   (Finkel.Core)

   ;; Internal
   (Finkel.Tool.Internal.CLI)
   (Finkel.Tool.Internal.Exception)
   (Finkel.Tool.Internal.Loop)
   (Finkel.Tool.Internal.Macro.Repl)
   (Finkel.Tool.Internal.Types)))



;;; Exported

(defn (:: replMain (=> (CLI m) (-> [String] (m ()))))
  "Main entry point function for REPL."
  [args]
  (lept [all-descrs (++ repl-options repl-fnk-env-options)
         (, repl-opts ghc-opts) (partition-descrs all-descrs args)
         initial-option (make-initial-option ghc-opts)]
    (case (getOpt Permute all-descrs repl-opts)
      (, o _other []) (do-repl (foldl (flip id) initial-option o))
      (, _ _ es) (liftIO (throwIO (ArgumentErrors "repl" es))))))

(defn (:: repl-env FnkEnv)
  "Environment value used by the Finkel REPL."
  (lept [macros (mergeMacros specialForms replMacros)]
    (defaultFnkEnv {(= envContextModules ["Prelude"])
                    (= envMacros macros)
                    (= envDefaultMacros macros)
                    (= envQualifyQuotePrimitives True)})))


;;; Internal

(data ReplMode
  Help
  Run)

(data ReplOption
  (ReplOption {(:: repl-mode ReplMode)
               (:: repl-listen-port (Maybe Int))
               (:: repl-input-path (Maybe FilePath))
               (:: repl-prompt (Maybe String))
               (:: repl-quiet Bool)
               (:: repl-init-form Code)
               (:: repl-ghc-options [String])
               (:: repl-fnk-env FnkEnv)}))

(defn (:: make-initial-option (-> [String] ReplOption))
  [ghc-options]
  (ReplOption {(= repl-mode Run)
               (= repl-listen-port Nothing)
               (= repl-input-path Nothing)
               (= repl-prompt Nothing)
               (= repl-quiet False)
               (= repl-init-form greet)
               (= repl-ghc-options ghc-options)
               (= repl-fnk-env repl-env)}))

(defn (:: repl-options [OptDescr (-> ReplOption ReplOption)])
  [(Option [] ["help"]
           (NoArg (\o (o {(= repl-mode Help)})))
           "Show this help and exit")
   (Option [] ["listen"]
           (OptArg (\mb-port o
                     (lept [port (mplus (fmap read mb-port) (Just 50321))]
                       (o {(= repl-mode Run)
                           (= repl-listen-port port)})))
                   "PORT")
           "Listen to port (default: 50321)")
   (Option [] ["file"]
           (ReqArg (\file o (o {(= repl-input-path (Just file))}))
                   "FILE")
           "File to get input from")
   (Option [] ["prompt"]
           (ReqArg (\str o (o {(= repl-prompt (Just str))}))
                   "TEXT")
           "Prompt for input (default: '> ')")
   (Option [] ["quiet"]
           (NoArg (\o (o {(= repl-quiet True)
                          (= repl-prompt (Just ""))
                          (= repl-init-form '(:begin))})))
           "Suppress message from REPL")])

(defn (:: repl-fnk-env-options [(OptDescr (-> ReplOption ReplOption))])
  (fromFnkEnvOptions (\f o
                       (o {(= repl-fnk-env (f (repl-fnk-env o)))}))))

(defn (:: do-repl (=> (CLI m) (-> ReplOption (m ()))))
  [ro]
  (case (repl-mode ro)
    Help print-usage
    Run (liftIO (start-repl (repl-ghc-options ro)
                            (repl-input-path ro)
                            (repl-listen-port ro)
                            (repl-fnk-env ro)
                            (maybe mempty (\p (mempty {(= prompt-string p)}))
                                   (repl-prompt ro))
                            (repl-init-form ro)))))

(defn (:: greet Code)
  "Form containing initial message for the REPL."
  '(System.IO.putStrLn
    "Hit `Ctrl-d' or type ,q to quit, type ,? for help."))

(defn (:: print-usage (=> (CLI m) (m ())))
  (do (<- name (liftIO getProgName))
      (putString
       (unlines
        [(concat ["USAGE: " name " repl [OPTIONS]"])
         ""
         "Start interactive REPL."
         ""
         (usageInfo "OPTIONS:\n" repl-options)
         (fnkEnvOptionsUsage "DEBUG OPTIONS:\n")
         "  Other options are passed to ghc."]))))

(defn (:: replMacros EnvMacros)
  "Default macros imported in REPL. These macros always get imported after
loading compiled modules."
  (macrolet [(the-macros ()
               `[,@(map (\mac `(, ,mac ,(make-symbol mac)))
                        (: "repl_macro" (exported-macros Finkel.Core)))])]
    (makeEnvMacros (the-macros))))
