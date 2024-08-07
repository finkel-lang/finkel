;;; -*- mode: finkel -*-
;;; Module for run sub command

(defmodule Finkel.Tool.Command.Run
  (export runMain)
  (import
   ;; base
   (Control.Exception [throwIO])
   (Control.Monad.IO.Class [(MonadIO ..)])
   (System.Console.GetOpt
    [(ArgDescr ..) (ArgOrder ..) (OptDescr ..) getOpt usageInfo])
   (System.Environment [getProgName])

   ;; finkel-kernel
   (Language.Finkel.Fnk [FnkEnv])
   (Language.Finkel.Options [fromFnkEnvOptions fnkEnvOptionsUsage])

   ;; finkel-core
   (Finkel.Core.Functions [make-symbol])

   ;; Internal
   (Finkel.Tool.Command.Eval [eval-and-exit-with-args])
   (Finkel.Tool.Command.Repl [repl-env])
   (Finkel.Tool.Internal.CLI)
   (Finkel.Tool.Internal.Exception)))

(defn (:: runMain (=> (CLI m) (-> [String] (m ()))))
  [args0]
  (lept [(, args1 prog-args) (split-program-args args0)
         all-opts (++ run-opts run-fnk-env-opts)
         (, rargs ghc-args) (partition-descrs all-opts args1)
         (, o _ es) (getOpt Permute all-opts rargs)
         ro (foldl (flip id) initial-run-option o)
         main-fn (make-symbol (mangle-name (ro-main ro)))]
    (if (ro-help ro)
      print-run-help
      (liftIO
       (case es
         [] (eval-and-exit-with-args prog-args
                                     ghc-args
                                     (ro-fnk-env ro)
                                     main-fn)
         _ (throwIO (ArgumentErrors "run" es)))))))

(data RunOption
  (RunOption {(:: ro-help Bool)
              (:: ro-main String)
              (:: ro-fnk-env FnkEnv)}))

(defn (:: initial-run-option RunOption)
  (RunOption {(= ro-help False)
              (= ro-main "main")
              (= ro-fnk-env repl-env)}))

(defn (:: run-opts [(OptDescr (-> RunOption RunOption))])
  [(Option [] ["help"]
           (NoArg (\o (o {(= ro-help True)})))
           "Show this help and exit")
   (Option [] ["main"]
           (ReqArg (\name o (o {(= ro-main name)}))
                   "NAME")
           "Name of the function to run (default: main)")])

(defn (:: run-fnk-env-opts [(OptDescr (-> RunOption RunOption))])
  (fromFnkEnvOptions (\f o (o {(= ro-fnk-env (f (ro-fnk-env o)))}))))

(defn (:: print-run-help (=> (CLI m) (m ())))
  (do (<- me (liftIO getProgName))
      (putString
       (unlines
        [(concat ["USAGE: " me " run [OPTIONS] FILE [-- ARGS]"])
         ""
         "Compile and run given FILE."
         ""
         "Arguments after `--' are passed to the given FILE."
         ""
         (usageInfo "OPTIONS:\n" run-opts)
         (fnkEnvOptionsUsage "DEBUG OPTIONS:\n")
         others-passed-to-ghc]))))

(defn (:: split-program-args (-> [String] (, [String] [String])))
  [xs]
  (case (break (== "--") xs)
    (, pre (: "--" post)) (, pre post)
    (, pre _) (, pre [])))

(defn (:: mangle-name (-> String String))
  (lefn [(replace [c]
           (if (== c #'-) #'_ c))]
    (map replace)))
