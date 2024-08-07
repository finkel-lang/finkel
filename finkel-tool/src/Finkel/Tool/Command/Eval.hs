;;; -*- mode: finkel -*-
;;;; Eval sub command

(defmodule Finkel.Tool.Command.Eval
  (export evalMain
          eval-and-exit
          eval-and-exit-with-args)
  (require
   ;; finkel-core
   (Finkel.Tool.Internal.Macro.Ghc))
  (import
   ;; base
   (Control.Concurrent [MVar newEmptyMVar newMVar takeMVar])
   (Control.Exception [displayException throwIO])
   (Control.Monad.IO.Class [(MonadIO ..)])
   (System.Console.GetOpt
    [(ArgDescr ..) (ArgOrder ..) (OptDescr ..) getOpt usageInfo])
   (System.Environment [getProgName])

   ;; finkel-kernel
   (Language.Finkel.Fnk [(Fnk) FnkEnv runFnk])
   (Language.Finkel.Form [Code])
   (Language.Finkel.Lexer [evalSP])
   (Language.Finkel.Options [fromFnkEnvOptions fnkEnvOptionsUsage])
   (Language.Finkel.Reader [sexpr])

   ;; Internal
   (Finkel.Tool.Command.Repl [repl-env])
   (Finkel.Tool.Internal.CLI)
   (Finkel.Tool.Internal.Eval)
   (Finkel.Tool.Internal.Exception)
   (Finkel.Tool.Internal.Types)))

(imports-from-ghc
 (GHC.Data.StringBuffer [stringToStringBuffer]))



;;; Exported

(defn (:: evalMain (=> (CLI m) (-> [String] (m ()))))
  [args]
  (lept [all-opts (++ eval-opts eval-fnk-env-opts)
         (, os gs) (partition-descrs all-opts args)
         mk-opt (foldl (flip id) initial-eval-option)
         (, o _ es) (getOpt Permute all-opts os)]
    (case es
      [] (do-eval (mk-opt o) gs)
      _ (liftIO (throwIO (ArgumentErrors "eval" es))))))


;;; Internal

(data EvalOption
  (EvalOption {(:: eo-help Bool)
               (:: eo-fnk-env FnkEnv)}))

(defn (:: initial-eval-option EvalOption)
  (EvalOption {(= eo-help False)
               (= eo-fnk-env repl-env)}))

(defn (:: eval-opts [(OptDescr (-> EvalOption EvalOption))])
  [(Option [] ["help"]
           (NoArg (\o (o {(= eo-help True)})))
           "Show this help and exit")])

(defn (:: eval-fnk-env-opts [(OptDescr (-> EvalOption EvalOption))])
  (fromFnkEnvOptions (\f o (o {(= eo-fnk-env (f (eo-fnk-env o)))}))))

(defn (:: print-eval-help (=> (CLI m) (m ())))
  (do (<- me (liftIO getProgName))
      (putString
       (unlines
        [(concat ["USAGE: " me " eval [OPTIONS] FORM"])
         ""
         "Evaluate given FORM expression."
         ""
         (usageInfo "OPTIONS:\n" eval-opts)
         (fnkEnvOptionsUsage "DEBUG OPTIONS:\n")
         others-passed-to-ghc]))))

(defn (:: do-eval (=> (CLI m) (-> EvalOption [String] (m ()))))
  [eo args]
  (if (eo-help eo)
    print-eval-help
    (lept [(, ghc-args mb-str) (separate-args args)
           parse (. (evalSP sexpr (Just "<eval>")) stringToStringBuffer)]
      (liftIO
       (case (fmap parse mb-str)
         (Just (Right form)) (eval-and-exit ghc-args (eo-fnk-env eo) form)
         (Just (Left err)) ($ throwIO FinkelToolException displayException err)
         Nothing (throwIO NoEvalInput))))))

(defn (:: separate-args (-> [String] (, [String] (Maybe String))))
  [args]
  (case args
    [] (, [] Nothing)
    [x] (, [] (Just x))
    (: x xs) (lept [(, ghc-opts mb-form) (separate-args xs)]
               (, (: x ghc-opts) mb-form))))

(defn (:: eval-and-exit (-> [String] FnkEnv Code (IO ())))
  (eval-and-exit-with-args []))

(defn (:: eval-and-exit-with-args (-> [String] [String] FnkEnv Code (IO ())))
  [wrapper-args ghc-args-0 fnk-env form]
  ;; Adding "-v0" to front of ghc arguments, to suppress module compilation
  ;; messages.
  (lept [hdl (error "eval-and-exit: uninitialized handle")
         ghc-args-1 (: "-v0" ghc-args-0)]
    (runFnk
     (do (<- (, in-mv out-mv) (make-in-and-out form))
         (eval-once wrapper-args ghc-args-1 hdl in-mv)
         (liftIO (case-do (takeMVar out-mv)
                   (Right msg) (putStr msg)
                   (Left errs) ($ throwIO FinkelToolException errs))))
     fnk-env)))

(defn (:: make-in-and-out (-> Code (Fnk (, (MVar Input) (MVar Result)))))
  [form]
  (liftIO
   (do (<- out-mv newEmptyMVar)
       (<- in-mv (newMVar (Input Prompt form out-mv)))
       (return (, in-mv out-mv)))))
