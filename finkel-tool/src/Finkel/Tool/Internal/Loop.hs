;;; -*- mode: finkel -*-
;;; Starting REPL

(defmodule Finkel.Tool.Internal.Loop
  (export start-repl acquire-repl cleanup-repl)
  (import-when [:compile]
   ;; finkel-core
   (Finkel.Prelude))
  (import
   ;; base
   (Control.Concurrent [ThreadId forkIO])
   (Control.Concurrent.MVar [MVar newEmptyMVar])
   (Control.Exception [bracket catch throwIO])
   (System.IO [Handle hClose openTempFile])
   (System.IO.Error [isDoesNotExistError])

   ;; directory
   (System.Directory [getTemporaryDirectory removeFile])

   ;; haskeline
   (System.Console.Haskeline
    [defaultBehavior defaultSettings useFile runInputTBehavior])

   ;; finkel-kernel
   (Language.Finkel.Form [Code])
   (Language.Finkel.Fnk [(FnkEnv ..)])

   ;; internal
   (Finkel.Tool.Internal.Eval)
   (Finkel.Tool.Internal.Listen)
   (Finkel.Tool.Internal.IO)
   (Finkel.Tool.Internal.Types)))

;;; Extra imports

(cond-expand
  [(== :os "mingw32")
   (import System.IO (hSetEncoding stdin utf8))]
  [otherwise
   (:begin)])


;;; Starting the REPL

(defn (:: start-repl
        (-> [String]
            (Maybe FilePath)
            (Maybe Int)
            FnkEnv
            ReplState
            Code
            (IO ())))
  "Start REPL, maybe listen to given port number when given."
  [ghc-opts mb-path mb-port fnk-env repl-st init-form]
  (bracket acquire-repl cleanup-repl
           (use-repl ghc-opts mb-path mb-port fnk-env repl-st init-form)))

(defn (:: acquire-repl (IO (, FilePath Handle (MVar Input))))
  "Return a file path and handle for temporary use."
  (do (<- dir getTemporaryDirectory)
      (<- (, path hdl) (openTempFile dir "finkel-repl-.out"))
      (<- mvar newEmptyMVar)
      (return (, path hdl mvar))))

(defn (:: cleanup-repl (-> (, FilePath Handle a) (IO ())))
  "Clean up temporary file."
  [(, path hdl _)]
  (catch (do (hClose hdl)
             (removeFile path))
    (\e
      (if (isDoesNotExistError e)
        (return ())
        (throwIO e)))))

(defn (:: use-repl (-> [String]
                       (Maybe FilePath)
                       (Maybe Int)
                       FnkEnv
                       ReplState
                       Code
                       (, a Handle (MVar Input))
                       (IO ())))
  "Inner work for `start-repl'."
  [ghc-opts mb-path mb-port fnk-env repl-st init-form (, _ hdl in-mv)]
  (do (mapM- (start-listener in-mv) mb-port)
      (<- tid (fork-eval-loop ghc-opts hdl in-mv fnk-env))
      (lept [rpl (read-print-loop init-form in-mv tid)
             behavior (maybe defaultBehavior useFile mb-path)
             run (runInputTBehavior behavior defaultSettings)])

      ;; Using UTF-8 for Windows. See "GHCi.UI.interactiveUI"
      (cond-expand
        [(== :os "mingw32")
         (hSetEncoding stdin utf8)]
        [otherwise
         (return ())])

      (run-repl (run rpl) repl-st)))

(defn (:: start-listener (-> (MVar Input) Int (IO ThreadId)))
  "Start listner in separate thread, and return temporary file for
getting String output from statement."
  [in-mv port]
  (do (putStrLn (++ "Listening on port " (show port)))
      (forkIO (listener (fromIntegral port) in-mv))))
