;;; -*- mode: finkel -*-
;;;; IO related function for REPL

(defmodule Finkel.Tool.Internal.IO
  (export
   read-form
   read-print-loop
   with-io-redirect)
  (require
   ;; finkel-core
   (Finkel.Tool.Internal.Macro.Ghc))
  (import
   ;; base
   (Control.Concurrent
    [MVar ThreadId killThread newEmptyMVar putMVar takeMVar throwTo])
   (Control.Exception [(AsyncException ..) catch throwIO])
   (Control.Monad.Catch [(MonadMask ..) bracket])
   (Control.Monad [when])
   (Control.Monad.IO.Class [(MonadIO ..)])
   (Data.List [intercalate isPrefixOf isSubsequenceOf])
   (GHC.IO.Handle [hDuplicate hDuplicateTo])
   (System.IO
    [Handle (SeekMode ..) hClose hFlush hGetLine hSeek hSetFileSize stdout])
   (System.IO.Error [isEOFError])

   ;; deepseq
   (Control.DeepSeq)

   ;; finkel-kernel
   (Language.Finkel)
   (Language.Finkel.Lexer [evalSP])
   (Language.Finkel.Reader [sexpr])

   ;; finkel-core
   (Finkel.Core.Functions [make-symbol])

   ;; Internal
   (Finkel.Tool.Internal.CLI)
   (Finkel.Tool.Internal.Types)))

(imports-from-ghc
 (GHC.Data.StringBuffer [appendStringBuffers stringToStringBuffer]))


;;; Read and print loop

(defn (:: read-print-loop
        (=> (MonadIO cl) (CLI cl) (HasReplState cl)
            (-> Code (MVar Input) ThreadId (cl ()))))
  "Loop for reading input and printing the output.

Tracks the state of intermediate S-expression from input, and continue
reading the input until successful parse result."
  [init-form to-mvar eval-tid]
  (lefn [(go [result-mv]
           (do (<- st0 getReplState)
               (lefn [(prompt
                        (if (null (pending-input st0)) (prompt-string st0) ""))
                      (reset-pending [st]
                        (st {(= pending-input Nothing)}))])
               (<- mb-input
                 ;; Handle interrupt signals thrown while waiting for input, to
                 ;; handle `Ctrl-C' key presses without valid evaluation form,
                 ;; and to refresh intermediate user inputs in Repl state.
                 (handleInterrupt (do (putReplState (reset-pending st0))
                                      (return (Just [])))
                                  (withInterrupt (getString prompt))))
               (maybe quit (go1 st0 result-mv) mb-input)))
         (go1 [st0 result-mv line]
           (case line
             (: h tl) (| ((== line "(quit)")
                          quit)
                         ((null (pending-input st0))
                          (== #', h)
                          (if (isSubsequenceOf tl "quit")
                            quit
                            (go-command result-mv tl)))
                         (otherwise
                          (go-line result-mv line)))
             [] (go result-mv)))
         (go-command [result-mv lin]
           ;; Using raw symbol for non-mangled REPL commands, to skip replacing
           ;; hyphens to underscores.  Otherwise, command arguments like
           ;; "ghc-boot" will be replaced to "ghc_boot" by the parser.
           (if (mangled-command lin)
             (go-line result-mv (concat ["(repl-macro " lin ")"]))
             (go-form result-mv (as-repl-macro lin))))
         (go-line [result-mv line]
           (do (<- mb-form (read-form line))
               (maybe (go result-mv) (go-form result-mv) mb-form)))
         (go-form [result-mv form]
           ;; Handle interrupt signals thrown while evaluation thread is
           ;; working. Give a chance to interrupt here, after parsing input and
           ;; before printing outputs.
           (do (handleInterrupt
                (do (liftIO (throwTo eval-tid UserInterrupt))
                    (print-io result-mv))
                (withInterrupt
                 (do (liftIO (putMVar to-mvar (Input Prompt form result-mv)))
                     (print-io result-mv))))
               (go result-mv)))
         (quit
           ($ liftIO killThread eval-tid))]

    ;; Print the result from boot expression, then start the loop.
    (do (<- result-mv (liftIO newEmptyMVar))
        (liftIO (putMVar to-mvar (Input Prompt init-form result-mv)))
        (print-io result-mv)
        (go result-mv))))

(defn (:: print-io (=> (MonadIO m) (CLI m)
                       (-> (MVar Result) (m ()))))
  [result-mv]
  (lefn [(pr [str]
           (when (not (null str))
             (>> (putString str) (liftIO (hFlush stdout)))))]
    (case-do (liftIO (takeMVar result-mv))
      (Right str) (pr str)
      (Left str) (pr str))))

(defn (:: read-form (=> (HasReplState repl) (MonadIO repl)
                        (-> String (repl (Maybe Code)))))
  "Read single S-expression form."
  [input0]
  (do (<- st getReplState)
      (lefn [(input1 (stringToStringBuffer (: #'\n input0)))
             (put-and-return [pending ret]
               (do (putReplState (st {(= pending-input pending)}))
                   (return ret)))])
      (<- input2
        (liftIO (maybe (pure input1)
                       (flip appendStringBuffers input1)
                       (pending-input st))))
      (case (evalSP sexpr (Just "<interactive>") input2)
        (Right forms) (put-and-return Nothing (Just forms))
        (Left _err) (put-and-return (Just input2) Nothing))))

;;; IO redirect

(defn (:: with-io-redirect
        (=> (MonadIO m) (MonadMask m)
            (-> Handle (m a) (m (, a String)))))
  "Execute given action with redirecting stdout to given 'Handle'."
  [hdl action]
  (bracket
   (liftIO (do (<- stdout2 (hDuplicate stdout))
               (hSetFileSize hdl 0)
               (hSeek hdl AbsoluteSeek 0)
               (hDuplicateTo hdl stdout)
               (return stdout2)))
   (\stdout2
     (liftIO (do (hDuplicateTo stdout2 stdout)
                 (hClose stdout2))))
   (\_stdout2
     (do (<- x action)
         (liftIO (do (hFlush stdout)
                     (hSeek hdl AbsoluteSeek 0)
                     (<- ls (get-lines hdl))
                     (lept [contents (intercalate "\n" ls)])
                     (deepseq contents (return (, x contents)))))))))

(defn (:: get-lines (-> Handle (IO [String])))
  (lefn [(go [acc hdl]
           (catch (do (<- l (hGetLine hdl))
                      (go (: l acc) hdl))
             (\e
               (if (isEOFError e)
                 (return (reverse acc))
                 (throwIO e)))))]
    (go [])))


;;; Auxiliary

(defn (:: mangled-command (-> String Bool))
  [lin]
  (lept [commands ["expand" "expand!" "info" "kind" "load" "type"]]
    (case (words lin)
      (: w _) (any (isPrefixOf w) commands)
      _ False)))

(defn (:: as-repl-macro (-> String Code))
  [str]
  `(repl-macro ,@(map make-symbol (words str))))
