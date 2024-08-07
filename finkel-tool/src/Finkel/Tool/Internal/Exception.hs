;;; -*- mode: finkel -*-

(defmodule Finkel.Tool.Internal.Exception
  (export
   (FinkelToolException ..)
   finkel-tool-exception-handler)
  (import
   ;; base
   (Control.Exception [(Exception ..) handle])
   (System.Environment [getProgName])
   (System.Exit [exitFailure])
   (System.IO [hPutStrLn stderr])
   (System.IO.Unsafe [unsafePerformIO])))

(data FinkelToolException
  (:doc "Error with command line arguments.")
  (ArgumentErrors String        ; Name of the command.
                  [String]      ; Error messages.
                  %_end)

  (:doc "No input given for eval command.")
  NoEvalInput

  (:doc "Generic exception for finkel-tool package.")
  (FinkelToolException String)
  (deriving Eq Show))

(instance (Exception FinkelToolException)
  (defn displayException
    [(ArgumentErrors cmd msgs)] (++ (unlines msgs) (brief-usage cmd))
    [NoEvalInput] (++ "eval: No input given.\n" (brief-usage "eval"))
    [(FinkelToolException msg)] msg))

(defn (:: brief-usage (-> String String))
  [cmd]
  (++ "Try `" (unsafePerformIO getProgName) " help " cmd "' for usage."))

(defn (:: finkel-tool-exception-handler (-> (IO a) (IO a)))
  (lefn [(:: handler (-> FinkelToolException (IO a)))
         (handler [e]
           (do ($ (hPutStrLn stderr) displayException e)
               exitFailure))]
    (handle handler)))
