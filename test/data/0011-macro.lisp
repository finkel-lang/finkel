;;; -*- mode: lisp -*-

;;; File containing forms using macros.

(module Main)

(import SK.Core.Form)
(import SK.Core.Macro)

;; (set-macro-transformer m1
;;   (\ (str)
;;    (returnE (nlForm
;;              `(putStrLn (++ "Hello, " ~(lTFormToForm (fTail str))))))))

(defn f0
  (\ (str)
   (putStrLn (++ "Hello, " str))))

(defn main
  (do (putStrLn "hello, macro")
      ;; (m1 "user defined macro from `m1'.")
      ))
