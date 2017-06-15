;;; File containing forms using macros.

(module Main)

(import SK.Core.Form)
(import SK.Core.SKC)

(define-macro m1
  (\ (form)
   (return `(putStrLn (++ "Hello, " ,(car form))))))

(defn main
  (m1 "macro"))
