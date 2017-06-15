;;; File containing forms using macros.

(module Main)

(import SK.Core)

(define-macro m1
  (\ (form)
   (return `(putStrLn (++ "Hello, " ,(car form))))))

(defn main
  (m1 "macro"))
