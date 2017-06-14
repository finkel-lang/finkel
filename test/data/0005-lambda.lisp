;;; -*- mode: lisp -*-

;;; File containing simple expressions.

(module Main)

;;; This function takes single argument which is a function, and applies
;;; 3 and 11 to it.
(= (f1 f)
  (f 3 11))

;; Expression with explicit type signature
(= (f2 n)
  (if (< n (:: 2 Int))
      n
      (+ (f2 (- n 1))
         (f2 (- n 2)))))

;;; Pass lambda to function `foo'.
(= main
  (do (print (f1 (\ (a b) (* a (+ a b)))))
      (print (f2 10))))
