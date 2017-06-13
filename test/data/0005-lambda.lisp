;;; -*- mode: lisp -*-

;;; File containing simple lambda.

(module Main)

;;; Pass lambda to function `foo'.
(= main
   (print (foo (\ (a b) (* a (+ a b))))))

;;; This function takes single argument which is a function, and applies
;;; 3 and 11 to it.
(= (foo f)
  (f 3 11))
