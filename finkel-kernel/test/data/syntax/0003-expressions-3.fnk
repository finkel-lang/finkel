;;; -*- mode: finkel -*-
;;;
;;; This module contains expressions using `forall' as variable idntifier, which
;;; will show an error from ghc 9.10 in default flag settings.

(module Main)

(:: let1 (-> Int [Int]))
(= let1 n
  (let ((= forall n))
    [forall]))

(:: case1 (-> (, Int Int) Int))
(= case1 (, as forall) (+ as forall))

(:: main (IO ()))
(= main
  (do (print (let1 42))
      (print (case1 (, 19 23)))))
