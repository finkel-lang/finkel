;;; -*- mode: finkel -*-

(module Main)

(:: main (IO ()))
(= main (print (:quasiquote foo bar buzz)))
