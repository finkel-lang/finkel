;;; -*- mode: finkel -*-

(:require M3 (greet))
(:require Control.Monad)

(module Main)

(:: main (IO ()))
(= main
  (greet "From main4.main"))
