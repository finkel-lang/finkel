;;; -*- mode: finkel -*-

(module Main)

(import M5)
(import M4)

(:: main (IO ()))
(= main (>> m4 m5))
