;;; -*- mode: finkel -*-

(module Main)

(import M02)
(import M03)

(:: main (IO ()))
(= main
  (do m02
      m03))
