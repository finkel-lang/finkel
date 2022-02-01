;;; -*- mode: finkel -*-

(module M03)

(:require M01)

(:: m03 (IO ()))
(= m03
  (do (m01)
      (putStrLn "plugin/M03.hs")))
