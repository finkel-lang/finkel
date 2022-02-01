;;; -*- mode: finkel -*-

(module M02)

(:require M01)

(:: m02 (IO ()))
(= m02
  (do (m01)
      (putStrLn "plugin/M02.hs")))
