;;; -*- mode: finkel -*-

(module Main)

(import qualified M1)
(import qualified M2)

(:: main (IO ()))
(= main
  (do (putStrLn "From main5.hs")
      M1.main
      M2.main))
