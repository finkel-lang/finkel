;;; -*- mode: finkel -*-

(module Main)

(:eval-when-compile
  (import Prelude)
  (import Language.Finkel))

(:eval-when-compile
  (:: m1 Macro)
  (= m1 (Macro (\_ (return `(putStrLn "From m1"))))))

(:: main (IO ()))
(= main (m1))
