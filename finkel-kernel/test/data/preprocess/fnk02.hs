;;; -*- mode: finkel -*-

(:eval-when-compile
  (import Prelude)
  (import Language.Finkel)
  (:: m1 Macro)
  (= m1 (Macro (const (pure '(putStrLn "preprocess/fnk02.hs"))))))

(module Main)

(:: main (IO ()))
(= main (m1))
