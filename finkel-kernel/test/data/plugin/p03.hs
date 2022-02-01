;;; -*- mode: finkel -*-

(:eval-when-compile
  (import Prelude)
  (import Language.Finkel)
  (:: m1 Macro)
  (= m1
    (Macro
     (const (pure '(putStrLn "plugin/p03.hs"))))))

(:: main (IO ()))
(= main (m1))
