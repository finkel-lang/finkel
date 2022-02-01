;;; -*- mode: finkel -*-

;;; Macro codes

(:eval-when-compile
  (import Prelude)
  (import Language.Finkel)
  (:: m1 Macro)
  (= m1
    (Macro
     (const (pure '(putStr (unlines ["====================="
                                     "From dummy02/exe01.hs"
                                     "====================="])))))))

;;; Module sources code

(module Main)

(:: main (IO ()))
(= main (print True))


