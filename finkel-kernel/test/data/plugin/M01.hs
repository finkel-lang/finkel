;;; -*- mode: finkel -*-

(module M01)

(import Language.Finkel)

(:: m01 Macro)
(= m01 (Macro (const (pure '(putStrLn "plugin/M01.hs"))) ))
