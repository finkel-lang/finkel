;;; -*- mode: finkel -*-

(:require R01)

(module R07b)

(import Language.Finkel)

(:: r07b Macro)
(= r07b (Macro (const (return (toCode (foo-macro))))))
