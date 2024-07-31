;;; -*- mode: finkel -*-

(:require R07b)

(module R07a)

(import Language.Finkel)

(:: r07a Macro)
(= r07a (Macro (const (return (toCode (r07b))))))
