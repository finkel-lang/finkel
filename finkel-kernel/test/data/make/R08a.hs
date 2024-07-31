;;; -*- mode: finkel -*-

(:require R08b)

(module R08a)

(import Language.Finkel)

(:: r08a Macro)
(= r08a (Macro (const (return (toCode (r08b))))))
