;;; -*- mode: finkel -*-

(module R08b)

(import R01)

(import Language.Finkel)

(:: r08b Macro)
(= r08b (Macro (const (return (toCode foo-function)))))
