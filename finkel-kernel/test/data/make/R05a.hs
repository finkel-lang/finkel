;;; -*- mode: finkel -*-

(module R05a)

(import Language.Finkel)
(import R01)

(:: r05a Macro)
(= r05a (Macro (const (return (toCode foo-function)))))
