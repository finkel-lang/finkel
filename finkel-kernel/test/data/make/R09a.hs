;;; -*- mode: finkel -*-

(module R09a)

(import Language.Finkel)
(import R09b)

(:: r09a Macro)
(= r09a (Macro (const (return (toCode r09b)))))
