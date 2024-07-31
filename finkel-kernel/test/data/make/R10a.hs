;;; -*- mode: finkel -*-

(module R10a)

(import Language.Finkel)
(import R10b)

(:: r10a Macro)
(= r10a (Macro (const (return (toCode r10b)))))
