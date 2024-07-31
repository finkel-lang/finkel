;;; -*- mode: finkel -*-

(:require R01)

(module R06a)

(import Language.Finkel)

(:: r06a Macro)
(= r06a (Macro
         (const (do (let ((= x (foo-macro))))
                    (return (toCode x))))))
