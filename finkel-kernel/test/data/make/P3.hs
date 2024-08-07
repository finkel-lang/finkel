;;; -*- mode: finkel -*-

(:require P1)

(module P3)

(:eval-when-compile
  (import Prelude)
  (import Language.Finkel))

(import Language.Finkel)

(:eval-when-compile
  (define-macro define-bar form
    (case (unCode form)
      (List [_ name]) (return
                       `(define-macro ,name _
                          (return '(return "bar from define-bar")))))))

(define-bar bar)
