;;; -*- mode: lisp -*-
;;;
;;; Tests for tokens and literal values.

(module Main)

(import SExpr.Form3)

(= main
  (do (print "string literal") ;; Simple string
      ;; (print "string\" with \escapes.")

      ;; Integer.
      (print 42)

      ;; Floating point number.
      ;; (print 4.2)

      ;; Characters.
      ;; (print \a) (print \space) (print \newline)

      ;; Unit.
      (print ())

      ;; List literals.
      (print [1 2 3 4 5])

      ;; List containing expressions.
      (print [(if (> 2 3)
                  (do (<- x (return 100))
                      (<- y (return 23))
                      (return (+ x y)))
                  (return 123))
              (Left "foo")])))
