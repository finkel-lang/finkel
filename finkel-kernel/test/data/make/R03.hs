;;; -*- mode: finkel -*-

(:require R01)

(module R03)

(:: foo String)
(= foo (foo-macro))
