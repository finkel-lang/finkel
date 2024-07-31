;;; -*- mode: finkel -*-

(:require R01)

(module R04)

(:: foo String)
(= foo (foo-macro))

(:: main (IO ()))
(= main (putStrLn (foo-macro)))
