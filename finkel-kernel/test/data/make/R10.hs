;;; -*- mode: finkel -*-

;;; R10 --require-> R10a --import-> R10b --import-> R01

(:require R10a)

(module R10)

(:: main (IO ()))
(= main (putStrLn (r10a)))
