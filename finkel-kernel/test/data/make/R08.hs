;;; -*- mode: finkel -*-

;;; R08 --require-> R08a --require-> R08b --import-> R01

(:require R08a)

(module R08)

(:: main (IO ()))
(= main (putStrLn (r08a)))
