;;; -*- mode: finkel -*-

;;; R09 --require-> R09a --import-> R09b --require-> R01

(:require R09a)

(module R09)

(:: main (IO ()))
(= main (putStrLn (r09a)))
