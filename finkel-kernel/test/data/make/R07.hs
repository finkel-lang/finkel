;;; -*- mode: finkel -*-

;;; R07 --require-> R07a --require-> R07b --require-> R01

(:require R07a)

(module R07)

(:: main (IO ()))
(= main (putStrLn (r07a)))
