;;; -*- mode: finkel -*-

(:require R06a)

(module R06)

(:: main (IO ()))
(= main (putStrLn (r06a)))
