;;; -*- mode: finkel -*-

(module Main)

(:require M6.A hiding (m6a2))
(:require M6.B (m6b1))

(:: m6a2 (-> Int (IO ())))
(= m6a2 x (putStrLn (++ "From main9.m6a2: " (show x))))

(:: m6b2 (-> Int (IO ())))
(= m6b2 x (putStrLn (++ "From main9.m6b2: " (show x))))

(:: main (IO ()))
(= main
  (do (m6a1)
      (m6a2 42)
      (m6b1)
      (m6b2 42)))
