;;; -*- mode: finkel -*-

(module M5 m5)

(import M4)

(:: m5 (IO ()))
(= m5 (putStrLn (++ "From M5.m5: m4b=" m4b)))
