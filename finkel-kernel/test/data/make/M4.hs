;;; -*- mode: finkel -*-

(module M4 m4 m4a m4b)

(import M4.A)
(import M4.B)

(:: m4 (IO ()))
(= m4 (putStrLn (++ "M4.m4: m4a=" (show m4a) ", m4b=" (show m4b))))
