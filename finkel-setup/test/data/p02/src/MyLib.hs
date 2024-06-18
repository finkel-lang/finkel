;;; -*- mode: finkel -*-

(:doc "Header documentation for MyLib")

(module MyLib someFunc)

(:doc "Documentation for 'someFunc'.")
(:: someFunc (IO ()))
(= someFunc (putStrLn "someFunc"))
