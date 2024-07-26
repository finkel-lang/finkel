;;; -*- mode: finkel -*-

(:doc "Module without explicit export entities.")
(module Main)

(:dh1 "Level 1")
(:dh2 "Level 2")
(:dh3 "Level 3")

(:doc "Docmentation for 'foo'.")
(:: foo (-> Int Int))
(= foo succ)

(:dh1 "Level 1")
(:doc$ main "Named comment.")

(:doc "Documentation for 'main'.")
(:: main (IO ()))
(= main (print (foo 41)))
