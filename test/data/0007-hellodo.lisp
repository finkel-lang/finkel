;;; -*- mode: lisp -*-
;;;
;;; Simple example for `do' notation.

(module Main)

(= (showBar x)
  (do (putStrLn "String `bar' from showBar.")
      (return x)))

(= main
  (do (putStrLn "foo")
      (<- buzz (showBar "buzz"))
      (putStrLn buzz)))
