;;; -*- mode: lisp -*-
;;;
;;; Simple example for `import'.

(module Main)

(import Data.Maybe)

(= main
  (putStrLn (foo (Just "bar"))))

(= (foo x)
  (fromMaybe "foo" x))
