;;; -*- mode: finkel -*-

(defmodule Main
  (export main foo bar))

(defn (:: main (IO ()))
  "Main function of m01"
  (putStrLn "=== m01.fnk ==="))

(defn (:: foo String)
  "A string value named foo."
  "foo")

(defn (:: bar (-> Int Int))
  "A function named bar"
  [x]
  (* x (+ x 2)))
