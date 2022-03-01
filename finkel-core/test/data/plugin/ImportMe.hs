;;; -*- mode: finkel -*-

(defmodule ImportMe)

(defn (:: f1 (-> Int (IO ())))
  [n]
  (putStrLn (if (even n)
              "even"
              "odd")))
