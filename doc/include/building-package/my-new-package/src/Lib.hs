;;; -*- mode: finkel -*-
(defmodule Lib
  (export someFunc))

(defn (:: someFunc (IO ()))
  (putStrLn "Hello from my-new-package"))
