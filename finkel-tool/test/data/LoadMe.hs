;;; -*- mode: finkel -*-

(defmodule LoadMe
  (export from-load-me))

(defn (:: from-load-me (-> String (IO ())))
  (. putStrLn (++ "LoadMe.from-load-me: ")))
