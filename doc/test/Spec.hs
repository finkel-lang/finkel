;;; -*- mode: finkel -*-

(defmodule Main
  (import
   (qualified Doc)))

(defn (:: main (IO ()))
  Doc.main)
