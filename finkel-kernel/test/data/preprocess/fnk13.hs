;;; -*- mode: finkel -*-

(defmodule Main
  (invalid-form)
  (import
   Symbol_is_expanded_to_empty_form)
  (import-when invalid_phase
    (Control.Monad)))

(:: main (IO ()))
(= main (putStrLn "preprocess/fnk13.hs"))
