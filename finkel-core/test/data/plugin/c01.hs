;;; -*- mode: finkel -*-

(defmodule Main
  (import
   (Control.Monad [forM-])
   (ImportMe [f1])))

(defn (:: main (IO ()))
  (forM- (Just 3) f1))
