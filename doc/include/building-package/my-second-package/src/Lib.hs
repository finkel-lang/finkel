;;;; File: my-second-package/src/Lib.hs

(defmodule Lib
  (export someFunc)
  (import (HsCodes [hsfactorial fnkfactorial])))

(defn (:: someFunc (IO ()))
  (putStrLn
   (++ "From `Lib.someFunc':\n"
       "  hsfactorial 10  : " (show (hsfactorial 10)) "\n"
       "  fnkfactorial 10 : " (show (fnkfactorial 10)))))
