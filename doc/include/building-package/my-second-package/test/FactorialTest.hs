;;;; File: FactorialTest.hs

(defmodule FactorialTest
  (export test)
  (import
   (HsCodes)
   (System.Exit (exitFailure))))

(defn (:: test (IO ()))
  (if (== (fnkfactorial 10) (hsfactorial 10))
      (return ())
      exitFailure))
