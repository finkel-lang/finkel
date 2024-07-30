;;;; File: my-second-package/src/FnkCodes.hs

(defmodule FnkCodes
  (export fnkfactorial))

(defn (:: fnkfactorial (-> Int Int))
  [n]
  (if (<= n 1)
      n
      (* n (fnkfactorial (- n 1)))))
