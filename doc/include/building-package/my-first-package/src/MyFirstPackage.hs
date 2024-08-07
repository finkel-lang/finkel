;;;; File: my-first-package/src/MyFirstPackage.hs

(defmodule MyFirstPackage
  (export factorial))

(defn (:: factorial (-> Integer Integer))
  "Compute factorial of the given number.

This function does not support negative numbers. If the argument was
negative, constantly returns @-1@.

==== __Example__

>>> (factorial 10)
3628800
>>> (factorial -42)
1
"
  [n]
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))
