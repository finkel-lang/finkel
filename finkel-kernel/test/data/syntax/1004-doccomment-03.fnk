;;; Doccomment with unusable UNPACK pragma

(module Main)

(:doc "Documentation for data type 'D1'.")
(data (D1 a b)
  (D1a a)
  (:doc^ "Documentation for 'D1a'.")

  (D1b %p(UNPACK) !b)
  (:doc^ "Documentation for 'D1b', has unusable UNPACK pragma.

This comment contains empty lines.

A line containing some foo: Foo foo foo foo, foo foo, and foo.")

  (D1ab a (:doc^ "The first argument 'D1ab.")
        b (:doc^ "The 2nd.")
        Int (:doc^ "The 3rd."))

  (deriving Show))

(:: main (IO ()))
(= main
  (print [(D1a True) (D1b False)]))
