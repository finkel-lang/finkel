;;; File containing forms using macros.

(module Main)

(import SK.Core)

(defmacro m1 (x)
  (return `(putStrLn (++ "Hello, " ,x))))

(macrolet ((m2 (a b c)
             (return `(,a (+ ,b ,c)))))
  (:: f1 (-> Int (-> Int (IO ()))))
  (= (f1 x y)
    (m2 print x y)))

(:: main (IO ()))
(= main
  (do (m1 "macro")
      (f1 11 31)))
