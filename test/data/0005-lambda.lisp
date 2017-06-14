;;; -*- mode: lisp -*-

;;; File containing simple expressions.

(module Main)

;;; This function takes single argument which is a function, and applies
;;; 3 and 11 to it.
(= (f1 f)
  (f 3 11))

;; Expression with explicit type signature
(= (f2 n)
  (if (< n (:: 2 Int))
      n
      (+ (f2 (- n 1))
         (f2 (- n 2)))))

;; Expression with empty 'let'
(= (f3 n)
  (let ()
    (+ n 35)))

;; Expression with 'let'. In bindings of `let', 'a' is a integer value
;; 14, and `f' is a function taking two arguments.
(= (f4 n)
  (let ((a 14)
        ((f x y) (+ x y))
        (g (\ (x) (* x 2))))
    (g (f n a))))

;;; Main entry point.
(= main
  (do (print (f1 (\ (a b) (* a (+ a b)))))
      (print (f2 10))
      (print (f3 7))
      (print (f4 7))))
