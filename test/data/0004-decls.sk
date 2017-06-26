;;;; Forms containing type declarations.

(module Main)

;; Unit type.
(:: foo ())
(= foo
  ())

;;; Simple function type.
(:: bar (-> String (IO ())))
(= (bar str)
  (putStrLn (++ "From bar: " str)))

;;; Another function type, taking multiple arguments.
(:: buzz (-> Int (-> Bool (-> String (IO ())))))
(= (buzz i b s)
  (do (putStrLn (++ "Int: " (show i)))
      (putStrLn (++ "Bool: " (show b)))
      (putStrLn (++ "String: " (show s)))))

;;; Function taking higher order function.
(:: quux (-> (-> Int Int) Int))
(= (quux f)
  (f 6))

;;; Function taking list.
(:: listy (-> [Int] (IO ())))
(= (listy xs)
  (mapM_ print xs))

;;; Function with type variable.
(:: tv01 (-> a (-> [b] Int)))
(= (tv01 x ys)
  (length ys))

;;; Main.
(:: main (IO ()))
(= main
  (do (print foo)
      (bar "BAR")
      (buzz 1 True "buzz")
      (print (quux (\ (n) (* (+ n 1) n))))
      (listy [1 2 3])
      (print (tv01 True [1 2 3]))))
