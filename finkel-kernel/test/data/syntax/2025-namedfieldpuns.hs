;;;; File containing code using `NamedFieldPuns' language extension

%p(LANGUAGE NamedFieldPuns)

(import Data.Monoid ((All All)))
(import qualified Data.Monoid as M)

(data C1
  (C1 {(:: a Int)}))

(data C2
  (C2 {(:: b Int)
       (:: c Int)
       (:: d Int)}))

(:: f1 (-> C1 (IO ())))
(= f1 (C1 {a}) (print a))

(:: f2 C1)
(= f2
  (let ((= a 100))
    (C1 {a})))

(:: f3 (-> C2 Int))
(= f3 (C2 {b (= c 4)}) b)
(= f3 _ 0)

(:: f4 (-> All Bool))
(= f4 (All {M.getAll}) getAll)

(:: main (IO ()))
(= main
  (do (f1 f2)))
