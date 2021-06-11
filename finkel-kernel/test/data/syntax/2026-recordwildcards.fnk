;;;; File containing code using `RecordWildCards' language extension

%p(LANGUAGE RecordWildCards
            NamedFieldPuns)

(import Data.Monoid ((All getAll)))

(data C
  (C {(:: a b c d Int)})
  (deriving Eq Show))

(= f1 (C {(= a 1) ..})
  (+ b c d))
(= f1 _ 0)

(= f2 (C {(= a 1) b ..})
  (+ b c d))
(= f2 _ 1)

(= e
  (C {(= a 111) (= b 222) (= c 333) (= d 444)}))

(= f3
  (let ((= (C {(= a 111) ..}) e))
    [b c d]))

(= f4
  (let ((= a 12) (= b 34) (= c 56) (= d 78))
    (C {..})))

(:: main (IO ()))
(= main
  (do (let ((= c1 (C 1 2 3 4))))
      (print (f1 c1))
      (print (f2 c1))
      (print f3)
      (print f4)))
