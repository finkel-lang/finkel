;;;; Scoped Type Variable

%p(LANGUAGE ScopedTypeVariables)

(data Zero)
(data (Succ n))

(type One (Succ Zero))
(type Two (Succ One))
(type Four (Succ (Succ Two)))
(type Six (Succ (Succ Four)))
(type Eight (Succ (Succ Six)))

(class (Nat n)
  (:: toInt (-> n Int)))

(instance (Nat Zero)
  (= toInt _ 0))

(instance (=> (Nat n) (Nat (Succ n)))
  (= toInt _ (+ 1 (toInt (:: undefined n)))))

(:: f_stv01 (IO ()))
(= f_stv01
  (print (map (\ (:: x Int) (+ x 1)) [1 2 3])))

(:: f_stv02 (forall a (-> [a] [a])))
(= f_stv02 xs
  (where ys
    (:: ys [a])
    (= ys (reverse xs))))

(class (STVC a)
  (:: stv_op (-> [a] (Maybe a)))
  (= stv_op xs
    (case (reverse xs)
      (: x _) (Just x)
      [] Nothing)))

(instance (=> (STVC b) (STVC [b]))
  (= stv_op xs
    (case (:: xs [[b]])
      (: ys _) (Just (reverse ys))
      [] Nothing)))

(instance (STVC Bool))

(:: main (IO ()))
(= main
  (do (print (toInt (:: undefined Four)))
      (print (toInt (:: undefined Eight)))
      f_stv01
      (print (f_stv02 "abc"))
      (print (stv_op [True False False]))))
