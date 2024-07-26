;;;  GADTs with unusable UNPACK pragmas.

%p(LANGUAGE GADTs)

(module Main)

(data (G2 a)
  (:: G2a (-> %p(UNPACK) !(Maybe a) (G2 a)))
  (:: G2b (-> %p(UNPACK) !Int (G2 Int)))
  (:: G2c (-> %p(UNPACK) !a %p(UNPACK) !Int (G2 a))))

(instance (=> (Show a) (Show (G2 a)))
  (= show (G2a a) (concat ["G2a (" (show a) ")"]))
  (= show (G2b a) (concat ["G2b (" (show a) ")"]))
  (= show (G2c a b) (concat ["G2c (" (show a) " " (show b) ")"])))

(:: gadt2 (-> Int (IO ())))
(= gadt2 n
  (do (print (G2a (Just #'x)))
      (print (G2b n))
      (print (G2c n 43))))

(:: main (IO ()))
(= main (gadt2 42))
