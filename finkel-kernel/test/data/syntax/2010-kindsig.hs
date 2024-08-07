;;;; Kind signatues

%p(LANGUAGE ExplicitForAll
            KindSignatures
            MultiParamTypeClasses)

(module Main)

(import Data.Kind ((Type)))

(data (KS1 (:: m (-> Type Type)) a)
  (KS1 [a]))

(data (KS2 (:: m (-> * *)) a)
  (KS2 [a]))

(newtype (KS3 (:: m (-> Type Type)) a)
  (KS3 [a])
  (deriving Show))

(type (KS4 (:: f (-> Type Type)))
  (f Int))

(class (KS5 (:: f (-> Type Type)) a)
  (:: ks5 (-> (f Int) a)))

(:: f_ks1 (-> (:: Int Type) Int))
(= f_ks1 (+ 1))

(:: f_ks2 (forall (:: a *) (-> a a)))
(= f_ks2 x x)

(:: f_ks3 (forall a (-> a (:: a Type))))
(= f_ks3 x x)

(:: f_ks4 (forall (:: a *) (:: b *) (-> a b a)))
(= f_ks4 x _ x)

(:: main (IO ()))
(= main
  (do (case (KS1 [(:: 1 Int) 2 3])
        (KS1 xs) (print xs))
      (case (KS2 [(:: 4 Int) 5 6])
        (KS2 xs) (print xs))
      (print (KS3 [(:: 7 Int) 8 9]))
      (print (:: (Just 42) (KS4 Maybe)))
      (print (f_ks1 41))
      (print (f_ks2 "f_ks2"))
      (print (f_ks3 "f_ks3"))
      (print (f_ks4 "f_ks4" undefined))))
