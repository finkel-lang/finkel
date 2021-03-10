;;;; File containing codes using `DerivingStrategies' language extension

%p(LANGUAGE DeriveAnyClass
            DerivingStrategies
            GeneralizedNewtypeDeriving)

(module Main)

(class (C a))

(newtype Buzz (Buzz Double)
  (deriving Eq Ord)
  (deriving stock Read Show)
  (deriving newtype Num Fractional Floating)
  (deriving anyclass C))

(:: main (IO ()))
(= main (print (:: 42 Buzz)))
