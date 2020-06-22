;;;; File containing codes using `StandaloneDeriving' language extension

%p(LANGUAGE DeriveAnyClass
            DerivingStrategies
            GeneralizedNewtypeDeriving
            StandaloneDeriving)

(module Main)

(class (C a))

(newtype (Foo a) (MkFoo Int))

(deriving instance (Eq (Foo a)))
(deriving instance %p(OVERLAPPING) (Ord (Foo a)))
(deriving stock instance (Show (Foo a)))
(deriving stock instance %p(OVERLAPPABLE) (Read (Foo a)))
(deriving newtype instance (Enum (Foo a)))
(deriving newtype instance (Real (Foo a)))
(deriving newtype instance %p(OVERLAPS) (Integral (Foo a)))
(deriving newtype instance %p(INCOHERENT) (Num (Foo a)))
(deriving anyclass instance (C (Foo a)))

(:: main (IO ()))
(= main (print (== (succ (:: 3 (Foo Char))) 4)))
