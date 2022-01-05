;;;; File containing codes using `Standalonekindsignatures' language extension

%p(LANGUAGE StandaloneKindSignatures)

(module Main)

(import Data.Kind)

(type (:: MyMaybe (-> Type Type)))

(data (MyMaybe a)
  MyNothing
  (MyJust a)
  (deriving Eq Show))

(:: main (IO ()))
(= main
  (do (print (MyJust False))
      (print (:: MyNothing (MyMaybe Int)))))
