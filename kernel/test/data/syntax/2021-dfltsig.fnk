;;;; File containing codes using `DefaultSignatures' language extension

%p(LANGUAGE DefaultSignatures)

(module Main)

(class (SPretty a)
  (:: sPpr (-> a String))
  (default (:: sPpr (=> (Show a) (-> a String))))
  (= sPpr show))

(instance (SPretty Bool))

(:: main (IO ()))
(= main (putStrLn (sPpr False)))
