;;;; RankNTypes

%p(LANGUAGE RankNTypes)

(module Main)

(:: f3 (-> (forall a (-> a a)) (, Char Bool)))
(= f3 f (, (f #'a) (f True)))

(:: main (IO ()))
(= main
  (print (f3 id)))
