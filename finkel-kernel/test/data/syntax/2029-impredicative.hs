;;; -*- mode: finkel -*-

;;; File containing codes using `ImpredicativeTypes' language extension.
;;; According to the ghc documentation, the `ImpredicativeTypes' extension did
;;; exist since ghc 6.10, but was unreliable until ghc 9.2.

%p(LANGUAGE ImpredicativeTypes)

(module Main)

(:: f (-> (Maybe (forall a (-> [a] [a]))) (Maybe (, [Int] [Char]))))
(= f (Just g) (Just (, (g [1 2 3]) (g "hello"))))
(= f Nothing Nothing)

(= main
  (print (f (Just reverse))))
