;;;; UndecidableInstances, and some others.

%p(LANGUAGE FlexibleInstances
            MonoLocalBinds
            UndecidableInstances)

(module Main)

(class (=> (Show a) (Monoid a) (ShowMonoid a)))

(instance (=> (Show a) (Monoid a) (ShowMonoid a)))

(:: showMonoid (=> (ShowMonoid a) (-> a String)))
(= showMonoid x
  (concat [(show x) "(mempty=" (show (asTypeOf mempty x)) ")"]))

(:: main (IO ()))
(= main
  (do (putStrLn (showMonoid [(:: 1 Int) 2 3]))
      (putStrLn (showMonoid GT))))
