;;;; File containing codes using `PolyKinds' language extension

%p(LANGUAGE DataKinds
            GADTs
            PolyKinds
            RankNTypes
            TypeFamilies
            TypeOperators)

;; ghc-8.2.x and 8.4.x requires `TypeInType' extension.
%p(LANGUAGE TypeInType)

(module Main)

(import Data.Kind ((Type)))

(data (App f a) (MkApp (f a)))

(:: a1 (App Maybe Int))
(= a1 (MkApp (Just 42)))

(data (T a) (MkT (a Int)))

(:: a2 (App T Maybe))
(= a2 (MkApp (MkT (Just 42))))

(:: print-a2 (IO ()))
(= print-a2
  (case a2
    (MkApp (MkT ji)) (print ji)))

(type family (F1 a)
  (= (F1 'True)  'False)
  (= (F1 'False) 'True)
  (= (F1 x)      x))

(type family (:: (F3 (:: a Bool)) Bool)
  (= (F3 'True)  'False)
  (= (F3 'False) 'True))

(data (Proxy a) Proxy
  (deriving Eq Show))

(:: print-f3 (IO ()))
(= print-f3
  (let ((:: x (Proxy (F3 'True)))
        (= x Proxy))
    (print x)))

(class (HTestEquality (:: t (forall k (-> k Type))))
  (:: hTestEquality (forall k1 k2 (:: a k1) (:: b k2)
                      (-> (t a) (t b) (Maybe (:~~: a b))))))

(data (:: :~~: (forall k1 (-> k1 (forall k2 (-> k2 Type)))))
  (:: HRefl (:~~: a a)))

(instance (HTestEquality ((:~~:) a))
  (= hTestEquality HRefl HRefl (Just HRefl)))

;;; Requires `PolyKinds' language extension.
(type family (:: (TF2 a) k))

(type family (:: TF3 (-> k Type)))

(:: main (IO ()))
(= main
  (do (print-a2)
      (print-f3)))
