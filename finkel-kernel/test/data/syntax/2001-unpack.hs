;;;; "UNPACK" and "SPECIALIZE INLINE" pragma

%p(LANGUAGE GADTs)

(module Main)

;;; UNPACK

(data D1
  (C1 %p(UNPACK) (! Int)
      %p(UNPACK) !Char)
  (deriving Eq Show))

(data D2
  (C2 {(:: c2field1 %p(UNPACK) !Int)
       (:: c2field2 %p(UNPACK) (! Char))})
  (deriving Eq Show))

(:: unpackprgm (IO ()))
(= unpackprgm
  (do (print (C1 42 #'x))
      (print (C2 42 #'x))))

;; SPECIALIZE INLNE

(data (Lst e)
  (:: LstInt (-> !Int [Int] (Lst Int)))
  (:: LstPair (-> !Int (Lst e1) (Lst e2) (Lst (, e1 e2)))))

(:: !: (-> (Lst e) Int e))
(= !: (LstInt _ xs) i (!! xs i))
(= !: (LstPair _ l1 l2) i (, (!: l1 i) (!: l2 i)))

%p(SPECIALIZE INLINE (:: !: (-> (Lst Int) Int Int)))
%p(SPECIALIZE INLINE (:: !: (-> (Lst (, a b)) Int (, a b))))

(:: main (IO ()))
(= main unpackprgm)
