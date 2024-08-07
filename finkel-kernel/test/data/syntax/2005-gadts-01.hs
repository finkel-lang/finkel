;;; GADTs

%p(LANGUAGE GADTs KindSignatures RankNTypes)

(module Main)

(import Data.Kind)

(data (Expr a)
  (:: I (-> Int (Expr Int)))
  (:: B (-> Bool (Expr Bool)))
  (:: Add (-> (Expr Int) (Expr Int) (Expr Int)))
  (:: Mul (-> (Expr Int) (Expr Int) (Expr Int)))
  (:: Eq (=> (Eq a) (-> (Expr a) (Expr a) (Expr Bool)))))

(:: eval (-> (Expr a) a))
(= eval e
  (case e
    (I n) n
    (B b) b
    (Add x y) (+ (eval x) (eval y))
    (Mul x y) (* (eval x) (eval y))
    (Eq x y) (== (eval x) (eval y))))

(:: gadt1 (IO ()))
(= gadt1
  (print (eval (Eq (Mul (Add (I 10) (I 11)) (I 2))
                   (I 42)))))

;; GADTs with UNPACK pragmas.

(data (G2 a)
  (:: G2a (-> !(Maybe a) (G2 a)))
  (:: G2b (-> %p(UNPACK) !Int (G2 Int)))
  (:: G2c (-> !a %p(UNPACK) !Int (G2 a))))

(instance (=> (Show a) (Show (G2 a)))
  (= show (G2a a) (concat ["G2a (" (show a) ")"]))
  (= show (G2b a) (concat ["G2b (" (show a) ")"]))
  (= show (G2c a b) (concat ["G2c (" (show a) " " (show b) ")"])))

(:: gadt2 (-> Int (IO ())))
(= gadt2 n
  (do (print (G2a (Just #'x)))
      (print (G2b n))
      (print (G2c n 43))))

;; GADTs with documentation comments.

(:doc "Documentation for top level `G3' data type declaration.")
(data (G3 a)
  (:doc "Documentation for `G3a'.")
  (:: G3a (-> a (G3 a)))
  (:doc "Documentation for `G3Int'")
  (:: G3Int (-> Int (:doc^ "An integer number.")
                (G3 Int))))

(:doc "Documentation for `Show' instance of `G3'.")
(instance (=> (Show a) (Show (G3 a)))
  (= show g3
    (case g3
      (G3a a) (++ "G3a" (show a))
      (G3Int n) (++ "G3Int " (show n)))))

(:: gadt3 (-> Int (IO ())))
(= gadt3 n
  (do (print (G3a True))
      (print (G3Int n))))

;;; GADTs with `deriving'

(data (Maybe1 a)
  (:: Nothing1 (Maybe1 a))
  (:: Just1 (-> a (Maybe1 a)))
  (deriving Eq Show))

(:: gadt4 (-> Int (IO ())))
(= gadt4 n
  (print [Nothing1 (Just1 n)]))

;;; XXX: GADTs with multiple constructors with single signature

;; (data Multi
;;   (:: (MA MB) Multi)
;;   (:: MC (-> Int Multi)))

;;; XXX: GADTs with record syntax

;; (data Person
;;   (:: Adult (-> {name String children [Person]} Person))
;;   (:: Child (=> (Show a) (-> {name !String funny a} Person))))

;; (:: gadt5 (-> Int (IO ())))
;; (= gadt5 n
;;   (do (let ((= adult (Adult {name "foo" children [child]}))
;;             (= child (Chilc [name "bar" funny n]))))
;;       (putStrLn (name adult))
;;       (putStrLn (name child))
;;       (mapM_ (. print funny) (children adult))))

;;; GADTs with kind signature

(data Ze)
(data (Su n))

(data (:: Vec (-> Type Type Type))
  (:: Nil (Vec a Ze))
  (:: Cons (-> a (Vec a n) (Vec a (Su n)))))

;;; GADT and RankNTypes

(data (Equal a b)
  (:: Refl (Equal a a)))

(:: subst (-> (Equal a b) (=> (~ a b) r) r))
(= subst Refl r r)

;;; Main function

(:: main (IO ()))
(= main
  (do gadt1
      (gadt2 42)
      (gadt3 42)
      (gadt4 42)))
