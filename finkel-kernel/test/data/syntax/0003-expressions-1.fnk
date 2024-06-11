;;; -*- mode: finkel -*-

(module Main)

(import Control.Monad (ap))

;;; Simple expressions

(= simple1 a b
  (+ (* a (simple2 (+ a b) (+ a b) a))
     (* (simple2 (* b b) (* a a) (* a b)) b)))

(= simple2 x y z
  (* (+ x (* y z))
     (+ y (* x z))))

(= factorial n
   (if (== n 1)
       1
       (* n (factorial (- n 1)))))

(= simples
  (>> (print (simple1 3 4))
      (print (factorial 10))))

;;; 3.3 Curried Applications and Lambda Abstractions

;;; This function takes single argument which is a function, and applies
;;; 3 and 11 to it.
(= lam1 f
  (f 3 11))

;;; Calls `lam1' defined above.
(= lam2
  (>>
   ;; Lambda WITH spaces between '\' and the first argument.
   (print (lam1 (\ a b
                  (* a (+ a b)))))

   ;; Lambda WITHOUT spaces between '\' and the first argument.
   (print (lam1 (\a b
                  (* a (+ a b)))))))

(= lamexprs
  lam2)

;;; 3.4 Operator Applications

;;; Operator are expanded when it takes more than two literal arguments.
;;; Partial application is supported for left hand side argument only.
(= op1
  (do (print (+ 1 2 3 4 5))
      (print (Prelude.* 1 2 3 4 5))
      (let ((= f (. putStrLn show (+ 3) (* 13)))))
      (f (:: 3 Int))
      ($ putStrLn show not False)))

;; The ':' operator expansion in pattern match.
(= op2 xs
  (case xs
    (: a b c _) (print (+ a b c))
    _ (return ())))

(= opexprs
  (do op1
      (op2 [1 2 3 4])))

;;; 3.7 Lists

(= lie0 (print (:: [] [Int])))
(= lie1 (print [1]))
(= lie2 (print [1 2]))
(= lie3 (print [1 2 3]))
(= lie4 (print [1 2 3 4]))
(= lie5 (print (: 1 (: 2 (: 3 [4 5])))))

(= listexprs
  (do lie0 lie1 lie2 lie3 lie4 lie5))

;;; 3.8 Tuples

(:: tup1 (-> a (-> b (-> c (, a b c)))))
(= tup1 a b c
  (, a b c))

(:: tup2 (-> a b (Maybe (, a b))))
(= tup2 a b
  (ap (ap (return (,)) (return a)) (return b)))

(:: tup3 (-> a b c d (Maybe (, a b c d))))
(= tup3 a b c d
  (ap (ap (ap (ap (return (,,,))
                  (return a))
              (return b))
          (return c))
      (return d)))

(= tupexprs
  (do (print (tup1 #'x 1.23 "4567"))
      (print (tup2 True #'y))
      (print (tup3 True #'a False #'b))))

;;; 3.9 Unit Expressions and Parenthesized Expressions

(= upe1 (print ()))
(= upe2 (print ((((((((((True))))))))))))

(= uparenexprs
  (do upe1 upe2))

;;; 3.10 Arithmetic Sequences

(= ase1 (print (take 10 [1 ..])))
(= ase2 (print (take 10 [1 3 ..])))
(= ase3 (print [2 .. 20]))
(= ase4 (print [2 4 .. 20]))

(= aseqexprs
  (do ase1 ase2 ase3 ase4))

;;; 3.11 List Comprehensions

(= lcmp1 [x | (<- x [1 2 3])])
(= lcmp2 [x | (<- x [1 2 3]) (even x)])
(= lcmp3 [y | (<- x [1 2 3]) (even x) (let ((= y (+ x 1))))])
(= lcmp4 [(, x y) | (<- x [1 2 3]) (even x) (let ((= y (+ x 1))))])

(= lcmpexprs
  (do (print lcmp1)
      (print lcmp2)
      (print lcmp3)
      (print lcmp4)))

;;; 3.12 Let Expressions

;;; Expression with empty 'let'.
(= let1 n
  (let ()
    (+ n 35)))

;;; Expression with 'let'. In bindings of `let', 'a' is a integer value
;;; 14, and `f' is a function taking two arguments.
(= let2 n
  (let ((:: a Int)
        (= a 14)
        (:: f (-> Int Int Int))
        (= f x y
          (+ x y))
        (:: g (-> Int Int))
        (= g (\x (* x 2))))
    (g (f n a))))

;;; Using non-reserved special value names (e.g. `as', `hiding' ...).
(= let3 n
  (let ((= as n)
        (= ccall n)
        (= hiding n)
        (= qualified n)
        (= ! a b (+ a b))
        (= ~~ a b (* a b)))
    [as ccall hiding qualified (! 19 23) (~~ 6 7)]))

;;; `let' with pattern match
(= let4 n
  (let ((:: f (-> Int Int))
        (= f 1 1)
        (= f 2 1)
        (= f k (+ (f (- k 1)) (f (- k 2))))
        (:: g (-> Int Int))
        (= g 0 0)
        (= g n n))
    (g (f n))))

(= letexprs
  (do (print (let1 7))
      (print (let2 7))
      (print (let3 7))
      (print (let4 7))))

;;; 3.13 Case Expressions

(= case1 x
  (case x
    (Just n) (+ n 1)
    _ 0))

(= case2 x
  (case x
    (Right (Just _)) 1
    (Right Nothing)  2
    (Left (Just _))  3
    (Left Nothing)   4))

(:: mbeven (-> Int (Maybe Int)))
(= mbeven n
  (if (even n)
      (Just n)
      Nothing))

(:: case3 (-> (Maybe Int) Int String))
(= case3 x y
  (case x
    (Just n) (| ((odd n) (> n 100)
                 "small odd number")
                ((odd n)
                 "big odd number")
                ((<- (Just m) (mbeven n))
                 (let ((:: k Int)
                       (= k (+ m 1))))
                 (< k 101)
                 "small even number")
                (otherwise
                 "big even number"))
    Nothing  (| ((even y) "y is even")
                (otherwise "y is odd"))))

(:: case4 (-> (Maybe Int) Int String))
(= case4 x y
  (where (case x
           (Just n) (where (| ((even n) (show (f n y)))
                              ((odd n) (++ (h n) (show (f n 1)))))
                      (= f a b (+ a b)))
           Nothing "nothing")
    (= h i
      (replicate (+ i y) #'@))))

;;; Using non-reserved special value names (e.g. `as', `hiding' ...), in
;;; pattern.

(:: case5 (-> (, Int Int Int Int) Int))
(= case5 (, as ccall qualified hiding)
  (sum [as ccall qualified hiding]))

(= caseexprs
  (do (print (case1 (Just 41)))
      (print (case1 Nothing))
      (print (case2 (Right (Just ()))))
      (print (case3 (Just 42) 12))
      (print (case4 (Just 41) 1))
      (print (case5 (, 1 1 1 1)))))

;;; 3.14 Do Expressions

(= showBar x
  (do (putStrLn "String `bar' from showBar.")
      (return x)))

(= listdo
  (do (<- x [1 2 3])
      (<- y [4 5 6])
      [x y]))

(= letdo
  (do (let ((= f 1 1)
            (= f 2 1)
            (= f n (+ (f (- n 1)) (f (- n 2))))))
      (print (f 10))))

(= doexpres
  (do (putStrLn "foo")
      (<- buzz (showBar "buzz"))
      (let ((:: buzz3 buzz4 String)
            (= buzz3 (concat [buzz buzz buzz]))
            (= buzz4 "buzz4")))
      (putStrLn buzz3)
      (putStrLn buzz4)
      (print listdo)
      letdo))

;;; 3.15 Datatypes with Field Labels

(data R1
  (Con1 {(:: field1 Int)
         (:: field2 Bool)})
  (deriving Eq Show))

(:: mkR1 (-> Int (-> Bool R1)))
(= mkR1 a b
  (Con1 {(= field1 a) (= field2 b)}))

(:: fe_01 (IO ()))
(= fe_01
  (do (let ((:: r1 r2 R1)
            (= r1 (Con1 {(= field2 False)
                         (= field1 42)}))
            (= r2 (r1 {(= field1 (* (field1 r1) 2))}))
            (= r3 ((mkR1 21 True) {(= field1 12)}))
            (= as (r1 {(= field1 (* (field1 r1) 2))}))
            (= bs (as {(= field1 (* (field1 as) 2))}))))
      (print r1)
      (print r2)
      (print r3)
      (print as)
      (print bs)))

(:: fe_02 (IO ()))
(= fe_02
  (do (let ((:: as bs R1)
            (= as (Con1 {(= field2 True)
                         (= field1 3)}))
            (= bs (as {(= field1 (* (field1 as) 2))}))))
      (print as)
      (print bs)))

(= fieldexprs
  (do fe_01
      fe_02))

;;; 3.16 Expression Type-Signatures

(= t316a n
  (if (< n (:: 2 Int))
    n
    (+ (t316a (- n 1))
       (t316a (- n 2)))))

(= t316b xs
  (print (map (:: (\x (+ x 1)) (=> (Num a) (-> a a))) xs)))

(= tsigexprs
  (do (print (t316a 10))
      (t316b [1 2 3])
      (t316b [1.0 2.0 3.0])))

;;; 3.17 Pattern Matching

;;; Top level functions with pattern matches.

(= fib 0 0)
(= fib 1 1)
(= fib n (+ (fib (- n 1))
            (fib (- n 2))))

(= bar Nothing "bar got nothing")
(= bar _ "bar got something")

(= buzz (Just n) (putStrLn (++ "buzz: " (show n))))
(= buzz _ (putStrLn "buzz got nothing"))

(= addMaybes Nothing Nothing 0)
(= addMaybes (Just a) Nothing a)
(= addMaybes Nothing (Just b) b)
(= addMaybes (Just a) (Just b) (+ a b))

(= nest1 Nothing 0)
(= nest1 (Just (Right n)) n)
(= nest1 (Just (Left True)) 9999)
(= nest1 (Just (Left False)) 42)

(= lp1 [] 0)
(= lp1 [a] 1)
(= lp1 [(Just x) (Just y)] (+ x y))
(= lp1 [a b] 2)
(= lp1 _ 999)

(= patexprs1
  (do (print (fib 10))
      (putStrLn (bar Nothing))
      (putStrLn (bar (Just undefined)))
      (buzz (Just 3))
      (print (addMaybes Nothing Nothing))
      (print (addMaybes (Just 2) Nothing))
      (print (addMaybes Nothing (Just 3)))
      (print (addMaybes (Just 2) (Just 3)))
      (print (nest1 Nothing))
      (print (nest1 (Just (Right 3))))
      (print (nest1 (Just (Left True))))
      (print (lp1 []))
      (print (lp1 [Nothing]))
      (print (lp1 [Nothing Nothing]))
      (print (lp1 [(Just 28) (Just 14)]))
      (print (lp1 [Nothing (Just 1) Nothing]))))

(:: tupp1 (-> (, a b) a))
(= tupp1 (, a _) a)

(:: tupp2 (-> (, Char Char Char) Char))
(= tupp2 x
  (case x
    (, _ _ c) c))

(:: intpat (-> Int String))
(= intpat x
  (case x
    1 "one"
    2 "two"
    _ "unknown"))

(:: strpat (-> String String))
(= strpat x
  (case x
    "foo" "GOT FOO"
    "bar" "GOT BAR"
    _ "UNKNOWN"))

(:: charpat (-> Char String))
(= charpat x
  (case x
    #'a "GOT A"
    #'b "GOT B"
    _ "UNKNOWN"))

(= patexprs2
  (do (print (tupp1 (, True False)))
      (print (tupp2 (, #'a #'b #'c)))
      (print (intpat 1))
      (print (intpat 2))
      (print (intpat 3))
      (print (strpat "foo"))
      (print (strpat "bar"))
      (print (strpat "buzz"))
      (print (charpat #'a))
      (print (charpat #'X))))

(:: aspat1 (-> (Maybe Char) String))
(= aspat1 mbc
  (case mbc
    (@ x (Just y)) (unwords [(show x) (show y)])
    Nothing        "Nothing"))

(:: ap2a ap2b ap2c Int)
(:: aspat2 [Int])
(= (@ aspat2 [ap2a ap2b ap2c])
  [123 456 789])

(:: ap3 [String])
(= ap3
  (let ((:: p (Either String Int))
        (= (@ p (Right q)) (Right 42)))
    [(show p) (show q)]))

(:: ap4 [String])
(= ap4
  (let ((= (@ as (, a b)) (, #'a #'b))
        (= (@ ccall (, c d)) (, #'c #'d))
        (= (@ hiding (, e f)) (, #'e #'f))
        (= (@ qualified (, g h)) (, #'g #'h)))
    [(show as) (show ccall) (show hiding) (show qualified)]))

(= patexprs3
  (do (print (aspat1 (Just #'p)))
      (print ap2a)
      (print ap2b)
      (print ap2c)
      (print aspat2)
      (print ap3)
      (print ap4)))

(:: lzp1 (-> (Maybe Int) String))
(= lzp1 (@ _mbi ~(Just _i)) "matched")

(:: lzp2 (-> (, Int [Char] (, Char [Int])) Char))
(= lzp2 (, x ~(: y ys) ~(, z (: w ws)))
  (| ((even x) y)
     ((odd x)  z)))

(:: lzp3 (-> (, a (, b c)) String))
(= lzp3 x
  (case x
    ~(, _a ~(, _b _c)) "lzp3"))

(:: lzp4 (-> (, a (, b c)) (IO ())))
(= lzp4 x
  (do (<- ~(, _a ~(, _b _c)) (return x))
      (putStrLn "lzp4")))

(:: lzp5 (-> (, a (, b c)) (IO ())))
(= lzp5
  (\ ~(, _a ~(, _b _c)) (putStrLn "lzp5")))

(:: lzp6 (-> (, a (, b c)) (IO ())))
(= lzp6 x
  (let ((= ~(, _a ~(, _b _c)) x))
    (putStrLn "lzp6")))

(:: lzp7 (-> (, Int (, Char Bool)) (IO ())))
(= lzp7 ~x
  (print x))

(= patexprs4
  (do (print (lzp1 (Just undefined)))
      (print (lzp2 (, 3 undefined (, #'x [1 2 3]))))
      (print (lzp2 (, 4 [#'a #'b] (, undefined []))))
      (print (lzp3 undefined))
      (lzp4 undefined)
      (lzp5 undefined)
      (lzp6 undefined)
      (lzp7 (, 42 (, #'g True)))))

(= lfp1 mb
  (case mb
    (Just {}) "just"
    _         "nothing"))

(= lfp2 r1
  (case r1
    (Con1 {(= field1 i) (= field2 b)}) (if b (* i 7) i)))

(= patexprs5
  (do (print (lfp1 Nothing))
      (print (lfp1 (Just undefined)))
      (print (lfp2 (Con1 42 False)))
      (print (lfp2 (Con1 6 True)))))

(= unitp1 a
  (case a
    () (putStrLn "unitp1: `()' pattern matched.")))

(= unitp2 ()
  (putStrLn "unitp2: `()' pattern matched."))

(= patexprs6
  (do (unitp1 ())
      (unitp2 ())))

(= patexprs
  (do patexprs1
      patexprs2
      patexprs3
      patexprs4
      patexprs5
      patexprs6))

;;; Main

(= main
  (do simples
      lamexprs
      opexprs
      listexprs
      tupexprs
      uparenexprs
      aseqexprs
      lcmpexprs
      letexprs
      caseexprs
      doexpres
      fieldexprs
      tsigexprs
      patexprs))
