;;; -*- mode: finkel -*-

%p(LANGUAGE RankNTypes
            TypeFamilies)
(:doc "
Module      : Main
Description : Module for documentation comment
Copyright   : (c) someone, someyear
License     : GPL-3
Maintainer  : foo@bar.com
Stability   : experimental
Portability : POSIX

Module header documentation.

This comment is written inside `:doc' form.")

(module Main
  (:dh1 "The main function")
  (:doc "Main entry point function.
This function is performed from compiled executable.")
  main

  (:dh1 "Types and classes")
  (:doc$ auxdt)
  (D1 ..) (D2 ..) (D3 ..) (D4 ..) (T1)
  (C1 ..) (C2 ..) (TF1) (DF1)

  (:dh1 "Functions")
  (:doc$ auxfn)
  f1 f2 f3 f4 f5 f6)


;;; Functions

(:doc$ auxfn "Section documentation for auxiliary functions.")

(:doc "Documentation of 'f1'")
(:: f1 (-> String (IO ())))
(= f1 str (putStrLn (++ "From f1: " str)))

(:doc "Documentation of 'f2'")
(:: f2 (-> String (IO ())))
(= f2 (. putStrLn (++ "From f2: ")))

(:doc "Documentation of 'f3'")
(:: f3 (-> Int (:doc^ "Single line comment")
           String
           (:doc^ "Multiple lines comment for the second argument.

Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim
veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea
commodo consequat. Duis aute irure dolor in reprehenderit in voluptate
velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint
occaecat cupidatat non proident, sunt in culpa qui officia deserunt
mollit anim id est laborum.")
           (IO ())
           (:doc^ "Action to print given message for given times")))
(= f3 n msg
  (sequence_ (replicate n (putStrLn msg))) )

(:: f4 (forall a (-> a (:doc^ "Documentation for first argument.")
                     a (:doc^ "Documentation for result."))))
(:doc^ "Documentation for 'f4'.

This function contains unnecessary explicit @forall@ keyword.")
(= f4 x x)

(:: f5
  (-> (forall a (-> a a))
      (:doc^ "Documentation for first arg.")
      (, Char Bool)
      (:doc^ "Documentation for result.")))
(:doc^ "Documentation for 'f5'.

This function contains rank-n type function argument.")

(= f5 f (, (f #'a) (f True)))

(:: f6
  (=> (Show a) (Show b)
      (-> a (:doc^ "Documentation for first arg.")
          b (:doc^ "Documentation for second arg.")
          String (:doc^ "Documentation for result."))))
(:doc^ "Documentation for 'f6'.

Example for writing documentation for argument, with a function
containing type class constraints. This documentation comment includes
@since@ metadata.

@since 1.2.3.4.5.6.7")

(= f6 a b
  (++ "f6: a=" (show a) ", b=" (show b)))


;;; Types and classes

(:doc$ auxdt "Section documentation for auxiliary data and types.")

;;; Unlike haddock comment in Haskell source code, constructor
;;; documentation comments are allowed for ':doc^' forms only.

(:doc "Documentation for data type 'D1'.")
(data (D1 a b)
  (D1a a)
  (:doc^ "Documentation for 'D1a'.

This comment contains empty lines.

To add a line break in generated HTML document, need to add an empty
line. Otherwise, line breaks in source codes will disappear.")

  (D1ab a (:doc^ "The first argument of `D1ab'.")
        b (:doc^ "The 2nd.")
        Int (:doc^ "Documentation for the 3rd argument of `D1ab'.

Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim
veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea
commodo consequat. Duis aute irure dolor in reprehenderit in voluptate
velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint
occaecat cupidatat non proident, sunt in culpa qui officia deserunt
mollit anim id est laborum.")
        Int (:doc^ "The 4th."))
  (:doc^ "Documentation for constructor 'D1ab'.")
  (deriving Eq Show))

(newtype (D2 a)
  (D2 a)
  (:doc^ "Documentation for constructor `D2'."))
(:doc^ "Documentation for top level newtype declaration.")

(data (D3 a)
  (:doc "Documentation for 'D3a'.")
  (D3a {(:: d3_f1 Int) (:doc^ "Documentation for 'd3_f1' field.")

        (:: d3_f2 a) (:doc^ "Documentation for 'd3_f2' field.

Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim
veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea
commodo consequat. Duis aute irure dolor in reprehenderit in voluptate
velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint
occaecat cupidatat non proident, sunt in culpa qui officia deserunt
mollit anim id est laborum.")
        (:: d3_f3 d3_f4 a)
        (:doc^ "Documentation for 'd3_f3' and 'd3_f4' fields.")})

  (:doc "Documentation for 'D3b'.")
  (D3b a a)

  (deriving Eq Show))
(:doc^ "Documentation for top level 'D3' data type declaration.")

(:doc "Documentation for top level `D4' data type declaration.")
(data (D4 a)
  (:doc "Documentation for `D4a'.")
  (D4a {(:doc "Documentation for `d4_f1'.")
        (:: d4-f1 Int)
        (:doc "Documentation for `d4_f2' and `d4_f3'.")
        (:: d4-f2 d4-f3 a)})
  (:doc "Documentation for `D4b'")
  D4b
  (deriving Eq Show))

(type (T1 a)
  (Maybe (, a a))
  (:doc^ "T1 is a synonym of optional pair of __@a@__ values."))
(:doc^ "Documentation for top level 'T1' type synonym.")

(class (C1 a)
  (type (C1T1 a))
  (:doc^ "Documentation for 'C1T1'.")

  (type (C1T2 a))
  (:doc^ "Documentation for 'C1T2'.

Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim
veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea
commodo consequat. Duis aute irure dolor in reprehenderit in voluptate
velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint
occaecat cupidatat non proident, sunt in culpa qui officia deserunt
mollit anim id est laborum.")

  (:: c1_m1 (-> a String))
  (:doc^ "Documentation for `c1_m1' method in 'C1'.")

  (:: c1_m2 (-> a (:doc^ "1st arg.")
                a (:doc^ "2nd arg.")
                String))
  (:doc^ "documentation for `c1_m2' method in 'C1'.

This method takes two __@a@__ arguments. "))

(:doc^ "Documentation for top level 'C1' type class.")

(:doc "Documentation for top level `C2' type class.")
(class (C2 a)
  (:doc "Documentation for `C2T1'.")
  (type (C2T1 a))

  (:doc "Documentation for `c2_m1'.")
  (:: c2-m1 (-> a String))

  (:doc "Documentation for `c2_m2'.")
  (:: c2-m2 (-> (C2T1 a) (:doc^ "1st arg.")
                a (:doc^ "2nd arg.")
                String)))

(:doc "Documentation for instance declaration of `C2' for `Int'.")
(instance (C2 Int)
  (type (C2T1 Int) Int)
  (= c2_m1 show)
  (= c2_m2 a1 a2
    (++ "arg1=" (show a1) ", arg2=" (show a2))))

(type family (TF1 a))
(:doc^ "Documentation for top level 'TF1' type family.")

(type instance (TF1 Bool) Int)
(:doc^ "Documentation for top level `TF1' instance for `Bool'.")

(data family (DF1 a))
(:doc^ "Documentation for top level data family `DF1'.")

(data instance (DF1 Bool) (DF1B Int)
  (deriving Eq Show))
(:doc^ "Documentation for top level `DF1' instance for `Bool'.")

(data instance (DF1 Double)
  (DF1D1 Double)
  (DF1D2 Int)
  (deriving Eq Show))
(:doc^ "Documentation for top level `DF1' instance for `Double'.")

(newtype instance (DF1 Char)
  (DF1C Bool))
(:doc^ "Documentation for top level `DF1' instance for `Char'.")

;;; Main function

(:: main (IO ()))
(:doc^ "Documentation of 'main'.")
(= main
  (do (putStrLn "documentation comment tests.")
      (f1 "foo")
      (f2 "bar")
      (f3 3 "buzz")))
