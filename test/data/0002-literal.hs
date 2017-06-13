module Main where
import SExpr.Form3
{-# LINE 9 "test/data/0002-literal.lisp" #-}
main
  = do { print "string literal";
         print 42;
         print ();
         print [1, 2, 3, 4, 5];
         print
           [if (>) 2 3 then
                do { x <- return 100;
                     y <- return 23;
                     return ((+) x y) }
            else
                return 123,
            Left "foo"] }
