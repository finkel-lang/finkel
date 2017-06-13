module Main where
{-# LINE 8 "test/data/0005-lambda.lisp" #-}
main = print (foo (\ a b -> (*) a ((+) a b)))
{-# LINE 13 "test/data/0005-lambda.lisp" #-}
foo f = f 3 11
