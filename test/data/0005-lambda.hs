module Main where
{-# LINE 9 "test/data/0005-lambda.lisp" #-}
f1 f = f 3 11
{-# LINE 13 "test/data/0005-lambda.lisp" #-}
f2 n
  = if (<) n (2 :: Int) then n else (+) (f2 ((-) n 1)) (f2 ((-) n 2))
{-# LINE 20 "test/data/0005-lambda.lisp" #-}
main
  = do { print (f1 (\ a b -> (*) a ((+) a b)));
         print (f2 10) }
