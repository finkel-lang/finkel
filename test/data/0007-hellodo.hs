module Main where
{-# LINE 7 "test/data/0007-hellodo.lisp" #-}
showBar x
  = do { putStrLn "String `bar' from showBar.";
         return x }
{-# LINE 11 "test/data/0007-hellodo.lisp" #-}
main
  = do { putStrLn "foo";
         buzz <- showBar "buzz";
         putStrLn buzz }
