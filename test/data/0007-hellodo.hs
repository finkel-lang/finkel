{-# LINE 1 "test/data/0007-hellodo.hs" #-}
module Main where
showBar x
  = do { putStrLn "String `bar' from showBar.";
         return x }
main
  = do { putStrLn "foo";
         buzz <- showBar "buzz";
         putStrLn buzz }
