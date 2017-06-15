{-# LINE 1 "test/data/0006-pattern.hs" #-}
-- | File containing forms with pattern matches.
module Main where
main
  = do { print (fib 10);
         putStrLn (bar Nothing);
         putStrLn (bar (Just undefined));
         buzz (Just 3);
         print (addMaybes Nothing Nothing);
         print (addMaybes (Just 2) Nothing);
         print (addMaybes Nothing (Just 3));
         print (addMaybes (Just 2) (Just 3));
         print (nest1 Nothing);
         print (nest1 (Just (Right 3)));
         print (nest1 (Just (Left True))) }
fib 0 = 0
fib 1 = 1
fib n = (+) (fib ((-) n 1)) (fib ((-) n 2))
bar Nothing = "bar got nothing"
bar _ = "bar got something"
buzz (Just n) = putStrLn ((++) "buzz: " (show n))
buzz _ = putStrLn "buzz got nothing"
addMaybes Nothing Nothing = 0
addMaybes (Just a) Nothing = a
addMaybes Nothing (Just b) = b
addMaybes (Just a) (Just b) = (+) a b
nest1 Nothing = 0
nest1 (Just (Right n)) = n
nest1 (Just (Left True)) = 9999
nest1 (Just (Left False)) = 42
