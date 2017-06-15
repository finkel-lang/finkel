{-# LINE 1 "test/data/0009-type.hs" #-}
module Main where
main :: IO ()
main
  = do { print foo;
         bar "BAR";
         buzz 1 True "buzz";
         print (quux (\ n -> (*) ((+) n 1) n));
         listy [1, 2, 3] }
foo :: ()
foo = ()
bar :: String -> IO ()
bar str = putStrLn ((++) "From bar: " str)
buzz :: Int -> Bool -> String -> IO ()
buzz i b s
  = do { putStrLn ((++) "Int: " (show i));
         putStrLn ((++) "Bool: " (show b));
         putStrLn ((++) "String: " (show s)) }
quux :: (Int -> Int) -> Int
quux f = f 6
listy :: [Int] -> IO ()
listy xs = mapM_ print xs
