module Main where
main :: IO ()
{-# LINE 6 "test/data/0009-type.lisp" #-}
main
  = do { print foo;
         bar "BAR";
         buzz 1 True "buzz";
         print (quux (\ n -> (*) ((+) n 1) n));
         listy [1, 2, 3] }
foo :: ()
{-# LINE 14 "test/data/0009-type.lisp" #-}
foo = ()
bar :: String -> IO ()
{-# LINE 18 "test/data/0009-type.lisp" #-}
bar str = putStrLn ((++) "From bar: " str)
buzz :: Int -> Bool -> String -> IO ()
{-# LINE 22 "test/data/0009-type.lisp" #-}
buzz i b s
  = do { putStrLn ((++) "Int: " (show i));
         putStrLn ((++) "Bool: " (show b));
         putStrLn ((++) "String: " (show s)) }
quux :: (Int -> Int) -> Int
{-# LINE 28 "test/data/0009-type.lisp" #-}
quux f = f 6
listy :: [Int] -> IO ()
{-# LINE 32 "test/data/0009-type.lisp" #-}
listy xs = mapM_ print xs
