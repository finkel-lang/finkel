{-# LINE 1 "test/data/0005-lambda.hs" #-}
module Main where
f1 f = f 3 11
f2 n
  = if (<) n (2 :: Int) then n else (+) (f2 ((-) n 1)) (f2 ((-) n 2))
f3 n = let in (+) n 35
f4 n
  = let
      a = 14
      f x y = (+) x y
      g = (\ x -> (*) x 2)
    in g (f n a)
main
  = do { print (f1 (\ a b -> (*) a ((+) a b)));
         print (f2 10);
         print (f3 7);
         print (f4 7) }
