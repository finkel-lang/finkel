{-# LINE 1 "test/data/0003-argorder.hs" #-}
module Main where
main = (>>) (print (f 3 4)) (print (factorial 10))
f a b
  = (+)
      ((*) a (g ((+) a b) ((+) a b) a))
      ((*) (g ((*) b b) ((*) a a) ((*) a b)) b)
g x y z = (*) ((+) x ((*) y z)) ((+) y ((*) x z))
factorial n = if (==) n 1 then 1 else (*) n (factorial ((-) n 1))
