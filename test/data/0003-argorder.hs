module Main where
{-# LINE 5 "test/data/0003-argorder.lisp" #-}
main = (>>) (print (f 3 4)) (print (factorial 10))
{-# LINE 9 "test/data/0003-argorder.lisp" #-}
f a b
  = (+)
      ((*) a (g ((+) a b) ((+) a b) a))
      ((*) (g ((*) b b) ((*) a a) ((*) a b)) b)
{-# LINE 13 "test/data/0003-argorder.lisp" #-}
g x y z = (*) ((+) x ((*) y z)) ((+) y ((*) x z))
{-# LINE 17 "test/data/0003-argorder.lisp" #-}
factorial n = if (==) n 1 then 1 else (*) n (factorial ((-) n 1))
