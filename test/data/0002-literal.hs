{-# LINE 1 "test/data/0002-literal.hs" #-}
module Main where
main
  = do { print "string literal";
         print 42;
         print ();
         print [1, 2, 3, 4, 5];
         print ((:) True ((:) False []));
         print
           [if (>) 2 3 then
                do { x <- return 100;
                     y <- return 23;
                     return ((+) x y) }
            else
                return 123,
            Left "foo"] }
