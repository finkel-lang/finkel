case expr of -- Haskell
  Just y | even y -> r1
         | odd y, y < 10 -> r2
         | Just z <- lookup y kvs
         , let z' = z * 2
         -> r3 z'
         | otherwise -> r4
