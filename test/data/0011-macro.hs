{-# LINE 1 "test/data/0011-macro.hs" #-}
module Main where
import SK.Core
m1 :: Macro
m1
  = (\ form
       -> return
            (List
               [Atom (ASymbol "putStrLn"),
                List
                  [Atom (ASymbol "++"), Atom (AString "Hello, "),
                   toForm (car form)]]))
main = putStrLn ((++) "Hello, " "macro")
