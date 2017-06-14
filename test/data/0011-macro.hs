module Main where
import SK.Core.Form
import SK.Core.SKC
m1 :: Macro
{-# LINE 8 "test/data/0011-macro.lisp" #-}
m1
  = (\ form
       -> return
            (List
               [Atom (ASymbol "putStrLn"),
                List
                  [Atom (ASymbol "++"), Atom (AString "Hello, "),
                   toForm (car form)]]))
{-# LINE 12 "test/data/0011-macro.lisp" #-}
main = putStrLn ((++) "Hello, " "macro")
