module Main where
import SK.Core.Form
import SK.Core.Macro
{-# LINE 15 "test/data/0011-macro.lisp" #-}
f0 = (\ str -> putStrLn ((++) "Hello, " str))
{-# LINE 19 "test/data/0011-macro.lisp" #-}
main = do { putStrLn "hello, macro" }
