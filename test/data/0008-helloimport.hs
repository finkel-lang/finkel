module Main where
import Data.Maybe
{-# LINE 9 "test/data/0008-helloimport.lisp" #-}
main = putStrLn (foo (Just "bar"))
{-# LINE 12 "test/data/0008-helloimport.lisp" #-}
foo x = fromMaybe "foo" x
