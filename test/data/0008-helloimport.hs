{-# LINE 1 "test/data/0008-helloimport.hs" #-}
module Main where
import Data.Maybe
main = putStrLn (foo (Just "bar"))
foo x = fromMaybe "foo" x
