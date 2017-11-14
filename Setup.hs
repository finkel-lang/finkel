module Main where

import Distribution.Simple.SK

main :: IO ()
main = defaultMainWithHooks skkcHooks
