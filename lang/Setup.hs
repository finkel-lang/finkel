module Main where

import Distribution.Simple.SK

main :: IO ()
main = defaultMainWithHooks (skcHooksWith "skkc2" False)
