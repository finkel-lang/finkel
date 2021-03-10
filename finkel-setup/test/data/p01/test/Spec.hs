module Main where
import System.Exit

import P01.A       (p01a)
import TestAll     (expected)

main :: IO ()
main =
  if p01a == expected
    then exitSuccess
    else exitFailure
