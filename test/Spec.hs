-- | Main entry point of SK tests.
module Main where

import MacroTest

main :: IO ()
main =
  if all id [f1, f2 21 == 42]
    then putStrLn "Success"
    else error "Test failure"
