module Main where
import System.Exit
import S1A

main :: IO ()
main = do
  if s1a == 42
    then exitSuccess
    else exitFailure
