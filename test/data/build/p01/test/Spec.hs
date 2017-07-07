module Main where
import System.Exit
import P01A

main :: IO ()
main =
  if p01a == ["p01a", "p01b", "p01c", "p01d", "p01e", "p01f"]
    then exitSuccess
    else exitFailure
