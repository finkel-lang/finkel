module Main where
import System.Exit
import P01.A

main :: IO ()
main =
  if p01a == ["p01a", "p01b", "p01c", "p01d", "p01e", "p01f"
             ,"p01h", "p01i", "p01j"]
    then exitSuccess
    else exitFailure
