module Main where

import Data.List (isSubsequenceOf)
import P02.A (message)
import Test.Hspec

main :: IO ()
main = hspec (describe "message"
                       (it "should contain P02.A"
                           (isSubsequenceOf "P02.A" message)))
