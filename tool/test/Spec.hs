module Main where

import ReplTest
import SetupTest
import Test.Hspec

main :: IO ()
main = hspec (do replTests
                 setupTests)
