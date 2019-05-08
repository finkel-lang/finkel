module Main where

import MainTest
import ReplTest
import SetupTest
import Test.Hspec

main :: IO ()
main = hspec (do mainTest
                 replTests
                 setupTests)
