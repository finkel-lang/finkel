-- | Main entry point of Finkel tests.
module Main where

import CoreTest
import Test.Hspec

main :: IO ()
main = hspec coreTests
