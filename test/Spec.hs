-- | Main entry point of SK tests.
module Main where

import CoreTest
import Test.Hspec

main :: IO ()
main = hspec coreTests
