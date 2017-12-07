-- | Main entry point of SK tests.
module Main where

import CoreTest
import ReplTest
import Test.Hspec

main :: IO ()
main =
  hspec (do coreTests
            replTests)
