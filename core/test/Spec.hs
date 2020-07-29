-- | Main entry point of Finkel tests.
module Main where

import CoreTest
import FunctionTest
import Test.Hspec

main :: IO ()
main = hspec $ do
  functionTests
  coreTests
