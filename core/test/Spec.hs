-- | Main entry point of Finkel tests.
module Main where

import CoreTest
import FunctionTest
import GhcTest
import Test.Hspec

main :: IO ()
main = hspec $ do
  functionTests
  coreTests
  ghcTests
