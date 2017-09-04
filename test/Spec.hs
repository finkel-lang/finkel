-- | Main entry point of SK tests.
module Main where

import MacroTest
import Test.Hspec

main :: IO ()
main = hspec macroTests
