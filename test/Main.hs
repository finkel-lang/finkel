-- | Tests for SK.
module Main where

import Test.Hspec
import SyntaxTest
import BuildTest

main :: IO ()
main = do
  files <- getTestFiles
  hspec (do describe "Syntax" (syntaxTests files)
            describe "Build" buildTests)
