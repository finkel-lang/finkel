-- | Tests for SK.
module Main where

import Test.Hspec

import FormTest
import SyntaxTest
import MakeTest

main :: IO ()
main = do
  files <- getTestFiles
  hspec (do describe "Form" formTests
            describe "Syntax" (syntaxTests files)
            describe "Make" makeTests)
