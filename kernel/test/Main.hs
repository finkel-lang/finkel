-- | Tests for SK.
module Main where

import Data.List
import System.FilePath
import System.Directory

import Test.Hspec

import EvalTest
import FormTest
import SkcTest
import MakeTest
import SyntaxTest

getTestFiles :: String -> IO [FilePath]
getTestFiles name =
  let dir = "test" </> "data" </> name
      f x acc = if takeExtension x == ".sk"
                  then (dir </> x) : acc
                  else acc
      files = getDirectoryContents dir
  in  sort <$> foldr f [] <$> files

main :: IO ()
main = do
  syntaxFiles <- getTestFiles "syntax"
  evalFiles <- getTestFiles "eval"
  hspec
    (do describe "Form" formTests
        describe "Skc" skcTests
        describe "Eval" (evalTests evalFiles)
        describe "Make" makeTests
        describe "Syntax" (syntaxTests syntaxFiles))
