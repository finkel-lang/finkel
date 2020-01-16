-- | Tests for Finkel.
module Main where

import Data.List
import System.Directory
import System.FilePath

import Test.Hspec

import EmitTest
import EvalTest
import FnkTest
import FormTest
import MainTest
import MakeTest
import SyntaxTest

getTestFiles :: String -> IO [FilePath]
getTestFiles name =
  let dir = "test" </> "data" </> name
      f x acc = if takeExtension x == ".fnk"
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
        describe "Fnk" fnkTests
        describe "Emit" emitTests
        describe "Eval" (evalTests evalFiles)
        describe "Main" mainTests
        describe "Make" makeTests
        describe "Syntax" (syntaxTests syntaxFiles))
