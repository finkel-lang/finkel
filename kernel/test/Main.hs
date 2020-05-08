-- | Tests for Finkel.
module Main where

import Data.List
import System.Directory
import System.FilePath

import Test.Hspec

import Language.Finkel.Fnk (initUniqSupply')

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
    (beforeAll_
       -- Initializing UniqSupply before all tests, so that the tests not using
       -- 'Language.Finkel.Main.defaultMain' can use UniqSupply, and to avoid
       -- initializing the UniqSupply multiple times.
       (initUniqSupply' 0 1)
       (do describe "Form" formTests
           describe "Fnk" fnkTests
           describe "Emit" emitTests
           describe "Eval" (evalTests evalFiles)
           describe "Main" mainTests
           describe "Make" makeTests
           describe "Syntax" (syntaxTests syntaxFiles)))
