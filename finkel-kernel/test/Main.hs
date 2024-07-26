-- | Tests for Finkel.
module Main where

-- base
import System.Environment         (getArgs)

-- hspec
import Test.Hspec                 (beforeAll, beforeAll_, describe, hspec)

-- finkel-kernel
import Language.Finkel.Fnk        (initUniqSupply')
import Language.Finkel.Preprocess (defaultPreprocess)

-- Internal
import EmitTest
import EvalTest
import ExceptionTest
import FnkTest
import FormTest
import MainTest
import MakeTest
import PluginTest
import PreprocessTest
import SyntaxTest
import TestAux

-- To support plugin tests, the test executable is acting as a preprocessor if
-- specific three file paths were given. Otherwise, run the hspec tests.
main :: IO ()
main = do
  args <- getArgs
  case args of
    (orig:isrc:_opath:_rest) | orig == isrc -> defaultPreprocess
    _                                       -> doHspec

doHspec :: IO ()
doHspec =
  hspec
    (beforeAll_
       -- Initializing UniqSupply before all tests, so that the tests not using
       -- 'Language.Finkel.Main.defaultMain' can use UniqSupply, and to avoid
       -- initializing the UniqSupply multiple times.
       (initUniqSupply' 0 1)
       (do describe "Form" formTests
           describe "Fnk" fnkTests
           describe "Emit" emitTests
           describe "Preprocess" preprocessTests
           beforeAll getFnkTestResource $ do
             describe "Eval" evalFnkTests
             describe "Main" mainFnkTests
             describe "Make" makeFnkTests
             describe "Plugin" pluginTests
             describe "Syntax" syntaxFnkTests
             describe "Exception" exceptionFnkTests))
