-- | Tests for Finkel.
module Main where

-- hspec
import Test.Hspec          (beforeAll, beforeAll_, describe, hspec)

-- finkel-kernel
import Language.Finkel.Fnk (initUniqSupply')

-- Internal
import EmitTest
import EvalTest
import ExceptionTest
import FnkTest
import FormTest
import MainTest
import MakeTest
import SyntaxTest
import TestAux

main :: IO ()
main =
  hspec
    (beforeAll_
       -- Initializing UniqSupply before all tests, so that the tests not using
       -- 'Language.Finkel.Main.defaultMain' can use UniqSupply, and to avoid
       -- initializing the UniqSupply multiple times.
       (initUniqSupply' 0 1)
       (do describe "Form" formTests
           describe "Fnk" fnkTests
           describe "Emit" emitTests
           beforeAll getFnkTestResource $ do
             describe "Eval" evalFnkTests
             describe "Main" mainFnkTests
             describe "Make" makeFnkTests
             describe "Syntax" syntaxFnkTests
             describe "Exception" exceptionFnkTests))
