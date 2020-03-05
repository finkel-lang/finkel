module Main where

import CLITest
import CompatTest
import MainTest
import ReplMacroTest
import ReplTest
import Test.Hspec

main :: IO ()
main = hspec (do cliTests
                 compatTests
                 mainTests
                 replTests
                 replMacroTests)
