module Main where

import CLITest
import MainTest
import ReplMacroTest
import ReplTest
import Test.Hspec

main :: IO ()
main = hspec (do cliTests
                 mainTests
                 replTests
                 replMacroTests)
