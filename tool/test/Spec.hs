module Main where

import CLITest
import CompatTest
import MainTest
import ReplTest
import ReplMacroTest
import SetupTest
import Test.Hspec

main :: IO ()
main = hspec (do cliTests
                 compatTests
                 mainTests
                 replTests
                 replMacroTests
                 setupTests)
