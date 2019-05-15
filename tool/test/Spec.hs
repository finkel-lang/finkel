module Main where

import CLITest
import MainTest
import ReplTest
import ReplMacroTest
import SetupTest
import Test.Hspec

main :: IO ()
main = hspec (do cliTests
                 mainTests
                 replTests
                 replMacroTests
                 setupTests)
