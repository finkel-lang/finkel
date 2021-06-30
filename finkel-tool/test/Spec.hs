module Main where

-- Internal
import CLITest
import GhcTest
import MainTest
import ReplMacroTest
import ReplTest
import TestAux

-- hspec
import Test.Hspec

main :: IO ()
main = do
  etf <- makeEvalTestFns
  hspec $
    do afterAll_ (etf_cleanup etf)
                 (do describe "CLITest" cliTests
                     describe "GhcTest" ghcTests
                     describe "MainTest" mainTests
                     describe "ReplTest" (replTests etf)
                     describe "ReplMacroTest" (replMacroTests etf))
       listenTests etf
