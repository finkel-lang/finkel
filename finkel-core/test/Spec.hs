;;; -*- mode: finkel -*-

;;; Main entry point of Finkel tests.
(module Main)

;;; hspec
(import Test.Hspec)

;;; Internal
(import CoreTest)
(import FunctionTest)
(import PluginTest)

(:: main (IO ()))
(= main (hspec (do functionTests
                   coreTests
                   pluginTests)))
