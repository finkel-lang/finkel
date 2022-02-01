;;; -*- mode: finkel -*-

(module Main
  foo
  main)

(import Data.Maybe (fromMaybe))
(import qualified Control.Monad as M)

(:: main (IO ()))
(= main
  (M.forM- print (Just foo)))

(:: foo Bool)
(= foo (fromMaybe True Nothing))
