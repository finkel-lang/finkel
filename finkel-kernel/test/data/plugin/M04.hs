;;; -*- mode: finkel -*-

(module M04)

(import Control.Monad.IO.Class)
(import Language.Finkel)

(:require M04b)

(defmac m04 []
  '(putStrLn "M04.m04"))

