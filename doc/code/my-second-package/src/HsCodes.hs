-- File: my-second-package/src/HsCodes.hs

module HsCodes
  ( hsfactorial
  , fnkfactorial
  ) where

import FnkCodes

hsfactorial :: Int -> Int
hsfactorial = fnkfactorial
