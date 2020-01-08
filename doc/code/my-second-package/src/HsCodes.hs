-- File: my-second-package/src/HsCodes.hs

module HsCodes
  ( hsfactorial
  , skfactorial
  ) where

import SkCodes

hsfactorial :: Int -> Int
hsfactorial = skfactorial
