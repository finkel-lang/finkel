{-# LANGUAGE CPP #-}

-- | Version compatibility module for GHC.Data.FastString
module Language.Finkel.Data.FastString
  ( module FS
#if !MIN_VERSION_ghc(9,2,0)
  , module Language.Finkel.Data.FastString
#endif
  ) where

#include "ghc_modules.h"

-- ghc
import GHC_Data_FastString as FS

#if !MIN_VERSION_ghc(9,2,0)
unconsFS :: FastString -> Maybe (Char, FastString)
unconsFS fs =
  case unpackFS fs of
    []       -> Nothing
    (c : cs) -> Just (c, mkFastString cs)
{-# INLINABLE unconsFS #-}
#endif
