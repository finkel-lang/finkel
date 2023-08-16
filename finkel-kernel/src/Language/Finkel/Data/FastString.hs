{-# LANGUAGE CPP #-}

#include "ghc_modules.h"

-- Version compatibility module for GHC.Data.FastString
module Language.Finkel.Data.FastString
  ( module FS
#if !MIN_VERSION_ghc(9,2,0)
  , module Language.Finkel.Data.FastString
#endif
  ) where

import GHC_Data_FastString as FS

-- #if MIN_VERSION_ghc(9,0,0) && !MIN_VERSION_ghc(9,2,0)
-- import qualified Data.ByteString.Char8 as C8
-- #endif

-- #if !MIN_VERSION_ghc(9,2,0)
-- unconsFS :: FastString -> Maybe (Char, FastString)
-- #  if MIN_VERSION_ghc(9,0,0)
-- unconsFS fs =
--   if nullFS fs then
--     Nothing
--   else
--     Just (headFS fs, mkFastStringByteString (C8.tail (bytesFS fs)))
-- #  else
-- unconsFS fs = if nullFS fs then Nothing else Just (headFS fs, tailFS fs)
-- #  endif
-- {-# INLINABLE unconsFS #-}
-- #endif

#if !MIN_VERSION_ghc(9,2,0)
unconsFS :: FastString -> Maybe (Char, FastString)
unconsFS fs =
  case unpackFS fs of
    []       -> Nothing
    (c : cs) -> Just (c, mkFastString cs)
{-# INLINABLE unconsFS #-}
#endif
