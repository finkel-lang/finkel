{-# LANGUAGE CPP #-}

-- | Version compatibility module for GHC.Data.FastString
module Language.Finkel.Data.FastString
  ( module FS
  , module Language.Finkel.Data.FastString
  ) where

#include "ghc_modules.h"

-- binary
import Data.Binary         (Binary (..), Get, Put)

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


-- ------------------------------------------------------------------------
-- For Data.Binary.Binary
-- ------------------------------------------------------------------------

#if MIN_VERSION_ghc(9,0,0)
putFastString :: FastString -> Put
putFastString = put . FS.fastStringToShortByteString

getFastString :: Get FastString
getFastString = fmap FS.mkFastStringShortByteString get
#else
putFastString :: FastString -> Put
putFastString = put . FS.bytesFS

getFastString :: Get FastString
getFastString = fmap FS.mkFastStringByteString get
#endif

{-# INLINABLE getFastString #-}
{-# INLINABLE putFastString #-}
