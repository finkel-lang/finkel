{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Version compatibility module for 'SourceText'.
module Language.Finkel.Data.SourceText
  ( SourceText
  , IsSourceText(..)
  , toQuotedSourceText
  , fsToSourceText
  , strToSourceText
  ) where

#include "ghc_modules.h"

-- ghc
import GHC_Data_FastString  (FastString)
import GHC_Types_SourceText (SourceText (..))

#if MIN_VERSION_ghc(9,8,0)
import GHC_Data_FastString  (fsLit)
#else
import GHC_Data_FastString  (unpackFS)
#endif


-- ------------------------------------------------------------------------
-- Type class
-- ------------------------------------------------------------------------

class IsSourceText s where
  toSourceText :: s -> SourceText

instance IsSourceText String where
  toSourceText = strToSourceText
  {-# INLINE toSourceText #-}

instance IsSourceText FastString where
  toSourceText = fsToSourceText
  {-# INLINE toSourceText #-}

-- | Make a 'SourceText' quoted with double quotes, uses 'show' function.
toQuotedSourceText :: Show a => a -> SourceText
toQuotedSourceText = toSourceText . show
{-# INLINE toQuotedSourceText #-}


-- ------------------------------------------------------------------------
-- Converting to SourceText
-- ------------------------------------------------------------------------

fsToSourceText :: FastString -> SourceText
#if MIN_VERSION_ghc(9,8,0)
fsToSourceText = SourceText
#else
fsToSourceText = SourceText . unpackFS
#endif
{-# INLINABLE fsToSourceText #-}

strToSourceText :: String -> SourceText
#if MIN_VERSION_ghc(9,8,0)
strToSourceText = SourceText . fsLit
#else
strToSourceText = SourceText
#endif
{-# INLINABLE strToSourceText #-}
