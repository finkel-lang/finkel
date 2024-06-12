{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Version compatibility module for 'SourceText'.
module Language.Finkel.Data.SourceText
  ( SourceText
  , IsSourceText(..)
  , toQuotedSourceText
  , fsToSourceText
  , strToSourceText
  , putSourceText
  , getSourceText
  ) where

#include "ghc_modules.h"

-- binary
import Data.Binary                     (Get, Put, getWord8, putWord8)

#if !MIN_VERSION_ghc(9,8,0)
import Data.Binary                     (Binary (..))
#endif

-- ghc
import GHC_Data_FastString             (FastString)
import GHC_Types_SourceText            (SourceText (..))

#if MIN_VERSION_ghc(9,8,0)
import GHC_Data_FastString             (fsLit)
#else
import GHC_Data_FastString             (unpackFS)
#endif

-- Internal
#if MIN_VERSION_ghc(9,8,0)
import Language.Finkel.Data.FastString (getFastString, putFastString)
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
-- For Data.Binary.Binary
-- ------------------------------------------------------------------------

putSourceText :: SourceText -> Put
putSourceText st = case st of
#if MIN_VERSION_ghc(9,8,0)
  SourceText str -> putWord8 0 >> putFastString str
#else
  SourceText str -> putWord8 0 >> put str
#endif
  NoSourceText   -> putWord8 1
{-# INLINABLE putSourceText #-}

getSourceText :: Get SourceText
getSourceText = do
  t <- getWord8
  case t of
#if MIN_VERSION_ghc(9,8,0)
    0 -> SourceText <$> getFastString
#else
    0 -> SourceText <$> get
#endif
    1 -> pure NoSourceText
    _ -> error $ "getSourceText: unknown tag " ++ show t
{-# INLINABLE getSourceText #-}


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
