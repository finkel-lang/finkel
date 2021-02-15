{-# LANGUAGE CPP #-}
-- | Wrapper module to export 'cProjectVersion' from @ghc@ package.
module Finkel.Core.Ghc.Version
  ( cProjectVersionInt
  ) where

#if MIN_VERSION_ghc(9,0,0)
import GHC.Settings.Config (cProjectVersionInt)
#else
import Config              (cProjectVersionInt)
#endif
