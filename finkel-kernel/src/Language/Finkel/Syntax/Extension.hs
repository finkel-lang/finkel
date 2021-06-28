{-# LANGUAGE CPP #-}
-- | Modul for extenson field in Haskell AST.
--
-- This module condains type class and instances for extension field in Haskell
-- AST.  It seemed better to create dedicated specific class for ignored
-- extension field.
--
-- Still using CPP, because in ghc < 8.4, extension fields did not exist. The
-- CPP macro @NOEXT@ defined in @Syntax.h@ is expanded as "{- noext -}" block
-- comment in such cases.

module Language.Finkel.Syntax.Extension
  ( Unused(..)
  ) where

#if MIN_VERSION_ghc(9,2,0)
import GHC.Parser.Annotation             (AnnSortKey (..), EpAnn (..),
                                          SrcSpanAnn' (..))
import GHC.Types.SrcLoc                  (noSrcSpan)
import Language.Haskell.Syntax.Extension (NoExtField (..), noExtField)
#elif MIN_VERSION_ghc(8,10,0)
import GHC.Hs.Extension                  (NoExtField (..), noExtField)
#elif MIN_VERSION_ghc(8,6,0)
import HsExtension                       (NoExt (..), noExt)
#endif

-- | Type class to represent field value which is no in use.
class Unused a where
  -- | Unused value for extension field in AST data types.
  unused :: a

#if MIN_VERSION_ghc(9,2,0)
instance Unused (EpAnn a) where
  unused = EpAnnNotUsed
  {-# INLINE unused #-}

instance Unused a => Unused (SrcSpanAnn' a) where
  unused = SrcSpanAnn {ann = unused, locA = noSrcSpan}
  {-# INLINE unused #-}

instance Unused a => Unused [a] where
  unused = []
  {-# INLINE unused #-}

instance Unused AnnSortKey where
  unused = NoAnnSortKey
  {-# INLINE unused #-}
#endif

#if MIN_VERSION_ghc(8,10,0)
instance Unused NoExtField where
  unused = noExtField
  {-# INLINE unused #-}
#elif MIN_VERSION_ghc(8,6,0)
instance Unused NoExt where
  unused = noExt
  {-# INLINE unused #-}
#endif
