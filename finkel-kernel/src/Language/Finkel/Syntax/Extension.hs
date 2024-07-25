{-# LANGUAGE CPP #-}
-- | Modul for extenson field in Haskell AST.
--
-- This module condains type class and instances for extension field in Haskell
-- AST.  It seemed better to create dedicated specific class for ignored
-- extension field.
--
module Language.Finkel.Syntax.Extension
  ( Unused(..)
  ) where

#if MIN_VERSION_ghc(9,10,0)
import GHC.Hs.Binds                      (AnnSig (..), NamespaceSpecifier (..))
import GHC.Hs.Expr                       (AnnsIf, EpAnnHsCase (..))
import GHC.Parser.Annotation             (AddEpAnn (..), AnnList (..),
                                          AnnParen (..), EpLayout (..),
                                          EpToken (..), EpUniToken (..),
                                          NoAnn (..))
#elif MIN_VERSION_ghc(9,2,0)
import GHC.Parser.Annotation             (SrcSpanAnn' (..))
import GHC.Types.SrcLoc                  (noSrcSpan)
#endif

#if MIN_VERSION_ghc(9,2,0)
import GHC.Parser.Annotation             (AnnSortKey (..), EpAnn (..),
                                          EpAnnComments (..))
import Language.Haskell.Syntax.Extension (NoExtField (..), noExtField)
#else
import GHC.Hs.Extension                  (NoExtField (..), noExtField)
#endif

-- | Type class to represent field value which is not in use.
class Unused a where
  -- | Unused value for extension field in AST data types.
  unused :: a

#if MIN_VERSION_ghc(9,10,0)
instance Unused (AnnSortKey tag) where
  unused = NoAnnSortKey
  {-# INLINE unused #-}

instance Unused NamespaceSpecifier where
  unused = NoNamespaceSpecifier
  {-# INLINE unused #-}

instance Unused EpLayout where
  unused = EpNoLayout
  {-# INLINE unused #-}

instance NoAnn a => Unused (EpAnn a) where
  unused = noAnn
  {-# INLINE unused #-}

instance Unused AddEpAnn where
  unused = noAnn
  {-# INLINE unused #-}

instance Unused AnnList where
  unused = noAnn
  {-# INLINE unused #-}

instance Unused AnnParen where
  unused = noAnn
  {-# INLINE unused #-}

instance Unused AnnSig where
  unused = noAnn
  {-# INLINE unused #-}

instance Unused (EpToken s) where
  unused = noAnn
  {-# INLINE unused #-}

instance Unused (EpUniToken s t) where
  unused = noAnn
  {-# INLINE unused #-}

instance Unused EpAnnHsCase where
  unused = noAnn
  {-# INLINE unused #-}

instance Unused AnnsIf where
  unused = noAnn
  {-# INLINE unused #-}

#elif MIN_VERSION_ghc(9,2,0)
instance Unused (EpAnn a) where
  unused = EpAnnNotUsed
  {-# INLINE unused #-}

instance Unused a => Unused (SrcSpanAnn' a) where
  unused = SrcSpanAnn {ann = unused, locA = noSrcSpan}
  {-# INLINE unused #-}

instance Unused AnnSortKey where
  unused = NoAnnSortKey
  {-# INLINE unused #-}
#endif

#if MIN_VERSION_ghc(9,2,0)
instance Unused EpAnnComments where
  unused = EpaComments {priorComments = []}
  {-# INLINE unused #-}

instance Unused a => Unused [a] where
  unused = []
  {-# INLINE unused #-}
#endif

instance Unused NoExtField where
  unused = noExtField
  {-# INLINE unused #-}
