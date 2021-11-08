{-# LANGUAGE CPP #-}
-- | Module for location in Haskell AST
module Language.Finkel.Syntax.Location
  ( -- * Auxiliary function
    lN, lA, lL
  , mkLocatedList, mkLocatedListA

    -- * Re-export or aliase
  , LocatedN, LIdP
  , getLocA, la2la, reLoc, reLocA, addCLocA, addCLocAA
  , cL, dL

  ) where

-- ghc
#if MIN_VERSION_ghc(9,2,0)
import           Language.Haskell.Syntax.Extension (LIdP)
#elif MIN_VERSION_ghc(8,10,0)
import           GHC.Hs.Extension                  (LIdP)
#elif MIN_VERSION_ghc(8,6,0)
import           HsExtension                       (LIdP)
#elif MIN_VERSION_ghc(8,4,0)
import           HsExtension                       (IdP)
#endif

#if MIN_VERSION_ghc(9,2,0)
import           GHC.Parser.Annotation             (LocatedA, LocatedL,
                                                    LocatedN, SrcSpanAnn' (..),
                                                    SrcAnn, addCLocA, addCLocAA,
                                                    combineLocsA, getLocA,
                                                    la2la, noAnnSrcSpan, reLoc,
                                                    reLocA)
import           GHC.Types.SrcLoc                  (noSrcSpan)
#elif MIN_VERSION_ghc(9,0,0)
import           GHC.Types.SrcLoc                  (addCLoc, getLoc)
#else
import           SrcLoc                            (addCLoc, getLoc)
#endif

#if MIN_VERSION_ghc(9,0,0)
import           GHC.Types.SrcLoc                  (GenLocated (..), Located,
                                                    SrcSpan, combineLocs, noLoc)
#else
import           SrcLoc                            (GenLocated (..), Located,
                                                    SrcSpan, combineLocs, noLoc)
#endif

#if MIN_VERSION_ghc(8,8,0) && !MIN_VERSION_ghc(9,0,0)
import           SrcLoc                            (HasSrcSpan, SrcSpanLess)
import qualified SrcLoc
#endif

-- Internal
#if MIN_VERSION_ghc(9,2,0)
import           Language.Finkel.Syntax.Extension
#endif

-- Note [Location helper functions]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- From 9.2.x, ghc has shifted to use EPA (Exact Print Annotation), and quite a
-- lot of data types with location information has changed to use dedicated type
-- synonyms for located elements, such as 'LocatedA', 'LocatedN', 'LocatedL',
-- etc. Those new type synonyms are defined in 'GHC.Parser.Annotation' module in
-- ghc 9.2.x source.

#if !MIN_VERSION_ghc(9,2,0)
type LocatedN a = Located a
#endif

#if MIN_VERSION_ghc(9,2,0)
lN :: SrcSpan -> a -> LocatedN a
lN l = L (noAnnSrcSpan l)

lA :: SrcSpan -> a -> LocatedA a
lA l = L (noAnnSrcSpan l)

lL :: SrcSpan -> a -> LocatedL a
lL l = L (noAnnSrcSpan l)
#elif MIN_VERSION_ghc(8,8,0) && !MIN_VERSION_ghc(9,0,0)
lN :: HasSrcSpan a => SrcSpan -> SrcSpanLess a -> a
lN = cL

lA :: HasSrcSpan a => SrcSpan -> SrcSpanLess a -> a
lA = cL

lL :: HasSrcSpan a => SrcSpan -> SrcSpanLess a -> a
lL = cL
#else
lN :: SrcSpan -> a -> Located a
lN = L

lA :: SrcSpan -> a -> Located a
lA = L

lL :: SrcSpan -> a -> Located a
lL = L
#endif
{-# INLINABLE lN #-}
{-# INLINABLE lA #-}
{-# INLINABLE lL #-}

#if !MIN_VERSION_ghc(9,2,0)
getLocA :: Located a -> SrcSpan
getLocA = getLoc
{-# INLINE getLocA #-}

reLoc :: a -> a
reLoc = id
{-# INLINE reLoc #-}

reLocA :: a -> a
reLocA = id
{-# INLINE reLocA #-}

addCLocA :: Located a -> Located b -> c -> Located c
addCLocA = addCLoc
{-# INLINE addCLocA #-}

addCLocAA :: Located a -> Located b -> c -> Located c
addCLocAA = addCLoc
{-# INLINE addCLocAA #-}

la2la :: a -> a
la2la = id
{-# INLINE la2la #-}
#endif

-- For 8.8.x and 8.10.x compatibility in source code location management

#if MIN_VERSION_ghc(8,8,0) && !MIN_VERSION_ghc(9,0,0)
dL :: HasSrcSpan a => a -> Located (SrcSpanLess a)
dL = SrcLoc.dL

cL :: HasSrcSpan a => SrcSpan -> SrcSpanLess a -> a
cL = SrcLoc.cL
#else
dL :: a -> a
dL = id

cL :: s -> a -> GenLocated s a
cL = L
#endif

{-# INLINABLE cL #-}
{-# INLINABLE dL #-}

#if !MIN_VERSION_ghc(8,6,0)
#if MIN_VERSION_ghc(8,4,0)
type LIdP a = Located (IdP a)
#else
type LIdP a = Located a
#endif
#endif

-- Function defined in 'HsUtils', not exported.
mkLocatedList ::  [Located a] -> Located [Located a]
mkLocatedList [] = noLoc []
mkLocatedList ms = L (combineLocs (head ms) (last ms)) ms
{-# INLINABLE mkLocatedList #-}

#if MIN_VERSION_ghc(9,2,0)
mkLocatedListA
  :: Semigroup a
  => [GenLocated (SrcAnn a) e]
  -> GenLocated (SrcAnn a) [GenLocated (SrcAnn a) e]
mkLocatedListA [] = L (SrcSpanAnn unused noSrcSpan) []
mkLocatedListA ms = L (combineLocsA (head ms) (last ms)) ms
#else
mkLocatedListA :: [Located a] -> Located [Located a]
mkLocatedListA = mkLocatedList
#endif
{-# INLINABLE mkLocatedListA #-}
