{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}

 -- For HasLoc (GenLocated l e)
{-# LANGUAGE MonoLocalBinds   #-}

-- | Module for location in Haskell AST
module Language.Finkel.Syntax.Location
  ( -- * Auxiliary function
    lN, lA, lL
  , mkLocatedList, mkLocatedListA
  , mkLocatedListA'

    -- * Re-export or aliase
  , LocatedN, LIdP
  , getLocA, la2la, reLoc, reLocA, addCLocA, addCLocAA
  , cL, dL

  ) where

#include "ghc_modules.h"

-- ghc

import           GHC_Types_SrcLoc                  (GenLocated (..), Located,
                                                    SrcSpan, combineLocs, noLoc)

#if MIN_VERSION_ghc(9,10,0)
import qualified GHC.Hs.Utils                      (mkLocatedList)
import           GHC.Parser.Annotation             (HasAnnotation (..),
                                                    HasLoc (..), LocatedAn,
                                                    NoAnn (..))
#elif MIN_VERSION_ghc(9,2,0)
import           GHC.Parser.Annotation             (SrcAnn, SrcSpanAnn' (..),
                                                    addCLocAA, combineLocsA,
                                                    noAnn, noAnnSrcSpan, reLocA)
import           GHC.Types.SrcLoc                  (noSrcSpan)
#endif

#if MIN_VERSION_ghc(9,2,0)
import           Language.Haskell.Syntax.Extension (LIdP)
#else
import           GHC.Hs.Extension                  (LIdP)
#endif

#if MIN_VERSION_ghc(9,2,0)
import           GHC.Parser.Annotation             (LocatedA, LocatedL,
                                                    LocatedN, addCLocA, getLocA,
                                                    la2la, reLoc)
#else
import           GHC_Types_SrcLoc                  (addCLoc, getLoc)
#endif

#if MIN_VERSION_ghc(8,8,0) && !MIN_VERSION_ghc(9,0,0)
import           SrcLoc                            (HasSrcSpan, SrcSpanLess)
import qualified SrcLoc
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

#if MIN_VERSION_ghc(9,10,0)
reLocA :: (HasLoc (GenLocated a e), HasAnnotation b)
       => GenLocated a e -> GenLocated b e
reLocA = reLoc
{-# INLINE reLocA #-}

addCLocAA :: (HasLoc a, HasLoc b, HasAnnotation l)
          => a -> b -> c -> GenLocated l c
addCLocAA = addCLocA
{-# INLINE addCLocAA #-}
#endif

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

-- For concrete 'Located' input and output.
mkLocatedList :: [Located a] -> Located [Located a]
mkLocatedList []        = noLoc []
mkLocatedList ms@(hd:_) = L (combineLocs hd (last ms)) ms
{-# INLINABLE mkLocatedList #-}

#if MIN_VERSION_ghc(9,10,0)
mkLocatedListA :: (Semigroup a, NoAnn an)
               => [LocatedAn a e] -> LocatedAn an [LocatedAn a e]
mkLocatedListA = GHC.Hs.Utils.mkLocatedList

-- The expression is same as 'mkLocatedListA', but the type signature of the
-- resulting value has the same annotation as the element of given list.
mkLocatedListA' :: (Semigroup a, NoAnn a)
                => [LocatedAn a e] -> LocatedAn a [LocatedAn a e]
mkLocatedListA' = mkLocatedListA

#elif MIN_VERSION_ghc(9,2,0)
mkLocatedListA
  :: Semigroup a
  => [GenLocated (SrcAnn a) e]
  -> GenLocated (SrcAnn a) [GenLocated (SrcAnn a) e]
mkLocatedListA []        = L (SrcSpanAnn noAnn noSrcSpan) []
mkLocatedListA ms@(hd:_) = L (combineLocsA hd (last ms)) ms

mkLocatedListA'
  :: Semigroup a
  => [GenLocated (SrcAnn a) e]
  -> GenLocated (SrcAnn a) [GenLocated (SrcAnn a) e]
mkLocatedListA' = mkLocatedListA
#else
mkLocatedListA :: [Located a] -> Located [Located a]
mkLocatedListA = mkLocatedList

mkLocatedListA' :: [Located a] -> Located [Located a]
mkLocatedListA' = mkLocatedList
#endif
{-# INLINABLE mkLocatedListA #-}
