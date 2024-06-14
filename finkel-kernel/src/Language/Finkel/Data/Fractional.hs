{-# LANGUAGE CPP #-}
-- | Module for version compatible fractional literal value.
module Language.Finkel.Data.Fractional
  ( FractionalLit(..)
  , mkFractionalLit'
  , showFractionalList
#if MIN_VERSION_ghc(9,2,0)
  -- XXX: Rename 'fl_value' with 'rationalFromFractionalLit'?
  , fl_value
#endif
  , readFractionalLit
  , putFractionalLit
  , getFractionalLit
  ) where

-- binary
import Data.Binary                     (Binary (..), Get, Put)

#if MIN_VERSION_ghc(9,2,0)
import Data.Binary                     (getWord8, putWord8)
#endif

-- ghc
#if MIN_VERSION_ghc(9,8,0)
import GHC.Data.FastString             (unpackFS)
#endif

#if MIN_VERSION_ghc(9,2,0)
import GHC.Types.SourceText            (FractionalExponentBase (..),
                                        FractionalLit (..), SourceText (..),
                                        mkSourceFractionalLit,
                                        mkTHFractionalLit,
                                        rationalFromFractionalLit)
import GHC.Utils.Misc                  (readSignificandExponentPair)
#elif MIN_VERSION_ghc(9,0,0)
import GHC.Types.Basic                 (FractionalLit (..), SourceText (..),
                                        mkFractionalLit)
import GHC.Utils.Misc                  (readRational)
#else
import BasicTypes                      (FractionalLit (..), SourceText (..),
                                        mkFractionalLit)
import Util                            (readRational)
#endif

-- Internal
import Language.Finkel.Data.SourceText

-- | Make a 'FractionalLit' from given real value.
mkFractionalLit' :: Real a => a -> FractionalLit
{-# INLINE mkFractionalLit' #-}
#if MIN_VERSION_ghc(9,2,0)
mkFractionalLit' = mkTHFractionalLit . toRational
#else
mkFractionalLit' = mkFractionalLit
#endif

-- | Get string representation of 'FractionalLit'.
showFractionalList :: FractionalLit -> String
{-# INLINE showFractionalList #-}
#if MIN_VERSION_ghc(9,8,0)
showFractionalList fl = case fl_text fl of
  NoSourceText -> error "fractional literal with no source"
  SourceText s -> unpackFS s
#else
showFractionalList fl = case fl_text fl of
  NoSourceText -> error "fractional literal with no source"
  SourceText s -> s
#endif

#if MIN_VERSION_ghc(9,2,0)
-- | Get rational value from 'FractionalLit'.
fl_value :: FractionalLit -> Rational
fl_value = rationalFromFractionalLit
{-# INLINE fl_value #-}
#endif

-- | Read a given string as base 10 'FractionalLit'.
readFractionalLit :: String -> FractionalLit
#if MIN_VERSION_ghc(9,2,0)
readFractionalLit str = mkSourceFractionalLit str is_neg i e b
  where
    is_neg = startsWithMinus str
    (i, e) = readSignificandExponentPair str
    b = Base10
#else
readFractionalLit str = FL stxt is_neg rat
  where
    is_neg = startsWithMinus str
    rat = readRational str
    stxt = SourceText str
#endif
{-# INLINABLE readFractionalLit #-}

-- | Compare the first character of given string with minis sign.
startsWithMinus :: String -> Bool
startsWithMinus str = case str of
  '-': _ -> True
  _      -> False
{-# INLINE startsWithMinus #-}

putFractionalLit :: FractionalLit -> Put
getFractionalLit :: Get FractionalLit
{-# INLINABLE putFractionalLit #-}
{-# INLINABLE getFractionalLit #-}

#if MIN_VERSION_ghc(9,2,0)
putFractionalLit fl =
  putSourceText (fl_text fl) *> put (fl_neg fl) *> put (fl_signi fl) *>
  put (fl_exp fl) *> putFEB (fl_exp_base fl)

getFractionalLit =
  FL <$> getSourceText <*> get <*> get <*> get <*> getFEB

putFEB :: FractionalExponentBase -> Put
putFEB base = case base of
  Base2  -> putWord8 0
  Base10 -> putWord8 1
{-# INLINE putFEB #-}

getFEB :: Get FractionalExponentBase
getFEB = do
  t <- getWord8
  case t of
    0 -> pure Base2
    1 -> pure Base10
    _ -> error ("get (FractionalExponentBase): unknown tag " ++ show t)
{-# INLINE getFEB #-}

#else
putFractionalLit fl =
  putSourceText (fl_text fl) *> put (fl_neg fl) *> put (fl_value fl)

getFractionalLit =
  FL <$> getSourceText <*> get <*> get
#endif

