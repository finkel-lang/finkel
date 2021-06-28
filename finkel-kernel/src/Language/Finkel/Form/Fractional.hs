{-# LANGUAGE CPP #-}
-- | Module for version compatible fractional literal value.
module Language.Finkel.Form.Fractional
  ( FractionalLit(..)
  , mkFractionalLit'
  , fl_text_compat
#if MIN_VERSION_ghc(9,2,0)
  -- XXX: Rename 'fl_value' with 'rationalFromFractionalLit'?
  , fl_value
#endif
  , readFractionalLit
  , putFractionalLit
  , getFractionalLit

  , SourceText(..)
  , putSourceText
  , getSourceText
  ) where

-- binary
import Data.Binary          (Binary (..), Get, Put, getWord8, putWord8)

-- ghc
#if MIN_VERSION_ghc(9,2,0)
import GHC.Types.SourceText (FractionalExponentBase (..), FractionalLit (..),
                             SourceText (..), mkSourceFractionalLit,
                             mkTHFractionalLit, rationalFromFractionalLit)
import GHC.Utils.Misc       (readSignificandExponentPair)
#elif MIN_VERSION_ghc(9,0,0)
import GHC.Types.Basic      (FractionalLit (..), SourceText (..),
                             mkFractionalLit)
import GHC.Utils.Misc       (readRational)
#elif MIN_VERSION_ghc(8,4,0)
import BasicTypes           (FractionalLit (..), SourceText (..),
                             mkFractionalLit)
import Util                 (readRational)
#else
import BasicTypes           (FractionalLit (..), SourceText (..))
import Util                 (readRational)
#endif

-- | Make a 'FractionalLit' from given real value.
mkFractionalLit' :: Real a => a -> FractionalLit
{-# INLINE mkFractionalLit' #-}
#if MIN_VERSION_ghc(9,2,0)
mkFractionalLit' = mkTHFractionalLit . toRational
#elif MIN_VERSION_ghc(8,4,0)
mkFractionalLit' = mkFractionalLit
#else
mkFractionalLit' x = FL (show (realToFrac x :: Double)) (toRational x)
#endif

-- | Get string representation of 'FractionalLit'.
fl_text_compat :: FractionalLit -> String
{-# INLINE fl_text_compat #-}
#if MIN_VERSION_ghc(8,4,0)
fl_text_compat fl = case fl_text fl of
  NoSourceText -> error "fractional literal with no source"
  SourceText s -> s
#else
fl_text_compat = fl_text
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
#elif MIN_VERSION_ghc(8,4,0)
readFractionalLit str = FL stxt is_neg rat
  where
    is_neg = startsWithMinus str
    rat = readRational str
    stxt = SourceText str
#else
readFractionalLit str = FL str rat
  where
    rat = readRational str
#endif
{-# INLINABLE readFractionalLit #-}

#if MIN_VERSION_ghc(8,4,0)
-- | Compare the first character of given string with minis sign.
startsWithMinus :: String -> Bool
startsWithMinus str = case str of
  '-': _ -> True
  _      -> False
{-# INLINE startsWithMinus #-}
#endif

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

#elif MIN_VERSION_ghc(8,4,0)
putFractionalLit fl =
  putSourceText (fl_text fl) *> put (fl_neg fl) *> put (fl_value fl)

getFractionalLit =
  FL <$> getSourceText <*> get <*> get

#else
putFractionalLit fl = put (fl_text fl) *> put (fl_value fl)
getFractionalLit = FL <$> get <*> get
#endif

putSourceText :: SourceText -> Put
putSourceText st = case st of
  SourceText str -> putWord8 0 >> put str
  NoSourceText   -> putWord8 1
{-# INLINABLE putSourceText #-}

getSourceText :: Get SourceText
getSourceText = do
  t <- getWord8
  case t of
    0 -> SourceText <$> get
    1 -> pure NoSourceText
    _ -> error $ "getSourceText: unknown tag " ++ show t
{-# INLINABLE getSourceText #-}
