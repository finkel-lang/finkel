{-# LANGUAGE FlexibleInstances #-}
-- | Module containing 'Codish' typeclass and its instances
-- declarations.
module Language.SK.Codish
  ( Codish(..)
  ) where

import Language.SK.GHC
import Language.SK.Form


-- -------------------------------------------------------------------
--
-- Codish type class
--
-- -------------------------------------------------------------------

--- Instance data types of Formable class could be inserted to
--- S-expression with `unquote' and `unquote-splice'.

class Codish a where
  toCode :: a -> Code

  fromCode :: Code -> Maybe a
  fromCode _ = Nothing

  listToCode :: [a] -> Code
  listToCode xs =
     let xs' = map toCode xs
         l = getLoc (mkLocatedForm xs')
     in  LForm (L l (HsList xs'))

  listFromCode :: Code -> Maybe [a]
  listFromCode xs = case unLocLForm xs of
                      HsList as -> mapM fromCode as
                      _         -> Nothing

instance Codish () where
  toCode _ = LForm (genSrc (Atom AUnit))
  fromCode a =
    case unLocLForm a of
      Atom AUnit -> Just ()
      _          -> Nothing

instance Codish Char where
  toCode = LForm . genSrc . Atom . AChar
  fromCode a =
    case unLocLForm a of
      Atom (AChar x)  -> Just x
      _               -> Nothing
  listToCode = LForm . genSrc . Atom . AString
  listFromCode a = case unLocLForm a of
                     Atom (AString s) -> Just s
                     _                -> Nothing

instance Codish Int where
  toCode = LForm . genSrc . Atom . AInteger . fromIntegral
  fromCode a =
    case unLocLForm a of
      Atom (AInteger n) -> Just (fromIntegral n)
      _                 -> Nothing

instance Codish Integer where
  toCode = LForm . genSrc . Atom . AInteger
  fromCode a =
    case unLocLForm a of
      Atom (AInteger n) -> Just n
      _                 -> Nothing

instance Codish Double where
  toCode a =
    let r = toRational a
    in  LForm (genSrc (Atom (AFractional (FL (show a) r))))
  fromCode a =
    case unLocLForm a of
      Atom (AFractional x) -> Just (fromRational (fl_value x))
      _                    -> Nothing

instance Codish a => Codish [a] where
  toCode = listToCode
  fromCode = listFromCode

instance Codish Atom where
  toCode = LForm . genSrc . Atom
  fromCode a =
    case unLocLForm a of
      Atom x -> Just x
      _      -> Nothing

instance Codish (Form Atom) where
  toCode = LForm . genSrc
  fromCode = Just . unLocLForm

instance Codish (LForm Atom) where
  toCode = id
  fromCode = Just
