{-# OPTIONS_GHC -fno-warn-orphans #-}
-- Orphan instance definitions for Form, for QuickCheck.

module Orphan where

-- ghc
import BasicTypes           (FractionalLit (..))
import FastString           (unpackFS)
import SrcLoc               (GenLocated (..))

-- QuickCheck
import Test.QuickCheck      (Arbitrary (..), CoArbitrary (..), Gen,
                             arbitraryUnicodeChar, elements,
                             getUnicodeString, listOf, oneof, scale,
                             variant)

-- Internal
import Language.Finkel.Form

instance Arbitrary Atom where
   -- XXX: Unicode symbols are not generated.
  arbitrary =
    oneof [ return AUnit
          , aSymbol <$> symbolG
          , AChar <$> arbitraryUnicodeChar
          , aString <$> stringG
          , AInteger <$> arbitrary
          , aFractional <$> (arbitrary :: Gen Double) ]
    where
      headChars = ['A' .. 'Z'] ++ ['a' .. 'z'] ++ "_!$%&*+./<=>?@^~:"
      tailChars = headChars ++ "0123456789'_"
      symbolG = do
        x <- elements headChars
        xs <- listOf (elements tailChars)
        return (x:xs)
      stringG = getUnicodeString <$> arbitrary

instance CoArbitrary Atom where
  coarbitrary x =
    case x of
      AUnit         -> var 0
      ASymbol sym   -> var 1 . coarbitrary (unpackFS sym)
      AChar c       -> var 2 . coarbitrary c
      AString str   -> var 3 . coarbitrary (unpackFS str)
      AInteger i    -> var 4 . coarbitrary i
      AFractional d -> var 5 . coarbitrary (fl_value d)
    where
      var :: Int -> Gen a -> Gen a
      var = variant

instance Arbitrary a => Arbitrary (Form a) where
  arbitrary =
    oneof [Atom <$> arbitrary
          ,List <$> listOf (scale (`div` 3) arbitrary)
          ,HsList <$> listOf (scale (`div` 3) arbitrary)]
  shrink x =
    case x of
      Atom _    -> []
      List xs   -> map unCode xs ++ [List xs'|xs' <- shrink xs]
      HsList xs -> map unCode xs ++ [HsList xs'|xs' <- shrink xs]
      TEnd      -> []

instance CoArbitrary a => CoArbitrary (Form a) where
  coarbitrary x =
    case x of
      Atom y    -> var 0 . coarbitrary y
      List ys   -> var 1 . coarbitrary ys
      HsList ys -> var 2 . coarbitrary ys
      TEnd      -> var 3
    where
      var :: Int -> Gen a -> Gen a
      var = variant

instance Arbitrary a => Arbitrary (LForm a) where
  arbitrary = (LForm . genSrc) <$> arbitrary

instance CoArbitrary a => CoArbitrary (LForm a) where
  coarbitrary (LForm (L _ form)) = coarbitrary form
