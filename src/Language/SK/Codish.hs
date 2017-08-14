{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
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

instance Codish Float where
  toCode = realFracToCode
  fromCode = codeToFractional

instance Codish Double where
  toCode = realFracToCode
  fromCode = codeToFractional

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

instance Codish Bool where
  toCode = showAsSymbolCode
  fromCode a =
    case unLocLForm a of
      Atom (ASymbol sym) | sym == "True"  -> Just True
                         | sym == "False" -> Just False
      _                                   -> Nothing

instance Codish Ordering where
  toCode = showAsSymbolCode
  fromCode a =
    case unLocLForm a of
      Atom (ASymbol sym) | sym == "EQ" -> Just EQ
                         | sym == "LT" -> Just LT
                         | sym == "GT" -> Just GT
      _                                -> Nothing

instance Codish a => Codish (Maybe a) where
  toCode a =
    case a of
      Nothing -> toCode (aSymbol "Nothing")
      Just x  -> toCode (List [toCode (aSymbol "Just"), toCode x])
  fromCode a =
    case unLocLForm a of
      Atom (ASymbol "Nothing") -> Just Nothing
      List [LForm (L _ (Atom (ASymbol "Just"))), x] -> Just (fromCode x)
      _                                             -> Nothing

instance (Codish a, Codish b) => Codish (Either a b) where
  toCode a =
    case a of
      Right x -> toCode (List [symbolCode "Right", toCode x])
      Left x  -> toCode (List [symbolCode "Left", toCode x])
  fromCode a =
    case unLocLForm a of
      List [LForm (L _ (Atom (ASymbol x))), y]
        | x == "Right" -> fmap Right (fromCode y)
        | x == "Left"  -> fmap Left (fromCode y)
      _                -> Nothing

instance (Codish a, Codish b) => Codish (a, b) where
  toCode (a1, a2) =
    toCode (List [symbolCode ",", toCode a1, toCode a2])
  fromCode a =
    case unLocLForm a of
      List [LForm (L _ (Atom (ASymbol ","))), a1, a2]
        -> (\b1 b2 -> (b1, b2)) <$>
           fromCode a1 <*> fromCode a2
      _ -> Nothing

instance (Codish a, Codish b, Codish c) => Codish (a, b, c) where
  toCode (a1, a2, a3) =
    toCode (List [symbolCode ",", toCode a1, toCode a2, toCode a3])
  fromCode a =
    case unLocLForm a of
      List [LForm (L _ (Atom (ASymbol ","))), a1, a2, a3]
        -> (\b1 b2 b3 -> (b1, b2, b3)) <$>
           fromCode a1 <*> fromCode a2 <*> fromCode a3
      _ -> Nothing

instance (Codish a, Codish b, Codish c, Codish d)
         => Codish (a, b, c, d) where
  toCode (a1, a2, a3, a4) =
    toCode (List [ symbolCode ",", toCode a1, toCode a2, toCode a3
                 , toCode a4])
  fromCode a =
    case unLocLForm a of
      List [LForm (L _ (Atom (ASymbol ","))), a1, a2, a3, a4]
        -> (\b1 b2 b3 b4 -> (b1, b2, b3, b4)) <$>
           fromCode a1 <*> fromCode a2 <*> fromCode a3 <*> fromCode a4
      _ -> Nothing

instance (Codish a, Codish b, Codish c, Codish d, Codish e)
         => Codish (a, b, c, d, e) where
  toCode (a1, a2, a3, a4, a5) =
    toCode (List [ symbolCode ",", toCode a1, toCode a2, toCode a3
                 , toCode a4, toCode a5])
  fromCode a =
    case unLocLForm a of
      List [LForm (L _ (Atom (ASymbol ","))), a1, a2, a3, a4, a5]
        -> (\b1 b2 b3 b4 b5 -> (b1, b2, b3, b4, b5)) <$>
           fromCode a1 <*> fromCode a2 <*> fromCode a3 <*>
           fromCode a4 <*> fromCode a5
      _ -> Nothing

instance (Codish a, Codish b, Codish c, Codish d, Codish e, Codish f)
         => Codish (a, b, c, d, e, f) where
  toCode (a1, a2, a3, a4, a5, a6) =
    toCode (List [ symbolCode ",", toCode a1, toCode a2, toCode a3
                 , toCode a4, toCode a5, toCode a6])
  fromCode a =
    case unLocLForm a of
      List [LForm (L _ (Atom (ASymbol ","))), a1, a2, a3, a4, a5, a6]
        -> (\b1 b2 b3 b4 b5 b6 -> (b1, b2, b3, b4, b5, b6)) <$>
           fromCode a1 <*> fromCode a2 <*> fromCode a3 <*>
           fromCode a4 <*> fromCode a5 <*> fromCode a6
      _ -> Nothing


-- -------------------------------------------------------------------
--
-- Auxiliary
--
-- -------------------------------------------------------------------

realFracToCode :: (Real a, Show a) => a -> Code
realFracToCode a =
  LForm (genSrc (Atom (AFractional (FL (show a) (toRational a)))))

codeToFractional :: Fractional a => Code -> Maybe a
codeToFractional a =
  case unLocLForm a of
    Atom (AFractional x) -> Just (fromRational (fl_value x))
    _                    -> Nothing

symbolCode :: String -> Code
symbolCode = LForm . genSrc . Atom . aSymbol

showAsSymbolCode :: Show a => a -> Code
showAsSymbolCode = symbolCode . show
