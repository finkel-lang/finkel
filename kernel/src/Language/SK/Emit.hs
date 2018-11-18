{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
#if MIN_VERSION_ghc(8,4,0)
{-# LANGUAGE UndecidableInstances #-}
#endif
-- | Emit Haskell source code from Haskell syntax data type.
module Language.SK.Emit
  ( HsSrc(..)
  , Hsrc(..)
  , genHsSrc
  ) where

-- base
#if MIN_VERSION_base(4,11,0)
import Prelude hiding ((<>))
#endif

-- ghc
import GHC (OutputableBndrId, getPrintUnqual)
import GhcMonad (GhcMonad(..), getSessionDynFlags)
import HsDecls (DocDecl(..), HsDecl(..), TyClDecl(..))
import HsDoc (LHsDocString)
import HsSyn (HsModule(..))
import Outputable ( (<+>), (<>), Outputable(..), SDoc
                  , comma, empty, fsep, lparen, nest, punctuate
                  , showSDocForUser, text, vcat )
import RdrName (RdrName)
import SrcLoc ( GenLocated(..), unLoc )

#if MIN_VERSION_ghc(8,6,0)
import HsDoc (HsDocString, unpackHDS)
import HsExtension (GhcPass)
#elif MIN_VERSION_ghc(8,4,0)
import HsDoc (HsDocString(..))
import HsExtension (SourceTextX)
import FastString (unpackFS)
#else
import HsDoc (HsDocString(..))
import OccName (HasOccName(..))
import FastString (unpackFS)
#endif

-- Internal
import Language.SK.Lexer

#include "Syntax.h"


-- ---------------------------------------------------------------------
--
-- Constraints for Outputable
--
-- ---------------------------------------------------------------------

#if MIN_VERSION_ghc(8,6,0)
type OUTPUTABLE a pr = (OutputableBndrId a, a ~ GhcPass pr)
#elif MIN_VERSION_ghc(8,4,0)
type OUTPUTABLE a pr = (OutputableBndrId a, SourceTextX a)
#else
type OUTPUTABLE a pr = (OutputableBndrId a, HsSrc a)
#endif


-- ---------------------------------------------------------------------
--
-- Annotation dictionary
--
-- ---------------------------------------------------------------------

{-
isDocComment :: Located AnnotationComment -> Bool
isDocComment x =
  case unLoc x of
    AnnDocCommentNext _  -> True
    AnnDocCommentPrev _  -> True
    AnnDocCommentNamed _ -> True
    AnnDocSection _ _    -> True
    _                    -> False

buildDocMap :: [Located AnnotationComment] -> DocMap
buildDocMap acs = go (Map.empty, Nothing, []) (sortLocated acs)
  where
    go :: ( Map.Map SrcSpan [AnnotationComment]
          , Maybe SrcSpan
          , [AnnotationComment] )
       -> [Located AnnotationComment]
       -> DocMap
    go (acc, keySpan, block) [] =
      case keySpan of
        Nothing -> acc
        Just k  -> Map.insert k (reverse block) acc
    go (acc, keySpan, block) (com:coms) =
      case keySpan of
        Just k ->
          case (k, getLoc com) of
            (RealSrcSpan k', RealSrcSpan com') ->
              if (srcSpanEndLine k' + 1) == srcSpanStartLine com'
                 then go ( acc
                         , Just (combineSrcSpans k (getLoc com))
                         , unLoc com:block )
                         coms
                 else
                   let acc' = Map.insert k (reverse block) acc
                       isDoc = isDocComment com
                       keySpan' | isDoc = Just (getLoc com)
                                | otherwise = Nothing
                       block' | isDoc = [unLoc com]
                              | otherwise = []
                   in  go (acc', keySpan', block') coms
            _ -> go (acc, Nothing, []) coms
        Nothing ->
          if isDocComment com
             then go (acc, Just (getLoc com), [unLoc com]) coms
             else go (acc, Nothing, block) coms

spanStartLine :: SrcSpan -> Int
spanStartLine l =
  case l of
    RealSrcSpan s -> srcSpanStartLine s
    _             -> -1

spanEndLine :: SrcSpan -> Int
spanEndLine l =
  case l of
    RealSrcSpan s -> srcSpanEndLine s
    _             -> -1

-- | Lookup previous documentation comment.
--
-- Here @previous@ means the end line of documentation comment matches
-- to the start line of reference span - offset.
--
lookupPrevDoc :: Int -> SrcSpan -> DocMap -> Maybe [AnnotationComment]
lookupPrevDoc offset l =
  let line = spanStartLine l
      f k a | spanEndLine k == line - offset = Just a
            | otherwise                      = Nothing
  in  Map.foldMapWithKey f

emitPrevDoc :: SPState -> Located a -> SDoc
emitPrevDoc = emitPrevDocWithOffset 1

emitPrevDocWithOffset :: Int -> SPState -> Located a -> SDoc
emitPrevDocWithOffset offset st ref =
  case lookupPrevDoc offset (getLoc ref) (docMap st) of
    Nothing -> empty
    Just as -> vcat (map f as)
  where
    f annotated = case annotated of
      AnnDocCommentNext doc -> case lines doc of
        c:cs -> vcat ((text "-- | " <> text c):
                      map (\ x -> text "--" <> text x) cs)
        []   -> empty
      AnnLineComment doc -> text "-- " <> text doc
      _                  -> ppr annotated

#if !MIN_VERSION_ghc(8,4,0)
-- | 'whenPprDebug' does not exist in ghc 8.2. Defining one with
-- 'ifPprDebug'. Also, number of arguments in 'ifPprDebug' changed in
-- ghc 8.4.
whenPprDebug :: SDoc -> SDoc
whenPprDebug d = ifPprDebug d
#endif

-}


-- ---------------------------------------------------------------------
--
-- HsSrc class
--
-- ---------------------------------------------------------------------

class HsSrc a where
  toHsSrc :: SPState -> a -> SDoc

newtype Hsrc a = Hsrc {unHsrc :: a}

genHsSrc :: (GhcMonad m, HsSrc a) => SPState -> a -> m String
genHsSrc st0 x = do
  flags <- getSessionDynFlags
  unqual <- getPrintUnqual
  -- let st1 = st0 {docMap = buildDocMap (comments st0)}
  return (showSDocForUser flags unqual (toHsSrc st0 x))


-- ---------------------------------------------------------------------
--
-- Instances
--
-- ---------------------------------------------------------------------

instance HsSrc RdrName where
  toHsSrc _ = ppr

instance (HsSrc b) => HsSrc (GenLocated a b) where
  toHsSrc st (L _ e) = toHsSrc st e

instance HsSrc DocDecl where
  toHsSrc _st d = case d of
    DocCommentNext ds        -> commentWithHeader "-- |" ds
    DocCommentPrev ds        -> commentWithHeader "-- ^" ds
    DocCommentNamed name _ds -> text "-- named doc" <> text name
    DocGroup n _ds           -> text "-- group doc" <> text (show n)

#if MIN_VERSION_ghc(8,4,0)
instance OUTPUTABLE a pr
#else
instance (OUTPUTABLE a pr, HasOccName a)
#endif
         => HsSrc (Hsrc (HsModule a)) where
  toHsSrc st (Hsrc a) = case a of
    HsModule Nothing _ imports decls _ mbDoc ->
      vcat [ pp_langExts st
           , pp_mbModuleHeaderDoc mbDoc
           , pp_nonnull imports
           , hsSrc_nonnull st (map (Hsrc . unLoc) decls) ]
    HsModule (Just name) exports imports decls deprec mbDoc ->
      vcat [ pp_langExts st
           , pp_mbModuleHeaderDoc mbDoc
           , case exports of
               Nothing ->
                 pp_header (text "where")
               Just es ->
                 vcat [ pp_header lparen
                      , nest 8
                             (fsep (punctuate comma
                                              (map ppr (unLoc es))))
                      , nest 4 (text ") where")]
           , pp_nonnull imports
           , hsSrc_nonnull st (map (Hsrc . unLoc) decls) ]
      where
        pp_header rest =
          case deprec of
            Nothing -> pp_modname <+> rest
            Just d  -> vcat [pp_modname, ppr d, rest]
        pp_modname = text "module" <+> ppr name

instance OUTPUTABLE a pr => HsSrc (Hsrc (HsDecl a)) where
  toHsSrc st decl =
    case unHsrc decl of
      TyClD _EXT tycld -> toHsSrc st (Hsrc tycld)
      DocD _EXT doc    -> toHsSrc st doc
      decl'            -> ppr decl'

-- XXX: TODO. Documentation for constructors need manual formatting.
instance OUTPUTABLE a pr => HsSrc (Hsrc (TyClDecl a)) where
  toHsSrc _st = ppr . unHsrc


-- -------------------------------------------------------------------
--
-- Auxiliary
--
-- -------------------------------------------------------------------

pp_nonnull :: Outputable t => [t] -> SDoc
pp_nonnull [] = empty
pp_nonnull xs = vcat (map ppr xs)

pp_langExts :: SPState -> SDoc
pp_langExts sp = vcat (map f (langExts sp))
  where
    f (L _ e) = text "{-# LANGUAGE" <+> text e <+> text "#-}"

hsSrc_nonnull :: HsSrc a => SPState -> [a] -> SDoc
hsSrc_nonnull st xs =
  case xs of
    [] -> empty
    _  -> vcat (map (toHsSrc st) xs)

pp_mbModuleHeaderDoc :: Maybe LHsDocString -> SDoc
pp_mbModuleHeaderDoc = maybe empty mod_header
  where
    mod_header (L _ doc) = commentWithHeader "-- |" doc

commentWithHeader :: String -> HsDocString -> SDoc
commentWithHeader header doc =
  case lines (unpackHDS' doc) of
    []   -> empty
    d:ds -> vcat ((text header <+> text d):
                  map (\ x -> text "--" <+> text x) ds)

-- | GHC version compatible function for unpacking 'HsDocString'.
unpackHDS' :: HsDocString -> String
#if MIN_VERSION_ghc(8,6,0)
unpackHDS' = unpackHDS
#else
unpackHDS' (HsDocString fs) = unpackFS fs
#endif
