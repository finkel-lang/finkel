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
  , buildDocMap
  ) where

-- base
#if MIN_VERSION_base(4,11,0)
import Prelude hiding ((<>))
#endif

-- containers
import qualified Data.Map as Map

-- ghc
import GHC (OutputableBndrId, getPrintUnqual)

import ApiAnnotation (AnnotationComment(..))
import FastString (unpackFS)
import GhcMonad (GhcMonad(..), getSessionDynFlags)
import HsBinds (HsBindLR(..), Sig(..), pprTicks, pprVarSig)
import HsDecls (HsDecl(..))
import HsDoc (LHsDocString)
import HsExpr (HsExpr(..), pprFunBind)
import HsSyn (HsModule(..))
import Outputable ( ($$), (<+>), (<>)
                  , BindingSite(..), Outputable(..), SDoc
                  , comma, doubleQuotes, empty, fsep
                  , int, lparen, nest, pprBndr, punctuate
                  , showSDocForUser, text, vcat
#if MIN_VERSION_ghc(8,4,0)
                  , whenPprDebug
#else
                  , ifPprDebug
#endif
                  )
import RdrName (RdrName)
import SrcLoc ( Located, GenLocated(..), SrcLoc, SrcSpan(..)
              , combineSrcSpans, getLoc, unLoc
              , sortLocated, srcSpanEndLine, srcSpanStartLine )

#if MIN_VERSION_ghc(8,6,0)
import HsDoc (HsDocString, unpackHDS)
import HsExtension (GhcPass)
#elif MIN_VERSION_ghc(8,4,0)
import HsDoc (HsDocString(..))
import HsExtension (SourceTextX)
#else
import HsDoc (HsDocString(..))
import OccName (HasOccName(..))
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
  let st1 = st0 {docMap = buildDocMap (comments st0)}
  return (showSDocForUser flags unqual (toHsSrc st1 x))

unAnnotateComment :: AnnotationComment -> SDoc
unAnnotateComment c =
  case c of
    AnnLineComment str    -> text "--" <> text str
    AnnDocCommentNext str -> text "-- |" <> text str
    _                     -> ppr c

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

-- Not yet sure whether the use of LINE pragma is showing helpful error
-- messages.
--
-- Position of S-expression code and generated haskell code does not
-- always match with LINE pragmas in top-level definitions. When
-- generated Haskell source is longer than lisp code (which is likely
-- happen with macros), the line PRAGMAs in top-level bindings only are
-- too sparse to show the precise location of errors.

linePragma' :: SPState -> Int -> SDoc
linePragma' st linum =
  text "{-# LINE" <+>
  int linum <+>
  doubleQuotes (text file) <+>
  text "#-}"
    where
      file = replaceExtension (unpackFS (targetFile st))
      replaceExtension name =
        reverse ("sh" ++ dropWhile (/= '.') (reverse name))

emitPrevDoc :: SPState -> Located a -> SDoc
emitPrevDoc = emitPrevDocWithOffset 1

emitPrevDocWithOffset :: Int -> SPState -> Located a -> SDoc
emitPrevDocWithOffset offset st ref =
 case lookupPrevDoc offset (getLoc ref) (docMap st) of
    Nothing -> empty
    Just as -> vcat (map unAnnotateComment as)


-- ---------------------------------------------------------------------
--
-- Instances
--
-- ---------------------------------------------------------------------

instance HsSrc RdrName where
  toHsSrc _ = ppr

instance HsSrc SrcLoc where
  toHsSrc _ = ppr

instance (HsSrc b) => HsSrc (GenLocated a b) where
  toHsSrc st (L _ e) = toHsSrc st e

#if MIN_VERSION_ghc(8,4,0)
instance OUTPUTABLE a pr
#else
instance (OUTPUTABLE a pr, HasOccName a)
#endif
         => HsSrc (Hsrc (HsModule a)) where
  toHsSrc st (Hsrc a) = case a of
    HsModule Nothing _ imports decls _ mbDoc ->
      vcat [ linePragma' st 1
           , pp_langExts st
           , pp_mbModuleHeaderDoc mbDoc
           , pp_nonnull imports
           , hsSrc_nonnull st (map (Hsrc . unLoc) decls) ]
    HsModule (Just name) exports imports decls deprec mbDoc ->
      vcat [ linePragma' st 1
           , pp_langExts st
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

instance OUTPUTABLE a pr => HsSrc (Hsrc (HsExpr a)) where
  toHsSrc _ = ppr . unHsrc

instance OUTPUTABLE a pr => HsSrc (Hsrc (HsDecl a)) where
  toHsSrc st (Hsrc decl) =
    case decl of
      ValD _EXT binds -> toHsSrc st (Hsrc binds)
      SigD _EXT sigd  -> toHsSrc st (Hsrc sigd)
      _               -> ppr decl

instance OUTPUTABLE a pr => HsSrc (Hsrc (HsBindLR a a)) where
  toHsSrc st (Hsrc binds) =
    case binds of
      FunBind { fun_id = fun
              , fun_co_fn = wrap
              , fun_matches = matches
              , fun_tick = ticks }
        -> pprTicks empty (if null ticks
                              then empty
                              else text "-- ticks = " <> ppr ticks)
           $$ whenPprDebug (pprBndr LetBind (unLoc fun))

           -- Additional operation to `ppr' for `toHsSrc'. Find
           -- corresponding haddock comment for function binding, and
           -- print it out.
           $$ emitPrevDoc st fun

           $$ pprFunBind matches
           $$ whenPprDebug (ppr wrap)
      _ -> ppr binds

instance OUTPUTABLE a pr => HsSrc (Hsrc (Sig a)) where
  toHsSrc st (Hsrc sig) =
    case sig of
     TypeSig _EXT vars ty ->
        (case vars of
           []    -> empty
           var:_ -> emitPrevDoc st var)
        $$ pprVarSig (map unLoc vars) (ppr ty)
     _                    -> ppr sig


-- -------------------------------------------------------------------
--
-- Auxiliary
--
-- -------------------------------------------------------------------

pp_mbModuleHeaderDoc :: Maybe LHsDocString -> SDoc
pp_mbModuleHeaderDoc = maybe empty mod_header
  where
    mod_header (L _ doc) =
      case lines (unpackHDS' doc) of
        []   -> empty
        d:ds -> vcat ((text "-- |" <> text d):
                      map (\ x -> text "-- " <> text x) ds)

-- | GHC version compatible function for unpacking 'HsDocString'.
unpackHDS' :: HsDocString -> String
#if MIN_VERSION_ghc(8,6,0)
unpackHDS' = unpackHDS
#else
unpackHDS' (HsDocString fs) = unpackFS fs
#endif

#if !MIN_VERSION_ghc(8,4,0)
-- | 'whenPprDebug' does not exist in ghc 8.2. Defining one with
-- 'ifPprDebug'. Also, number of arguments in 'ifPprDebug' changed in
-- ghc 8.4.
whenPprDebug :: SDoc -> SDoc
whenPprDebug d = ifPprDebug d
#endif
