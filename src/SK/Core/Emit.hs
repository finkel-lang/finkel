{-# LANGUAGE FlexibleInstances #-}
-- | Emit Haskell source code from Haskell syntax data type.
module SK.Core.Emit
  ( HsSrc(..)
  , genHsSrc
  , buildDocMap
  , lookupDoc
  , lookupPrevDoc
  , lookupNextDoc
  ) where

-- Not yet sure whether the use of LINE pragma is showing helpful error
-- messages.
--
-- Position of S-expression code and generated haskell code does not
-- always match with LINE pragmas in top-level definitions. When
-- generated Haskell source is longer than lisp code (which is likely
-- happen with macros), the line PRAGMAs in top-level bindings only are
-- too sparse to show the precise location of errors.

-- From 'base'
import qualified Data.Map as Map

-- From 'ghc'
import GHC
import OccName
import Outputable
import SrcLoc

-- Internal
import SK.Core.Lexer
import SK.Core.GHC (unpackFS)


-- ---------------------------------------------------------------------
--
-- Annotation dictionary
--
-- ---------------------------------------------------------------------

type DocMap = Map.Map SrcSpan [AnnotationComment]

isDocComment :: Located AnnotationComment -> Bool
isDocComment x =
  case unLoc x of
    AnnDocCommentNext _ -> True
    AnnDocCommentPrev _ -> True
    AnnDocCommentNamed _ -> True
    AnnDocSection _ _    -> True
    -- AnnDocOptions _ -> True
    _ -> False

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
    _ -> -1

spanEndLine :: SrcSpan -> Int
spanEndLine l =
  case l of
    RealSrcSpan s -> srcSpanEndLine s
    _ -> -1

lookupDoc :: SrcSpan -> DocMap -> Maybe [AnnotationComment]
lookupDoc l =
  let line = spanStartLine l
      f k a = if spanStartLine k == line
                 then Just a
                 else Nothing
  in  Map.foldMapWithKey f

-- | Lookup previous documentation comment.
--
-- Here @previous@ means the end line of documentation comment matches
-- to the start line of reference span - offset.
--
lookupPrevDoc :: Int -> SrcSpan -> DocMap -> Maybe [AnnotationComment]
lookupPrevDoc offset l =
  let line = spanStartLine l
      f k a = if spanEndLine k == line - offset
                 then Just a
                 else Nothing
  in  Map.foldMapWithKey f

lookupNextDoc :: SrcLoc -> DocMap -> Maybe [AnnotationComment]
lookupNextDoc = undefined


-- ---------------------------------------------------------------------
--
-- HsSrc class
--
-- ---------------------------------------------------------------------

class HsSrc a where
  toHsSrc :: SPState -> a -> SDoc

genHsSrc :: (GhcMonad m, HsSrc a) => SPState -> a -> m String
genHsSrc st x =
  do flags <- getSessionDynFlags
     return (showSDocForUser flags neverQualify (toHsSrc st x))

unAnnotateComment :: AnnotationComment -> SDoc
unAnnotateComment c =
  case c of
    AnnLineComment str -> text str
    AnnDocCommentNext str -> text str
    _ -> ppr c

mbHeaderComment :: Outputable a => SPState -> Maybe (Located a) -> SDoc
mbHeaderComment st mbdoc =
  case mbdoc of
    Nothing -> empty
    Just a  ->
      let dm = buildDocMap (comments st)
          result = case lookupDoc (getLoc a) dm of
                     Just xs -> vcat (map unAnnotateComment xs)
                     Nothing -> empty
      in  result

pp_mb :: Outputable t => Maybe t -> SDoc
pp_mb (Just x) = ppr x
pp_mb Nothing  = empty

pp_nonnull :: Outputable t => [t] -> SDoc
pp_nonnull [] = empty
pp_nonnull xs = vcat (map ppr xs)

hsSrc_nonnull :: HsSrc a => SPState -> [a] -> SDoc
hsSrc_nonnull st xs =
  case xs of
    [] -> empty
    _  -> vcat (map (toHsSrc st) xs)

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
 case lookupPrevDoc offset (getLoc ref) (buildDocMap (comments st)) of
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

instance (HsSrc a, OutputableBndr a, HasOccName a)
          => HsSrc (HsModule a) where
  toHsSrc st a = case a of
    HsModule Nothing _ imports decls _ mbDoc ->
      pp_mb mbDoc
        $$ pp_nonnull imports
        $$ hsSrc_nonnull st decls
    HsModule (Just name) exports imports decls deprec mbDoc ->
      vcat [ linePragma' st 1
           , mbHeaderComment st mbDoc
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
           , hsSrc_nonnull st decls]
      where
        pp_header rest =
          case deprec of
            Nothing -> pp_modname <+> rest
            Just d  -> vcat [ pp_modname, ppr d, rest ]
        pp_modname = text "module" <+> ppr name

instance (OutputableBndr a, HsSrc a) => HsSrc (HsExpr a) where
  toHsSrc _ = ppr

instance (OutputableBndr a, HsSrc a) => HsSrc (HsDecl a) where
  toHsSrc st decl =
    case decl of
      ValD binds -> toHsSrc st binds
      SigD sigd  -> toHsSrc st sigd
      _          -> ppr decl

instance (OutputableBndr a, HsSrc a) => HsSrc (HsBindLR a a) where
  toHsSrc st binds =
    case binds of
      FunBind { fun_id = fun
              , fun_co_fn = wrap
              , fun_matches = matches
              , fun_tick = ticks }
        -> pprTicks empty (if null ticks
                              then empty
                              else text "-- ticks = " <> ppr ticks)
           $$ ifPprDebug (pprBndr LetBind (unLoc fun))

           -- Additional operation to `ppr' for `toHsSrc'. Find
           -- corresponding haddock comment for function binding, and
           -- print it out.
           --- $$ linePragma st fun
           $$ emitPrevDoc st fun

           $$ pprFunBind (unLoc fun) matches
           $$ ifPprDebug (ppr wrap)
      _ -> ppr binds

instance (OutputableBndr a, HsSrc a) => HsSrc (Sig a) where
  toHsSrc st sig =
    case sig of
      TypeSig vars ty ->
        (case vars of
           [] -> empty
           var:_ -> emitPrevDoc st var)
        $$ pprVarSig (map unLoc vars) (ppr ty)
      _               -> ppr sig
