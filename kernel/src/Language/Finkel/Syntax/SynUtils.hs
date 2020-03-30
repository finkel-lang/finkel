{-# LANGUAGE CPP #-}
-- | Utility codes for syntax.
module Language.Finkel.Syntax.SynUtils
  ( -- * This module
    module Language.Finkel.Syntax.SynUtils

#if MIN_VERSION_ghc(8,6,0)
     -- * Re-export for ghc version compatibility
  , PprPrec(..), topPrec, sigPrec, opPrec, funPrec, appPrec
#endif
  ) where

-- base
import           Data.Char               (isUpper)

-- ghc
import           Bag                     (consBag, emptyBag, listToBag)
import           BasicTypes              (SourceText (..))
import           FastString              (FastString, fsLit, headFS, unpackFS)
import           HaddockUtils            (addConDoc)
import           HsBinds                 (HsBindLR (..), HsLocalBindsLR (..),
                                          HsValBindsLR (..), emptyLocalBinds)
import           HsDecls                 (HsDecl (..), InstDecl (..), LConDecl,
                                          LDataFamInstDecl, LDocDecl,
                                          LFamilyDecl, LTyFamInstDecl,
                                          TyClDecl (..))
import           HsDoc                   (LHsDocString)
import           HsExpr                  (GRHSs (..), LGRHS, LHsExpr, LMatch,
                                          Match (..), MatchGroup (..))
import           HsLit                   (HsOverLit (..))
import           HsPat                   (HsRecField' (..), LHsRecField,
                                          LHsRecUpdField)
import           HsTypes                 (AmbiguousFieldOcc (..), FieldOcc (..),
                                          HsTyVarBndr (..), HsType (..),
                                          LHsContext, mkFieldOcc)
import           HsUtils                 (mkFunBind, mkHsIntegral)
import           Lexeme                  (isLexCon, isLexConSym, isLexVar,
                                          isLexVarSym)
import           OccName                 (NameSpace, srcDataName, tcName,
                                          tvName, varName)
import           OrdList                 (OrdList, fromOL, toOL)
import           RdrHsSyn                (cvTopDecls)
import           RdrName                 (RdrName, mkQual, mkUnqual,
                                          mkVarUnqual, nameRdrName)
import           SrcLoc                  (GenLocated (..), Located, SrcSpan,
                                          combineLocs, combineSrcSpans, noLoc,
                                          unLoc)
import           TysWiredIn              (consDataConName)

#if MIN_VERSION_ghc(8,8,0)
import qualified SrcLoc
#endif

#if MIN_VERSION_ghc(8,6,0)
import           BasicTypes              (PprPrec (..), appPrec, funPrec,
                                          opPrec, sigPrec, topPrec)
import           FastString              (fastStringToByteString)
import           HsDoc                   (HsDocString,
                                          mkHsDocStringUtf8ByteString)
#else
import           HsDoc                   (HsDocString (..))
#endif

#if MIN_VERSION_ghc(8,6,0)
import           HsExtension             (noExt)
#else
import           PlaceHolder             (PlaceHolder (..), placeHolderType)
#endif

-- Internal
import           Language.Finkel.Builder
import           Language.Finkel.Form

#include "Syntax.h"


mkRdrName :: FastString -> RdrName
mkRdrName = mkRdrName' tcName
{-# INLINE mkRdrName #-}

mkVarRdrName :: FastString -> RdrName
mkVarRdrName = mkRdrName' srcDataName
{-# INLINE mkVarRdrName #-}

mkRdrName' :: NameSpace -> FastString -> RdrName
mkRdrName' upperCaseNameSpace name
  -- ':' is special syntax. It is defined in module "GHC.Types" in
  -- package "ghc-prim", but not exported.
  | name == fsLit ":" = nameRdrName consDataConName

  -- Names starting with ':' are data constructor.
  | x == ':' = mkUnqual srcDataName name

  -- Names starting with capital letters might be qualified var names or
  -- data constructor names.
  | isUpper x =
    case splitQualName name of
      Nothing -> mkUnqual srcDataName name
      Just q@(_, name')
         | isLexCon name' -> mkQual upperCaseNameSpace q
         | otherwise      -> mkQual varName q

  -- Variable.
  | otherwise = mkVarUnqual name
  where
    x = headFS name
{-# INLINE mkRdrName' #-}

-- See also "compiler/parser/Lexer.x.source" in ghc source code. It has
-- private function named "splitQualName".
splitQualName :: FastString -> Maybe (FastString, FastString)
splitQualName fstr
  -- e.g. ":.+.", ":+:". Symbol may contain ".".
  | isLexConSym fstr = Nothing
  | otherwise = go (unpackFS fstr) "" []
  where
    go str0 tmp acc =
      case str0 of
        [] | null acc  -> Nothing
           | otherwise ->
             let mdl = reverse (tail (concat acc))
                 var = (reverse tmp)
             in  Just (fsLit mdl , fsLit var)
        c:str1
           | c == '.' ->
             case str1 of
               [] -> go str1 (c:tmp) acc
               _  -> go str1 [] ((c:tmp) : acc)
           | otherwise -> go str1 (c:tmp) acc
{-# INLINE splitQualName #-}

checkVarId :: Code -> FastString -> Builder ()
checkVarId orig name
  | isLexVar name = return ()
  | otherwise = setLastToken orig >> failB "invalid variable identifier"
{-# INLINE checkVarId #-}

getConId :: Code -> Builder FastString
getConId orig@(LForm (L _ form))
  | Atom (ASymbol sym) <- form
  -- `isLexVarSym' is for "TypeOperators" extension.
  , isLexCon sym || isLexVarSym sym
  = return sym
  | otherwise = do
    setLastToken orig
    failB "invalid constructor identifier"
{-# INLINE getConId #-}

getVarOrConId :: Code -> Builder FastString
getVarOrConId orig@(LForm (L _ form))
  | Atom (ASymbol sym) <- form, isLexCon sym || isLexVar sym = return sym
  | otherwise = do
    setLastToken orig
    failB "invalid identifier"
{-# INLINE getVarOrConId #-}

-- | Build 'HLocalBinds' from list of 'HDecl's.
declsToBinds :: SrcSpan -> [HDecl] -> HLocalBinds
declsToBinds l decls = L l binds'
  where
    binds' = case decls of
      [] -> emptyLocalBinds
      _  -> mkHsValBinds_compat (listToBag binds) sigs
    -- Using 'RdrHsSyn.cvTopDecls' to group same names in where
    -- clause. Perhaps better to do similar things done in
    -- 'RdrHsSyn.cvBindGroup', which is dedicated for 'P' monad ...
    decls' = cvTopDecls (toOL decls)
    (binds, sigs) = go ([],[]) decls'
    go (bs,ss) ds =
      case ds of
        []    -> (bs, ss)
        d:ds' -> case d of
          L ld (ValD _EXT b) -> go (L ld b:bs,ss) ds'
          L ld (SigD _EXT s) -> go (bs,L ld s:ss) ds'
          -- XXX: Ignoring.
          _                  -> go (bs,ss) ds'

-- Function defined in 'HsUtils', not exported.
mkLocatedList ::  [Located a] -> Located [Located a]
mkLocatedList [] = noLoc []
mkLocatedList ms = L (combineLocs (head ms) (last ms)) ms
{-# INLINE mkLocatedList #-}

-- | Convert record field constructor expression to record field update
-- expression.
cfld2ufld :: LHsRecField PARSED HExpr
          -> LHsRecUpdField PARSED
-- Almost same as 'mk_rec_upd_field' in 'RdrHsSyn'
#if MIN_VERSION_ghc(8,6,0)
cfld2ufld (L l0 (HsRecField (L l1 (FieldOcc _ rdr)) arg pun)) =
  L l0 (HsRecField (L l1 unambiguous) arg pun)
  where
    unambiguous = Unambiguous noExt rdr
cfld2ufld _ = error "Language.Finkel.Syntax.SynUtils:cfld2ufld"
#else
cfld2ufld (L l0 (HsRecField (L l1 (FieldOcc rdr _)) arg pun)) =
  L l0 (HsRecField (L l1 unambiguous) arg pun)
  where
    unambiguous = Unambiguous rdr PlaceHolder
#endif
{-# INLINE cfld2ufld #-}

-- | Make 'HsRecField' with given name and located data.
mkcfld :: (Located FastString, a) -> LHsRecField PARSED a
mkcfld ((L nl name), e) =
  L nl HsRecField { hsRecFieldLbl = mkfname name
                  , hsRecFieldArg = e
                  , hsRecPun = False }
  where
    mkfname n = L nl (mkFieldOcc (L nl (mkRdrName n)))
{-# INLINE mkcfld #-}

quotedSourceText :: String -> SourceText
quotedSourceText s = SourceText $ "\"" ++ s ++ "\""
{-# INLINE quotedSourceText #-}

-- Following `cvBindsAndSigs`, `getMonoBind`, `has_args`, and
-- `makeFunBind` functions are based on resembling functions defined in
-- `RdrHsSyn` module in ghc package.
--
-- Unlike the original version, `cvBindsAndSigs` has pattern matches
-- for 'ValD' and 'SigD' only, and `getMonoBind` ignores 'DocD'
-- declarations.

data CategorizedDecls = CategorizedDecls
  { cd_binds :: HBinds
  , cd_sigs  :: [HSig]
  , cd_fds   :: [LFamilyDecl PARSED]
  , cd_tfis  :: [LTyFamInstDecl PARSED]
  , cd_dfis  :: [LDataFamInstDecl PARSED]
  , cd_docs  :: [LDocDecl]
  }

toCategorizedDecls :: ( HBinds
                      , [HSig]
                      , [LFamilyDecl PARSED]
                      , [LTyFamInstDecl PARSED]
                      , [LDataFamInstDecl PARSED]
                      , [LDocDecl] )
                   -> CategorizedDecls
toCategorizedDecls (binds, sigs, fds, tfis, dfis, docs) =
  CategorizedDecls { cd_binds = binds
                   , cd_sigs = sigs
                   , cd_fds = fds
                   , cd_tfis = tfis
                   , cd_dfis = dfis
                   , cd_docs = docs }

cvBindsAndSigs :: OrdList HDecl -> Builder CategorizedDecls
cvBindsAndSigs fb = fmap toCategorizedDecls (go (fromOL fb))
  where
    go [] = return (emptyBag, [], [], [], [], [])
    go (L l (ValD _EXT d) : ds) = do
      let (b', ds') = getMonoBind (L l d) ds
      (bs, ss, fs, tfis, dfis, docs) <- go ds'
      return (b' `consBag` bs, ss, fs, tfis, dfis, docs)
    go (L l decl : ds) = do
      (bs, ss, fs, tfis, dfis, docs) <- go ds
      case decl of
        SigD _EXT s ->
          return (bs, L l s:ss, fs, tfis, dfis, docs)
        TyClD _EXT (FamDecl _EXT f) ->
          return (bs, ss, L l f:fs, tfis, dfis, docs)
        InstD _EXT (TyFamInstD {tfid_inst = tfi}) ->
          return (bs, ss, fs, L l tfi:tfis, dfis, docs)
        InstD _EXT (DataFamInstD {dfid_inst=dfi}) ->
          return (bs, ss, fs, tfis, L l dfi:dfis, docs)
        DocD _EXT doc ->
          return (bs, ss, fs, tfis, dfis, L l doc:docs)

        -- XXX: Ignoring other constructors.
        _ -> return (bs, ss, fs, tfis, dfis, docs)

getMonoBind :: HBind -> [HDecl] -> (HBind, [HDecl])
getMonoBind (L loc1 (FunBind { fun_id = fun_id1@(L _ f1),
                               fun_matches
                                 = MG { mg_alts = L _ mtchs1 }}))
            binds
  | has_args mtchs1 = go mtchs1 loc1 binds
  where
    go mtchs loc
       (L loc2 (ValD _EXT (FunBind { fun_id = L _ f2,
                                     fun_matches
                                       = MG { mg_alts = L _ mtchs2 }}))
                : binds2)
      | f1 == f2 = go (mtchs2 ++ mtchs)
                      (combineSrcSpans loc loc2) binds2
    go mtchs loc binds2
      = (L loc (mkFunBind fun_id1 (reverse mtchs)), binds2)
      -- Reverse the final matches, to get it back in the right order

getMonoBind bind binds = (bind, binds)

-- Don't group together FunBinds if they have no arguments.  This is
-- necessary that variable bindings with no arguments are now treated as
-- FunBinds rather than pattern bindings.
has_args :: [LMatch PARSED (LHsExpr PARSED)] -> Bool
has_args (L _ mtch:_) = not (null (m_pats mtch))
has_args []           = error "Language.Finkel.Syntax.SynUtils:has_args"

kindedTyVar :: Code -> Code -> HType -> Builder HTyVarBndr
kindedTyVar (LForm (L l _dc)) name kind
  | LForm (L ln (Atom (ASymbol name'))) <- name = do
    let name'' = L ln (mkUnqual tvName name')
    return (L l (KindedTyVar NOEXT name'' kind))
  | otherwise = builderError
{-# INLINE kindedTyVar #-}

codeToUserTyVar :: Code -> HTyVarBndr
codeToUserTyVar code =
  case code of
    LForm (L l (Atom (ASymbol name)))
     -> L l (UserTyVar NOEXT (L l (mkUnqual tvName name)))
    _ -> error "Language.Finkel.Syntax.SynUtils:codeToUserTyVar"
{-# INLINE codeToUserTyVar #-}

-- | Auxiliary function to make 'HsDocString'.
hsDocString :: FastString -> HsDocString
#if MIN_VERSION_ghc(8,6,0)
hsDocString = mkHsDocStringUtf8ByteString . fastStringToByteString
#else
hsDocString = HsDocString
#endif
{-# INLINE hsDocString #-}

-- | Auxiliary function to absorb version compatibiity of
-- 'mkHsIntegral'.
mkHsIntegral_compat :: IntegralLit -> HsOverLit PARSED
mkHsIntegral_compat il =
#if MIN_VERSION_ghc(8,6,0)
    mkHsIntegral il
#elif MIN_VERSION_ghc(8,4,0)
    mkHsIntegral il placeHolderType
#else
    mkHsIntegral (il_text il) (il_value il) placeHolderType
#endif
{-# INLINE mkHsIntegral_compat #-}

mkGRHSs :: [LGRHS PARSED t] -> [HDecl] -> SrcSpan -> GRHSs PARSED t
mkGRHSs grhss decls l = GRHSs NOEXT grhss (declsToBinds l decls)
{-# INLINE mkGRHSs #-}

mkHsValBinds_compat :: HBinds -> [HSig] -> HsLocalBindsLR PARSED PARSED
mkHsValBinds_compat binds sigs =
#if MIN_VERSION_ghc(8,6,0)
  HsValBinds noExt (ValBinds noExt binds sigs)
#else
  HsValBinds (ValBindsIn binds sigs)
#endif
{-# INLINE mkHsValBinds_compat #-}

mkHsQualTy_compat :: LHsContext PARSED -> HType -> HsType PARSED
mkHsQualTy_compat ctxt body
  | nullLHsContext ctxt = unLoc body
  | otherwise =
    HsQualTy { hst_ctxt = ctxt
#if MIN_VERSION_ghc(8,6,0)
             , hst_xqual = noExt
#endif
             , hst_body = body }
{-# INLINE mkHsQualTy_compat #-}

nullLHsContext :: LHsContext PARSED -> Bool
nullLHsContext (L _ cs) = null cs
{-# INLINE nullLHsContext #-}

addConDoc' :: Maybe LHsDocString -> LConDecl a -> LConDecl a
addConDoc' = flip addConDoc
{-# INLINE addConDoc' #-}

addConDoc'' :: LHsDocString -> LConDecl a -> LConDecl a
addConDoc'' = flip addConDoc . Just
{-# INLINE addConDoc'' #-}


-- For 8.8.0 compatibility in source code location management

#if MIN_VERSION_ghc(8,8,0)
dL :: SrcLoc.HasSrcSpan a => a -> Located (SrcLoc.SrcSpanLess a)
dL = SrcLoc.dL

cL :: SrcLoc.HasSrcSpan a => SrcSpan -> SrcLoc.SrcSpanLess a -> a
cL = SrcLoc.cL
#else
dL :: Located a -> Located a
dL = id

cL :: SrcSpan -> a -> Located a
cL = L
#endif

{-# INLINE cL #-}
{-# INLINE dL #-}

-- For parenthesizing
--
-- Until ghc 8.6.0, 'PprPrec' did not exist. Following 'PprPrec' and its
-- predefined values are taken from 'compiler/basicTypes/BasicTypes.hs' in ghc
-- source code. Re-exporting from this module.

#if !MIN_VERSION_ghc(8,6,0)
-- | A general-purpose pretty-printing precedence type.
newtype PprPrec = PprPrec Int deriving (Eq, Ord, Show)
-- See Note [Precedence in types] in ghc source.

topPrec, sigPrec, funPrec, opPrec, appPrec :: PprPrec
topPrec = PprPrec 0 -- No parens
sigPrec = PprPrec 1 -- Explicit type signatures
funPrec = PprPrec 2 -- Function args; no parens for constructor apps
                    -- See [Type operator precedence] for why both
                    -- funPrec and opPrec exist.
opPrec  = PprPrec 2 -- Infix operator
appPrec = PprPrec 3 -- Constructor args; no parens for atomic
#endif
