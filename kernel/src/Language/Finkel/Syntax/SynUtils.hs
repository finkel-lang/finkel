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

#include "Syntax.h"

-- base
import           Data.Char               (isUpper)

-- ghc
import           Bag                     (listToBag)
import           BasicTypes              (SourceText (..))
import           FastString              (FastString, fsLit, headFS, unpackFS)
import           HaddockUtils            (addConDoc)

import           GHC_Hs_Binds            (HsLocalBindsLR (..),
                                          HsValBindsLR (..), emptyLocalBinds)
import           GHC_Hs_Decls            (HsDecl (..), LConDecl,
                                          LDataFamInstDecl, LDocDecl,
                                          LFamilyDecl, LTyFamInstDecl)
import           GHC_Hs_Doc              (LHsDocString)
import           GHC_Hs_Expr             (GRHSs (..), LGRHS)
import           GHC_Hs_Lit              (HsOverLit (..))
import           GHC_Hs_Pat              (HsRecField' (..), LHsRecField,
                                          LHsRecUpdField)
import           GHC_Hs_Types            (AmbiguousFieldOcc (..), FieldOcc (..),
                                          HsTyVarBndr (..), HsType (..),
                                          LHsContext, mkFieldOcc)
import           GHC_Hs_Utils            (mkHsIntegral)

import           Lexeme                  (isLexCon, isLexConSym, isLexVar,
                                          isLexVarSym)
import           Lexer                   (P (..), ParseResult (..))
import           OccName                 (NameSpace, srcDataName, tcName,
                                          tvName, varName)
import           OrdList                 (OrdList, toOL)
import           RdrHsSyn                (cvTopDecls)
import qualified RdrHsSyn
import           RdrName                 (RdrName, mkQual, mkUnqual,
                                          mkVarUnqual, nameRdrName)
import           SrcLoc                  (GenLocated (..), Located, SrcSpan,
                                          combineLocs, noLoc, unLoc)
import           TysWiredIn              (consDataConName)

#if MIN_VERSION_ghc(8,10,0)
import           FastString              (bytesFS)
#elif MIN_VERSION_ghc(8,6,0)
import           FastString              (fastStringToByteString)
#endif

#if MIN_VERSION_ghc(8,8,0)
import qualified SrcLoc
#endif

#if MIN_VERSION_ghc(8,6,0)
import           BasicTypes              (PprPrec (..), appPrec, funPrec,
                                          opPrec, sigPrec, topPrec)
import           GHC_Hs_Doc              (HsDocString,
                                          mkHsDocStringUtf8ByteString)
#else
import           GHC_Hs_Doc              (HsDocString (..))
#endif

#if MIN_VERSION_ghc(8,10,0)
import           GHC_Hs_Extension        (noExtField)
#elif MIN_VERSION_ghc(8,6,0)
import           GHC_Hs_Extension        (noExt)
#else
import           PlaceHolder             (PlaceHolder (..), placeHolderType)
#endif

-- Internal
import           Language.Finkel.Builder
import           Language.Finkel.Form

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
      Just q@(_, name') -> if isLexCon name'
                              then mkQual upperCaseNameSpace q
                              else mkQual varName q

  -- Variable.
  | otherwise = mkVarUnqual name
  where
    x = headFS name
{-# INLINE mkRdrName' #-}

-- See also "compiler/parser/Lexer.x.source" in ghc source code. It has
-- private function named "splitQualName".
splitQualName :: FastString -> Maybe (FastString, FastString)
splitQualName fstr =
  -- e.g. ":.+.", ":+:". Symbol may contain ".".
  if isLexConSym fstr
     then Nothing
     else go (unpackFS fstr) "" []
  where
    go []       _   []  = Nothing
    go []       tmp acc = let mdl = reverse (tail (concat acc))
                              var = reverse tmp
                          in  Just (fsLit mdl, fsLit var)
    go ('.':[]) tmp acc = go [] ('.':tmp) acc
    go ('.':cs) tmp acc = go cs [] (('.':tmp) : acc)
    go (c:cs)   tmp acc = go cs (c:tmp) acc
{-# INLINE splitQualName #-}

checkVarId :: Code -> FastString -> Builder ()
checkVarId orig name =
  if isLexVar name
     then return ()
     else setLastToken orig >> failB "invalid variable identifier"
{-# INLINE checkVarId #-}

getConId :: Code -> Builder FastString
getConId orig@(LForm (L _ form)) =
  case form of
    Atom (ASymbol sym)
       -- `isLexVarSym' is for "TypeOperators" extension.
      | isLexCon sym    -> return sym
      | isLexVarSym sym -> return sym
    _ -> setLastToken orig >> failB "invalid constructor identifier"
{-# INLINE getConId #-}

getVarOrConId :: Code -> Builder FastString
getVarOrConId orig@(LForm (L _ form)) =
  case form of
    Atom (ASymbol sym)
      | isLexCon sym -> return sym
      | isLexVar sym -> return sym
    _ -> setLastToken orig >> failB "invalid identifier"
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
    decls' = RdrHsSyn.cvTopDecls (toOL decls)
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
    unambiguous = Unambiguous NOEXT rdr
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
cvBindsAndSigs fb =
  do ps <- fmap ghcPState getBState
     case unP (fmap toCategorizedDecls (RdrHsSyn.cvBindsAndSigs fb)) ps of
       POk _ cd -> return cd
       _        -> builderError

kindedTyVar :: Code -> Code -> HType -> Builder HTyVarBndr
kindedTyVar (LForm (L l _dc)) name kind =
  case name of
    LForm (L ln (Atom (ASymbol name'))) -> do
       let name'' = L ln (mkUnqual tvName name')
       return $! L l (KindedTyVar NOEXT name'' kind)
    _ -> builderError
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
#if MIN_VERSION_ghc(8,10,0)
hsDocString = mkHsDocStringUtf8ByteString . bytesFS
#elif MIN_VERSION_ghc(8,6,0)
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
  HsValBinds NOEXT (ValBinds NOEXT binds sigs)
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
             , hst_xqual = NOEXT
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
