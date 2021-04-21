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
#include "ghc_modules.h"

-- base
#if MIN_VERSION_ghc(9,0,0)
import           Control.Monad             (mplus)
#endif
import           Data.Char                 (isUpper)

-- bytestring
#if MIN_VERSION_ghc(9,0,0)
import qualified Data.ByteString.Char8     as BS
#endif


-- ghc
import           GHC_Data_Bag              (listToBag)
import           GHC_Data_FastString       (FastString, fsLit, headFS, unpackFS)
import           GHC_Types_Basic           (SourceText (..))

import           GHC_Hs_Binds              (HsLocalBindsLR (..),
                                            HsValBindsLR (..), emptyLocalBinds)
import           GHC_Hs_Decls              (HsDecl (..), LConDecl,
                                            LDataFamInstDecl, LDocDecl,
                                            LFamilyDecl, LTyFamInstDecl)
import           GHC_Hs_Doc                (LHsDocString)
import           GHC_Hs_Expr               (GRHSs (..), LGRHS)
import           GHC_Hs_Lit                (HsOverLit (..))
import           GHC_Hs_Pat                (HsRecField' (..), LHsRecField,
                                            LHsRecUpdField)
import           GHC_Hs_Type               (AmbiguousFieldOcc (..),
                                            FieldOcc (..), HsTyVarBndr (..),
                                            HsType (..), LHsContext, mkFieldOcc)
import           GHC_Hs_Utils              (mkHsIntegral)

import           GHC_Builtin_Types         (consDataConName)
import           GHC_Data_OrdList          (OrdList, toOL)
import           GHC_Parser_Lexer          (P (..), ParseResult (..))
import qualified GHC_Parser_PostProcess    as PostProcess
import           GHC_Types_Name_Occurrence (NameSpace, srcDataName, tcName,
                                            tvName, varName)
import           GHC_Types_Name_Reader     (RdrName, mkQual, mkUnqual,
                                            mkVarUnqual, nameRdrName)
import           GHC_Types_SrcLoc          (GenLocated (..), Located, SrcSpan,
                                            combineLocs, noLoc, unLoc)
import           GHC_Utils_Lexeme          (isLexCon, isLexConSym, isLexVar,
                                            isLexVarSym)

#if MIN_VERSION_ghc(9,0,0)
import           GHC_Data_FastString       (mkFastStringByteString)
import           GHC_Hs_Decls              (ConDecl (..))
import           GHC_Hs_Type               (LHsTyVarBndr)
import           GHC_Types_Var             (Specificity (..))
#else
import qualified GHC_Data_FastString       as FS
import           HaddockUtils              (addConDoc)
#endif

#if MIN_VERSION_ghc(8,10,0)
import           GHC_Data_FastString       (bytesFS)
import           GHC_Hs_Extension          (noExtField)
#elif MIN_VERSION_ghc(8,6,0)
import           GHC_Data_FastString       (fastStringToByteString)
import           GHC_Hs_Extension          (noExt)
#else
import           PlaceHolder               (PlaceHolder (..), placeHolderType)
#endif

#if MIN_VERSION_ghc(8,8,0) && !MIN_VERSION_ghc(9,0,0)
import qualified GHC_Types_SrcLoc          as SrcLoc
#endif

#if MIN_VERSION_ghc(8,6,0)
import           GHC_Hs_Doc                (HsDocString,
                                            mkHsDocStringUtf8ByteString)
import           GHC_Types_Basic           (PprPrec (..), appPrec, funPrec,
                                            opPrec, sigPrec, topPrec)
#else
import           GHC_Hs_Doc                (HsDocString (..))
#endif

-- Internal
import           Language.Finkel.Builder
import           Language.Finkel.Form

mkRdrName :: FastString -> RdrName
mkRdrName = mkRdrName' tcName
{-# INLINABLE mkRdrName #-}

mkVarRdrName :: FastString -> RdrName
mkVarRdrName = mkRdrName' srcDataName
{-# INLINABLE mkVarRdrName #-}

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
{-# INLINABLE mkRdrName' #-}

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
    go "."      tmp acc = go [] ('.':tmp) acc
    go ('.':cs) tmp acc = go cs [] (('.':tmp) : acc)
    go (c:cs)   tmp acc = go cs (c:tmp) acc
{-# INLINABLE splitQualName #-}

checkVarId :: Code -> FastString -> Builder ()
checkVarId orig name =
  if isLexVar name
     then return ()
     else setLastToken orig >> failB "invalid variable identifier"
{-# INLINABLE checkVarId #-}

getConId :: Code -> Builder FastString
getConId orig@(LForm (L _ form)) =
  case form of
    Atom (ASymbol sym)
       -- `isLexVarSym' is for "TypeOperators" extension.
      | isLexCon sym    -> return sym
      | isLexVarSym sym -> return sym
    _ -> setLastToken orig >> failB "invalid constructor identifier"
{-# INLINABLE getConId #-}

getVarOrConId :: Code -> Builder FastString
getVarOrConId orig@(LForm (L _ form)) =
  case form of
    Atom (ASymbol sym)
      | isLexCon sym -> return sym
      | isLexVar sym -> return sym
    _ -> setLastToken orig >> failB "invalid identifier"
{-# INLINABLE getVarOrConId #-}

-- | Build 'HLocalBinds' from list of 'HDecl's.
declsToBinds :: SrcSpan -> [HDecl] -> HLocalBinds
declsToBinds l decls = L l binds'
  where
    binds' = case decls of
      [] -> emptyLocalBinds
      _  -> mkHsValBinds_compat (listToBag binds) sigs
    -- Using 'PostProcess.cvTopDecls' to group same names in where
    -- clause. Perhaps better to do similar things done in
    -- 'PostProcess.cvBindGroup', which is dedicated for 'P' monad ...
    decls' = PostProcess.cvTopDecls (toOL decls)
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
{-# INLINABLE mkLocatedList #-}

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
#if !MIN_VERSION_ghc(9,0,0)
cfld2ufld _ = error "Language.Finkel.Syntax.SynUtils:cfld2ufld"
#endif
#else
cfld2ufld (L l0 (HsRecField (L l1 (FieldOcc rdr _)) arg pun)) =
  L l0 (HsRecField (L l1 unambiguous) arg pun)
  where
    unambiguous = Unambiguous rdr PlaceHolder
#endif
{-# INLINABLE cfld2ufld #-}

-- | Make 'HsRecField' with given name and located data.
mkcfld :: (Located FastString, a) -> LHsRecField PARSED a
mkcfld (L nl name, e) =
  L nl HsRecField { hsRecFieldLbl = mkfname name
                  , hsRecFieldArg = e
                  , hsRecPun = False }
  where
    mkfname n = L nl (mkFieldOcc (L nl (mkRdrName n)))
{-# INLINABLE mkcfld #-}

quotedSourceText :: String -> SourceText
quotedSourceText s = SourceText $ "\"" ++ s ++ "\""
{-# INLINABLE quotedSourceText #-}

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
     case unP (fmap toCategorizedDecls (PostProcess.cvBindsAndSigs fb)) ps of
       POk _ cd -> return cd
       _        -> builderError

kindedTyVar :: Code -> Code -> HType -> Builder HTyVarBndr
kindedTyVar (LForm (L l _dc)) name kind =
  case name of
    LForm (L ln (Atom (ASymbol name'))) -> do
       let name'' = L ln (mkUnqual tvName name')
#if MIN_VERSION_ghc(9,0,0)
       return $! L l (KindedTyVar NOEXT () name'' kind)
#else
       return $! L l (KindedTyVar NOEXT name'' kind)
#endif
    _ -> builderError
{-# INLINABLE kindedTyVar #-}

kindedTyVarSpecific :: Code -> Code -> HType -> Builder HTyVarBndrSpecific
#if MIN_VERSION_ghc(9,0,0)
kindedTyVarSpecific (LForm (L l _dc)) name kind =
  case name of
    LForm (L ln (Atom (ASymbol name'))) -> do
       let name'' = L ln (mkUnqual tvName name')
       return $! L l (KindedTyVar NOEXT SpecifiedSpec name'' kind)
    _ -> builderError
#else
kindedTyVarSpecific = kindedTyVar
#endif
{-# INLINABLE kindedTyVarSpecific #-}

#if MIN_VERSION_ghc(9,0,0)
codeToUserTyVar :: Code -> LHsTyVarBndr () PARSED
codeToUserTyVar code =
  case code of
    LForm (L l (Atom (ASymbol name)))
     -> L l (UserTyVar NOEXT () (L l (mkUnqual tvName name)))
    _ -> error "Language.Finkel.Syntax.SynUtils:codeToUserTyVar"

codeToUserTyVarSpecific :: Code -> LHsTyVarBndr Specificity PARSED
codeToUserTyVarSpecific code =
  case code of
    LForm (L l (Atom (ASymbol name)))
      -- XXX: Does not support 'InferredSpec' yet.
      -> L l (UserTyVar NOEXT SpecifiedSpec (L l (mkUnqual tvName name)))
    _ -> error "Language.Finkel.Syntax.SynUtils:codeToUserTyVarSpecific"
#else
codeToUserTyVar :: Code -> HTyVarBndr
codeToUserTyVar code =
  case code of
    LForm (L l (Atom (ASymbol name)))
     -> L l (UserTyVar NOEXT (L l (mkUnqual tvName name)))
    _ -> error "Language.Finkel.Syntax.SynUtils:codeToUserTyVar"

codeToUserTyVarSpecific :: Code -> HTyVarBndrSpecific
codeToUserTyVarSpecific = codeToUserTyVar
#endif
{-# INLINABLE codeToUserTyVar #-}

-- | Auxiliary function to make 'HsDocString'.
hsDocString :: FastString -> HsDocString
#if MIN_VERSION_ghc(8,10,0)
hsDocString = mkHsDocStringUtf8ByteString . bytesFS
#elif MIN_VERSION_ghc(8,6,0)
hsDocString = mkHsDocStringUtf8ByteString . fastStringToByteString
#else
hsDocString = HsDocString
#endif
{-# INLINABLE hsDocString #-}

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
{-# INLINABLE mkHsIntegral_compat #-}

mkGRHSs :: [LGRHS PARSED t] -> [HDecl] -> SrcSpan -> GRHSs PARSED t
mkGRHSs grhss decls l = GRHSs NOEXT grhss (declsToBinds l decls)
{-# INLINABLE mkGRHSs #-}

mkHsValBinds_compat :: HBinds -> [HSig] -> HsLocalBindsLR PARSED PARSED
mkHsValBinds_compat binds sigs =
#if MIN_VERSION_ghc(8,6,0)
  HsValBinds NOEXT (ValBinds NOEXT binds sigs)
#else
  HsValBinds (ValBindsIn binds sigs)
#endif
{-# INLINABLE mkHsValBinds_compat #-}

mkHsQualTy_compat :: LHsContext PARSED -> HType -> HsType PARSED
mkHsQualTy_compat ctxt body
  | nullLHsContext ctxt = unLoc body
  | otherwise =
    HsQualTy { hst_ctxt = ctxt
#if MIN_VERSION_ghc(8,6,0)
             , hst_xqual = NOEXT
#endif
             , hst_body = body }
{-# INLINABLE mkHsQualTy_compat #-}

nullLHsContext :: LHsContext PARSED -> Bool
nullLHsContext (L _ cs) = null cs
{-# INLINABLE nullLHsContext #-}

addConDoc' :: Maybe LHsDocString -> LConDecl a -> LConDecl a
addConDoc' = flip addConDoc
{-# INLINABLE addConDoc' #-}

addConDoc'' :: LHsDocString -> LConDecl a -> LConDecl a
addConDoc'' = flip addConDoc . Just
{-# INLINABLE addConDoc'' #-}


-- For 8.8.0 compatibility in source code location management

-- #if MIN_VERSION_ghc(8,8,0)
#if MIN_VERSION_ghc(9,0,0)
dL :: Located a -> Located a
dL = id

cL :: SrcSpan -> a -> Located a
cL = L
#elif MIN_VERSION_ghc(8,8,0)
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

{-# INLINABLE cL #-}
{-# INLINABLE dL #-}

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

#if MIN_VERSION_ghc(9,0,0)
addConDoc :: LConDecl a -> Maybe LHsDocString -> LConDecl a
addConDoc decl    Nothing = decl
addConDoc (L p c) doc     = L p ( c { con_doc = con_doc c `mplus` doc } )
#endif

-- Version compatibility helper.
tailFS :: FastString -> FastString
#if MIN_VERSION_ghc(9,0,0)
tailFS = mkFastStringByteString . BS.tail . bytesFS
#else
tailFS = FS.tailFS
#endif
{-# INLINABLE tailFS #-}

consListWith :: [Code] -> String -> Code
consListWith rest sym =
  LForm (genSrc (List (LForm (genSrc (Atom (aSymbol sym))) : rest)))
{-# INLINABLE consListWith #-}
