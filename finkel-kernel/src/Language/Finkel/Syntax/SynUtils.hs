{-# LANGUAGE CPP #-}

-- | Utility codes for syntax.
module Language.Finkel.Syntax.SynUtils
  ( -- * This module
    module Language.Finkel.Syntax.SynUtils

    -- * Extension module
  , module Language.Finkel.Syntax.Extension

    -- * Location module
  , module Language.Finkel.Syntax.Location

#if MIN_VERSION_ghc(8,6,0)
     -- * Re-export for ghc version compatibility
  , PprPrec(..), topPrec, sigPrec, opPrec, funPrec, appPrec
#endif
  ) where

#include "Syntax.h"
#include "ghc_modules.h"

-- base
#if MIN_VERSION_ghc(9,0,0)
import           Control.Applicative              (Alternative (..))
#endif
import           Data.Char                        (isUpper)

-- bytestring
#if MIN_VERSION_ghc(9,0,0)
import qualified Data.ByteString.Char8            as BS
#endif

-- ghc
import           GHC_Hs_Decls                     (LConDecl, LDataFamInstDecl,
                                                   LDocDecl, LFamilyDecl,
                                                   LTyFamInstDecl)
import           GHC_Hs_Doc                       (LHsDocString)
import           GHC_Hs_Lit                       (HsOverLit (..))
import           GHC_Hs_Pat                       (LHsRecField, LHsRecUpdField)
import           GHC_Hs_Type                      (AmbiguousFieldOcc (..),
                                                   FieldOcc (..),
                                                   HsTyVarBndr (..),
                                                   HsType (..), LHsContext,
                                                   mkFieldOcc)
import           GHC_Hs_Utils                     (mkHsIntegral)

import           GHC_Builtin_Types                (consDataConName)
import           GHC_Data_OrdList                 (OrdList)
import           GHC_Parser_Lexer                 (P (..), ParseResult (..))
import qualified GHC_Parser_PostProcess           as PostProcess
import           GHC_Types_Name_Occurrence        (NameSpace, srcDataName,
                                                   tcName, tvName, varName)
import           GHC_Types_Name_Reader            (RdrName, mkQual, mkUnqual,
                                                   mkVarUnqual, nameRdrName)
import           GHC_Types_SrcLoc                 (GenLocated (..), Located,
                                                   unLoc)
import           GHC_Utils_Lexeme                 (isLexCon, isLexConSym,
                                                   isLexVar, isLexVarSym)

#if MIN_VERSION_ghc(9,10,0)
import           GHC.Parser.Annotation            (noAnnSrcSpan)
#elif MIN_VERSION_ghc(9,4,0)
import           GHC.Parser.Annotation            (SrcSpanAnn' (..), noComments)
#endif

#if MIN_VERSION_ghc(9,8,0)
import           Language.Haskell.Syntax.Type     (HsBndrVis (..))
#endif

#if MIN_VERSION_ghc(9,4,0)
import           GHC.Hs.Doc                       (LHsDoc)
import           GHC.Hs.Pat                       (HsFieldBind (..))
import           GHC.Parser                       (parseIdentifier)
import           GHC.Parser.HaddockLex            (lexHsDoc)
import           GHC.Types.SrcLoc                 (SrcSpan)
#else
import           GHC_Hs_Pat                       (HsRecField' (..))
#endif

#if MIN_VERSION_ghc(9,2,0)
import           GHC.Hs.Extension                 (GhcPass (..))
#endif

#if MIN_VERSION_ghc(9,0,0)
import           GHC_Data_FastString              (mkFastStringByteString)
import           GHC_Hs_Decls                     (ConDecl (..))
import           GHC_Hs_Type                      (LHsTyVarBndr)
import           GHC_Types_Var                    (Specificity (..))
#else
import qualified GHC_Data_FastString              as FS
import           HaddockUtils                     (addConDoc)
#endif

#if MIN_VERSION_ghc(8,10,0)
import           GHC_Data_FastString              (bytesFS)
#elif MIN_VERSION_ghc(8,6,0)
import           GHC_Data_FastString              (fastStringToByteString)
#else
import           PlaceHolder                      (PlaceHolder (..),
                                                   placeHolderType)
#endif

#if MIN_VERSION_ghc(8,6,0)
import           GHC_Hs_Decls                     (DerivStrategy (..))
#else
import           BasicTypes                       (DerivStrategy (..))
#endif

#if MIN_VERSION_ghc(9,4,0)
import           GHC.Hs.Doc                       (HsDocString (..),
                                                   mkHsDocStringChunkUtf8ByteString)
#elif MIN_VERSION_ghc(8,6,0)
import           GHC_Hs_Doc                       (HsDocString,
                                                   mkHsDocStringUtf8ByteString)
#endif

#if MIN_VERSION_ghc(8,6,0)
import           GHC_Types_Basic                  (PprPrec (..), appPrec,
                                                   funPrec, opPrec, sigPrec,
                                                   topPrec)
#else
import           GHC_Hs_Doc                       (HsDocString (..))
#endif

-- Internal
import           Language.Finkel.Builder
import           Language.Finkel.Data.FastString  (FastString, fsLit, unconsFS,
                                                   unpackFS)
import           Language.Finkel.Form
import           Language.Finkel.Syntax.Extension
import           Language.Finkel.Syntax.Location

-- ------------------------------------------------------------------------
--
-- Types
--
-- ------------------------------------------------------------------------

-- | An alias for record field to suuport named field puns and record wild
-- cards.
--
-- @Left form@ represent a record wild pattern with @form@ being the @..@
-- code form, @Right (fld, Nothing)@ means named field pun with @fld@ being the
-- punned field name, and @Right (fld, Just x)@ is a traditional @field = x@
-- style form.
type PreRecField a = Either Code (Located FastString, Maybe a)


-- ------------------------------------------------------------------------
--
-- Functions
--
-- ------------------------------------------------------------------------

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
  | nameStartsWith (== ':') = mkUnqual srcDataName name

  -- Names starting with capital letters might be qualified var names or
  -- data constructor names.
  | nameStartsWith isUpper =
    case splitQualName name of
      Nothing -> mkUnqual upperCaseNameSpace name
      Just q@(_, name') -> if isLexCon name'
                              then mkQual upperCaseNameSpace q
                              else mkQual varName q

  -- Variable.
  | otherwise = mkVarUnqual name
  where
    nameStartsWith test = case unconsFS name of
                            Just (x,_) -> test x
                            _          -> False
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
    go []       tmp acc = case concat acc of
      [] -> Nothing
      _:tl -> let mdl = reverse tl
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
     else setLastToken orig >> failB "Invalid variable identifier"
{-# INLINABLE checkVarId #-}

getConId :: Code -> Builder FastString
getConId orig@(LForm (L _ form)) =
  case form of
    Atom (ASymbol sym)
       -- `isLexVarSym' is for "TypeOperators" extension.
      | isLexCon sym    -> return sym
      | isLexVarSym sym -> return sym
    _ -> setLastToken orig >> failB "Invalid constructor identifier"
{-# INLINABLE getConId #-}

getLConId :: Code -> Builder (Located FastString)
getLConId orig@(LForm (L l _)) = fmap (L l) (getConId orig)
{-# INLINABLE getLConId #-}

getVarOrConId :: Code -> Builder FastString
getVarOrConId orig@(LForm (L _ form)) =
  case form of
    Atom (ASymbol sym)
      | isLexCon sym -> return sym
      | isLexVar sym -> return sym
    _ -> setLastToken orig >> failB "Invalid identifier"
{-# INLINABLE getVarOrConId #-}

-- | Convert record field constructor expression to record field update
-- expression.
cfld2ufld :: LHsRecField PARSED HExpr
#if MIN_VERSION_ghc(9,8,0)
          -> LHsRecUpdField PARSED PARSED
#else
          -> LHsRecUpdField PARSED
#endif
-- Almost same as 'mk_rec_upd_field' in 'RdrHsSyn'
#if MIN_VERSION_ghc(9,4,0)
cfld2ufld (L l0 (HsFieldBind _ann (L l1 (FieldOcc _ rdr)) rhs pun)) =
  L l0 (HsFieldBind NOEXT (L l1 (Unambiguous NOEXT rdr)) rhs pun)
#elif MIN_VERSION_ghc(9,2,0)
cfld2ufld (L l0 (HsRecField _ (L l1 (FieldOcc _ rdr)) arg pun)) =
  L l0 (HsRecField NOEXT (L l1 (Unambiguous NOEXT rdr)) arg pun)
#elif MIN_VERSION_ghc(8,6,0)
cfld2ufld (L l0 (HsRecField (L l1 (FieldOcc _ rdr)) arg pun)) =
  L l0 (HsRecField (L l1 unambiguous) arg pun)
  where
    unambiguous = Unambiguous NOEXT rdr
#  if !MIN_VERSION_ghc(9,0,0)
cfld2ufld _ = error "Language.Finkel.Syntax.SynUtils:cfld2ufld"
#  endif
#else
cfld2ufld (L l0 (HsRecField (L l1 (FieldOcc rdr _)) arg pun)) =
  L l0 (HsRecField (L l1 unambiguous) arg pun)
  where
    unambiguous = Unambiguous rdr PlaceHolder
#endif
{-# INLINABLE cfld2ufld #-}

-- | Make 'HsRecField' with given name and located data.
mkcfld :: Bool -> (Located FastString, a) -> LHsRecField PARSED a
mkcfld is_pun (L nl name, e) =
#if MIN_VERSION_ghc(9,10,0)
  lA nl HsFieldBind { hfbAnn = NOEXT
                    , hfbLHS = L (noAnnSrcSpan nl)
                                 (mkFieldOcc (lN nl (mkRdrName name)))
                    , hfbRHS = e
                    , hfbPun = is_pun }
#elif MIN_VERSION_ghc(9,4,0)
  lA nl HsFieldBind { hfbAnn = NOEXT
                    -- XXX: Not much sure below location is appropriate
                    , hfbLHS = L (SrcSpanAnn noComments nl)
                                 (mkFieldOcc (lN nl (mkRdrName name)))
                    , hfbRHS = e
                    , hfbPun = is_pun }
#else
  lA nl HsRecField { hsRecFieldLbl = mkfname name
#  if MIN_VERSION_ghc(9,2,0)
                   , hsRecFieldAnn = NOEXT
#  endif
                   , hsRecFieldArg = e
                   , hsRecPun = is_pun }
  where
    mkfname n = L nl (mkFieldOcc (lN nl (mkRdrName n)))
#endif
{-# INLINABLE mkcfld #-}

-- | Dummy name for named field puns. See: @GHC.Parser.PostProcess.pun_RDR@.
pun_RDR :: RdrName
pun_RDR = mkUnqual varName (fsLit "pun-right-hand-side")
{-# INLINABLE pun_RDR #-}

-- Following `cvBindsAndSigs`, `getMonoBind`, `has_args`, and
-- `makeFunBind` functions are based on resembling functions defined in
-- `RdrHsSyn` module in ghc package.
--
-- Unlike the original version, `cvBindsAndSigs` has pattern matches
-- for 'ValD' and 'SigD' only, and `getMonoBind` ignores 'DocD'
-- declarations.

#if MIN_VERSION_ghc(9,2,0)
type LDocDecl' a = LDocDecl a
#else
type LDocDecl' a = LDocDecl
#endif

data CategorizedDecls = CategorizedDecls
  { cd_binds :: HBinds
  , cd_sigs  :: [HSig]
  , cd_fds   :: [LFamilyDecl PARSED]
  , cd_tfis  :: [LTyFamInstDecl PARSED]
  , cd_dfis  :: [LDataFamInstDecl PARSED]
  , cd_docs  :: [LDocDecl' PARSED]
  }

toCategorizedDecls :: ( HBinds
                      , [HSig]
                      , [LFamilyDecl PARSED]
                      , [LTyFamInstDecl PARSED]
                      , [LDataFamInstDecl PARSED]
                      , [LDocDecl' PARSED] )
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

kindedTyVar :: Code -> Code -> HType -> Builder HTyVarBndrVis
kindedTyVar (LForm (L l _dc)) name kind =
  case name of
    LForm (L ln (Atom (ASymbol name'))) -> do
       let name'' = lN ln (mkUnqual tvName name')
#if MIN_VERSION_ghc(9,10,0)
       return $! lA l (KindedTyVar NOEXT (HsBndrRequired NOEXT) name'' kind)
#elif MIN_VERSION_ghc(9,8,0)
       return $! lA l (KindedTyVar NOEXT HsBndrRequired name'' kind)
#elif MIN_VERSION_ghc(9,0,0)
       return $! lA l (KindedTyVar NOEXT () name'' kind)
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
       let name'' = lN ln (mkUnqual tvName name')
       return $! lA l (KindedTyVar NOEXT SpecifiedSpec name'' kind)
    _ -> builderError
#else
kindedTyVarSpecific = kindedTyVar
#endif
{-# INLINABLE kindedTyVarSpecific #-}

#if MIN_VERSION_ghc(9,10,0)
codeToUserTyVar :: Code -> HTyVarBndrVis
codeToUserTyVar code =
  -- XXX: Always using HsBndrRequired.
  case code of
    LForm (L l (Atom (ASymbol name))) ->
      let bvis = HsBndrRequired NOEXT
      in lA l (UserTyVar NOEXT bvis (lN l (mkUnqual tvName name)))
    _ -> error "Language.Finkel.Syntax.SynUtils:codeToUserTyVar"
codeToUserTyVarSpecific :: Code -> LHsTyVarBndr Specificity PARSED
codeToUserTyVarSpecific code =
  case code of
    LForm (L l (Atom (ASymbol name))) ->
      lA l (UserTyVar NOEXT SpecifiedSpec (lN l (mkUnqual tvName name)))
      -- XXX: Does not support 'InferredSpec' yet.
    _ -> error "Language.Finkel.Syntax.SynUtils:codeToUserTyVarSpecific"
#elif MIN_VERSION_ghc(9,8,0)
codeToUserTyVar :: Code -> HTyVarBndrVis
codeToUserTyVar code =
  case code of
    LForm (L l (Atom (ASymbol name))) ->
      lA l (UserTyVar NOEXT HsBndrRequired (lN l (mkUnqual tvName name)))
    _ -> error "Language.Finkel.Syntax.SynUtils:codeToUserTyVar"
codeToUserTyVarSpecific :: Code -> LHsTyVarBndr Specificity PARSED
codeToUserTyVarSpecific code =
  -- XXX: Does not support 'InferredSpec' yet.
  case code of
    LForm (L l (Atom (ASymbol name))) ->
      lA l (UserTyVar NOEXT SpecifiedSpec (lN l (mkUnqual tvName name)))
    _ -> error "Language.Finkel.Syntax.SynUtils:codeToUserTyVarSpecific"
#elif MIN_VERSION_ghc(9,0,0)
codeToUserTyVar :: Code -> LHsTyVarBndr () PARSED
codeToUserTyVar code =
  case code of
    LForm (L l (Atom (ASymbol name))) ->
      lA l (UserTyVar NOEXT () (lN l (mkUnqual tvName name)))
    _ -> error "Language.Finkel.Syntax.SynUtils:codeToUserTyVar"

codeToUserTyVarSpecific :: Code -> LHsTyVarBndr Specificity PARSED
codeToUserTyVarSpecific code =
  case code of
    LForm (L l (Atom (ASymbol name))) ->
      lA l (UserTyVar NOEXT SpecifiedSpec (lN l (mkUnqual tvName name)))
      -- XXX: Does not support 'InferredSpec' yet.
    _ -> error "Language.Finkel.Syntax.SynUtils:codeToUserTyVarSpecific"
#else
codeToUserTyVar :: Code -> HTyVarBndr
codeToUserTyVar code =
  case code of
    LForm (L l (Atom (ASymbol name))) ->
      L l (UserTyVar NOEXT (L l (mkUnqual tvName name)))
    _ -> error "Language.Finkel.Syntax.SynUtils:codeToUserTyVar"

codeToUserTyVarSpecific :: Code -> HTyVarBndrSpecific
codeToUserTyVarSpecific = codeToUserTyVar
#endif
{-# INLINABLE codeToUserTyVar #-}
{-# INLINABLE codeToUserTyVarSpecific #-}

-- | Auxiliary function to make 'HsDocString'.
mkHsDocString :: FastString -> HsDocString
#if MIN_VERSION_ghc(9,4,0)
mkHsDocString = GeneratedDocString . mkHsDocStringChunkUtf8ByteString . bytesFS
#elif MIN_VERSION_ghc(8,10,0)
mkHsDocString = mkHsDocStringUtf8ByteString . bytesFS
#elif MIN_VERSION_ghc(8,6,0)
mkHsDocString = mkHsDocStringUtf8ByteString . fastStringToByteString
#else
mkHsDocString = HsDocString
#endif
{-# INLINABLE mkHsDocString #-}

#if MIN_VERSION_ghc(9,4,0)
lHsDocString2LHsDoc :: LHsDocString -> LHsDoc PARSED
lHsDocString2LHsDoc = fmap (lexHsDoc parseIdentifier)

mkLHsDoc :: SrcSpan -> FastString -> LHsDoc PARSED
mkLHsDoc l = lHsDocString2LHsDoc . L l . mkHsDocString
#else
lHsDocString2LHsDoc :: a -> a
lHsDocString2LHsDoc = id

mkLHsDoc :: a -> FastString -> HsDocString
mkLHsDoc _ = mkHsDocString
#endif
{-# INLINABLE lHsDocString2LHsDoc #-}
{-# INLINABLE mkLHsDoc #-}

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

mkHsQualTy_compat :: LHsContext PARSED -> HType -> HsType PARSED
mkHsQualTy_compat ctxt body
  | nullLHsContext ctxt = unLoc body
  | otherwise =
    HsQualTy { hst_ctxt = real_ctxt
#if MIN_VERSION_ghc(8,6,0)
             , hst_xqual = NOEXT
#endif
             , hst_body = body }
  where
#if MIN_VERSION_ghc(9,4,0)
    real_ctxt = ctxt
#elif MIN_VERSION_ghc(9,2,0)
    real_ctxt = Just ctxt
#else
    real_ctxt = ctxt
#endif
{-# INLINABLE mkHsQualTy_compat #-}

nullLHsContext :: LHsContext PARSED -> Bool
nullLHsContext (L _ cs) = null cs
{-# INLINABLE nullLHsContext #-}

#if MIN_VERSION_ghc(9,4,0)
addConDoc' :: Maybe LHsDocString -> LConDecl PARSED -> LConDecl PARSED
#else
addConDoc' :: Maybe LHsDocString -> LConDecl' a -> LConDecl' a
#endif
addConDoc' = flip addConDoc
{-# INLINABLE addConDoc' #-}

#if MIN_VERSION_ghc(9,4,0)
addConDoc'' :: LHsDocString -> LConDecl PARSED -> LConDecl PARSED
#else
addConDoc'' :: LHsDocString -> LConDecl' a -> LConDecl' a
#endif
addConDoc'' = flip addConDoc . Just
{-# INLINABLE addConDoc'' #-}

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

#if MIN_VERSION_ghc(9,2,0)
type LConDecl' a = LConDecl (GhcPass a)
#else
type LConDecl' a = LConDecl a
#endif

#if MIN_VERSION_ghc(9,4,0)
addConDoc :: LConDecl PARSED  -> Maybe LHsDocString -> LConDecl PARSED
addConDoc decl    Nothing        = decl
addConDoc (L p c) (Just ld) = L p (c {con_doc = con_doc c <|> doc'})
  where
    doc' = case ld of L l d -> Just (L l (lexHsDoc parseIdentifier d))
#elif MIN_VERSION_ghc(9,0,0)
addConDoc :: LConDecl' a -> Maybe LHsDocString -> LConDecl' a
addConDoc decl    Nothing = decl
addConDoc (L p c) doc     = L p (c {con_doc = con_doc c <|> doc})
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

fsSymbol :: Code -> Builder (Located FastString)
fsSymbol (LForm (L l x)) =
  case x of
    Atom (ASymbol sym) -> pure (L l sym)
    _                  -> builderError
{-# INLINABLE fsSymbol #-}

#if MIN_VERSION_ghc(8,6,0)
stockStrategy, anyclassStrategy, newtypeStrategy :: DerivStrategy PARSED
#else
stockStrategy, anyclassStrategy, newtypeStrategy :: DerivStrategy
#endif

#if MIN_VERSION_ghc(9,2,0)
stockStrategy = StockStrategy NOEXT
anyclassStrategy = AnyclassStrategy NOEXT
newtypeStrategy = NewtypeStrategy NOEXT
#else
stockStrategy = StockStrategy
anyclassStrategy = AnyclassStrategy
newtypeStrategy = NewtypeStrategy
#endif

{-# INLINABLE stockStrategy #-}
{-# INLINABLE anyclassStrategy #-}
{-# INLINABLE newtypeStrategy #-}
