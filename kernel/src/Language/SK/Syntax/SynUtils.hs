{-# LANGUAGE CPP #-}
-- | Utility codes for syntax.
module Language.SK.Syntax.SynUtils where

-- base
import Data.Char (isUpper)

-- ghc
import Bag (consBag, emptyBag, listToBag)
import BasicTypes ( SourceText(..)
#if MIN_VERSION_ghc (8,4,0)
                  , IntegralLit(..)
#endif
                  )
import FastString (FastString, fsLit, headFS, nullFS, unpackFS)
import HsBinds ( HsBindLR(..), HsLocalBindsLR(..), HsValBindsLR(..)
               , emptyLocalBinds )
import HsDecls (HsDecl(..))
import HsExpr ( GRHSs(..), LGRHS, LHsExpr, LMatch, Match(..)
              , MatchGroup(..) )
import HsLit (HsOverLit(..))
import HsPat ( HsRecField'(..), LHsRecField
             , LHsRecUpdField )
import HsTypes ( AmbiguousFieldOcc(..), FieldOcc(..), LHsContext
               , HsTyVarBndr(..), HsType(..), mkFieldOcc )
import HsUtils (mkFunBind, mkHsIntegral)
import OccName (NameSpace, srcDataName, tcName, tvName, varName)
import OrdList (OrdList, fromOL, toOL)
import RdrHsSyn (cvTopDecls)
import RdrName (RdrName, mkQual, mkUnqual, mkVarUnqual, nameRdrName)
import SrcLoc ( GenLocated(..), Located, SrcSpan, combineLocs
              , combineSrcSpans, noLoc )
import TysWiredIn (consDataConName)

#if MIN_VERSION_ghc(8,6,0)
import HsDoc (HsDocString, mkHsDocString)
import HsExtension (noExt)
#else
import HsDoc (HsDocString(..))
import PlaceHolder (PlaceHolder(..), placeHolderType)
#endif

-- ghc-boot-th
import GHC.Lexeme (isVarSymChar, startsVarId, startsVarSym)

-- Internal
import Language.SK.Builder
import Language.SK.Form

#include "Syntax.h"

-- ---------------------------------------------------------------------
--
-- Auxiliary
--
-- ---------------------------------------------------------------------

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
         | isUpper y || y == ':' -> mkQual upperCaseNameSpace q
         | otherwise             -> mkQual varName q
         where
           y = headFS name'

  -- Variable.
  | otherwise = mkVarUnqual name
  where
    x = headFS name
{-# INLINE mkRdrName' #-}

splitQualName :: FastString -> Maybe (FastString, FastString)
splitQualName fstr = go (unpackFS fstr) "" []
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

getVarId :: Code -> Builder FastString
getVarId orig@(LForm (L _ form))
  | Atom (ASymbol sym) <- form, isVarId sym = return sym
  | otherwise = do
    setLastToken orig
    failB "invalid variable identifier"
{-# INLINE getVarId #-}

getConId :: Code -> Builder FastString
getConId orig@(LForm (L _ form))
  | Atom (ASymbol sym) <- form, isConId sym = return sym
  | otherwise = do
    setLastToken orig
    failB "invalid constructor identifier"
{-# INLINE getConId #-}

getVarOrConId :: Code -> Builder FastString
getVarOrConId orig@(LForm (L _ form))
  | Atom (ASymbol sym) <- form, isConId sym || isVarId sym = return sym
  | otherwise = do
    setLastToken orig
    failB "invalid identifier"
{-# INLINE getVarOrConId #-}

-- | 'True' when the given 'FastString' is a variable identifier.
isVarId :: FastString -> Bool
isVarId fs =
  case (if nullFS fs then [] else unpackFS fs) of
    []     -> False
    (c:cs) | startsVarSym c -> all isVarSymChar cs
           | startsVarId c  -> all (not . isVarSymChar) cs
           | otherwise      -> False
{-# INLINE isVarId #-}

-- | 'True' when the given 'FastString' is a constructor identifier.
isConId :: FastString -> Bool
isConId fs =
  case (if nullFS fs then [] else unpackFS fs) of
    []     -> False
    (c:cs) | isUpper c -> all (not . isVarSymChar) cs
           | c == ':'  -> all isVarSymChar cs
           | otherwise -> False
{-# INLINE isConId #-}

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
cfld2ufld _ = error "Language.SK.Syntax.SynUtils:cfld2ufld"
#else
cfld2ufld (L l0 (HsRecField (L l1 (FieldOcc rdr _)) arg pun)) =
  L l0 (HsRecField (L l1 unambiguous) arg pun)
  where
    unambiguous = Unambiguous rdr PlaceHolder
#endif
{-# INLINE cfld2ufld #-}

-- | Make 'HsRecField' with given name and located data.
mkcfld :: (Located FastString, Located a) -> LHsRecField PARSED (Located a)
mkcfld ((L nl name), e@(L fl _)) =
  L fl HsRecField { hsRecFieldLbl = mkfname name
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
-- `RdrHsSyn` module in ghc package, since these functions were not
-- exported.
--
-- Unlike the original version, `cvBindsAndSigs` has pattern matches
-- for 'ValD' and 'SigD' only, and `getMonoBind` ignores 'DocD'
-- declarations.

cvBindsAndSigs :: OrdList HDecl -> (HBinds, [HSig])
cvBindsAndSigs fb = go (fromOL fb)
  where
    go [] = (emptyBag, [])
    go (L l (ValD _EXT d) : ds)
      = let (b', ds') = getMonoBind (L l d) ds
            (bs, ss) = go ds'
        in  (b' `consBag` bs, ss)
    go (L l (SigD _EXT s) : ds)
      = let (bs, ss) = go ds
        in  (bs, L l s : ss)

    -- XXX: Ignoring  other constructors.
    go (_ : ds) = go ds

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
has_args []           = error "Language.SK.Syntax.SynUtils:has_args"

codeToUserTyVar :: Code -> HTyVarBndr
codeToUserTyVar code =
  case code of
    LForm (L l (Atom (ASymbol name))) ->
      L l (UserTyVar NOEXT (L l (mkUnqual tvName name)))
    _ -> error "Language.SK.Syntax.SynUtils:codeToUserTyVar"
{-# INLINE codeToUserTyVar #-}

-- | Auxiliary function to make 'HsDocString'.
hsDocString :: String -> HsDocString
#if MIN_VERSION_ghc(8,6,0)
hsDocString = mkHsDocString
#else
hsDocString = HsDocString . fsLit
#endif
{-# INLINE hsDocString #-}

-- | Auxiliary function to absorb version compatibiity of
-- 'mkHsIntegral'.
mkHsIntegral_compat :: Integer -> HsOverLit PARSED
mkHsIntegral_compat n =
#if MIN_VERSION_ghc(8,6,0)
  let il = IL { il_text = SourceText (show n)
              , il_neg = n < 0
              , il_value = n }
  in  mkHsIntegral il
#elif MIN_VERSION_ghc(8,4,0)
  let il = IL { il_text = SourceText (show n)
              , il_neg = n < 0
              , il_value = n }
  in  mkHsIntegral il placeHolderType
#else
  mkHsIntegral (SourceText (show n)) n placeHolderType
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

mkHsQualTy_compat :: LHsContext PARSED -> HType -> HsType PARSED
mkHsQualTy_compat ctxt body =
  HsQualTy { hst_ctxt = ctxt
#if MIN_VERSION_ghc(8,6,0)
           , hst_xqual = noExt
#endif
           , hst_body = body }
