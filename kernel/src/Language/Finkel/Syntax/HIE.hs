{-# LANGUAGE CPP          #-}
{-# LANGUAGE TypeFamilies #-}
-- | Syntax for module header, import and export entities.
module Language.Finkel.Syntax.HIE where

#include "Syntax.h"

-- ghc
import FastString                      (FastString, unpackFS)
import FieldLabel                      (FieldLbl (..))
import GHC_Hs                          (HsModule (..))
import GHC_Hs_Doc                      (LHsDocString)
import GHC_Hs_ImpExp                   (IE (..), IEWildcard (..),
                                        IEWrappedName (..), ImportDecl (..),
                                        simpleImportDecl)
import Lexeme                          (isLexCon)
import Module                          (mkModuleNameFS)
import OccName                         (tcClsName)
import OrdList                         (toOL)
import RdrHsSyn                        (cvTopDecls)
import RdrName                         (mkQual, mkUnqual)
import SrcLoc                          (GenLocated (..), SrcSpan)

#if MIN_VERSION_ghc(8,10,0)
import GHC_Hs_Extension                (noExtField)
import GHC_Hs_ImpExp                   (ImportDeclQualifiedStyle (..))
#elif MIN_VERSION_ghc(8,6,0)
import GHC_Hs_Extension                (noExt)
#endif

-- Internal
import Language.Finkel.Builder
import Language.Finkel.Form
import Language.Finkel.Syntax.SynUtils

-- ---------------------------------------------------------------------
--
-- Module
--
-- ---------------------------------------------------------------------

-- In GHC source code, there is a file "compiler/hsSyn/Convert.hs".
-- This module contains codes converting Template Haskell data types to
-- GHC's internal data type, which is a helpful resource for
-- understanding the values and types for constructing Haskell AST data.

type ModFn =
  Maybe LHsDocString -> [HImportDecl] -> [HDecl] -> HModule

b_module :: Maybe Code -> [HIE] -> Builder ModFn
b_module mb_form exports =
  case mb_form of
    Nothing -> return (modfn Nothing)
    Just (LForm (L l form)) | Atom (ASymbol name) <- form
      -> return (modfn (Just (L l (mkModuleNameFS name))))
    _ -> builderError
  where
    modfn mb_name mbdoc imports decls =
      HsModule { hsmodName = mb_name
               , hsmodExports = if null exports
                                   then Nothing
                                   else Just (mkLocatedList  exports)
               , hsmodImports = imports
               -- Function `cvTopDecls' is used for mergeing
               -- multiple top-level FunBinds, which possibly
               -- taking different patterns in its arguments.
               , hsmodDecls = cvTopDecls (toOL decls)
               , hsmodDeprecMessage = Nothing
               , hsmodHaddockModHeader = mbdoc }
{-# INLINE b_module #-}

b_implicitMainModule :: Builder ([HImportDecl] -> [HDecl] -> HModule)
b_implicitMainModule = b_module Nothing [] <*> pure Nothing
{-# INLINE b_implicitMainModule #-}

b_ieSym :: Code -> Builder HIE
b_ieSym form@(LForm (L l _)) = do
  name <- getVarOrConId form
  let var x = L l (IEVar NOEXT (L l (IEName (L l (mkRdrName x)))))
      con x = iEThingAbs l x
  if isLexCon name
     then return (con name)
     else return (var name)
{-# INLINE b_ieSym #-}

b_ieGroup :: Int -> Code -> Builder HIE
b_ieGroup n form@(LForm (L l body))
  | List [_, doc_code] <- body
  , Atom (AString _ doc) <- unCode doc_code
  = return $! L l (IEGroup NOEXT (fromIntegral n) (hsDocString doc))
  | otherwise
  = setLastToken form >> failB "Invalid group documentation"
{-# INLINE b_ieGroup #-}

b_ieDoc :: Code -> Builder HIE
b_ieDoc (LForm (L l form)) =
  case form of
    Atom (AString _ str) -> return $! L l (IEDoc NOEXT (hsDocString str))
    _                    -> builderError
{-# INLINE b_ieDoc #-}

b_ieDocNamed :: Code -> Builder HIE
b_ieDocNamed (LForm (L l form))
  | List [_,name_code] <- form
  , Atom (ASymbol name) <- unCode name_code
  = return $! L l (IEDocNamed NOEXT (unpackFS name))
  | otherwise = builderError
{-# INLINE b_ieDocNamed #-}

b_ieAbs :: Code -> Builder HIE
b_ieAbs form@(LForm (L l _)) = iEThingAbs l <$> getConId form
{-# INLINE b_ieAbs #-}

b_ieAll :: Code -> Builder HIE
b_ieAll form@(LForm (L l _)) = do
  name <- getConId form
  let thing = L l (iEThingAll (L l (IEName (L l (uq name)))))
      iEThingAll = IEThingAll NOEXT
      uq = mkUnqual tcClsName
  return thing
{-# INLINE b_ieAll #-}

b_ieWith :: Code -> [Code] -> Builder HIE
b_ieWith (LForm (L l form)) names =
  case form of
    Atom (ASymbol name) -> return (thing name)
    _                   -> builderError
  where
    thing name = L l (iEThingWith (L l (IEName (L l name'))) wc ns fs)
      where
        name' = case splitQualName name of
                  Just qual -> mkQual tcClsName qual
                  Nothing   -> mkUnqual tcClsName name
    wc = NoIEWildcard
    (ns, fs) = foldr f ([],[]) names
    f (LForm (L l0 (Atom (ASymbol n0)))) (ns0, fs0) =
      if isLexCon n0
         then (L l0 (IEName (L l (mkUnqual tcClsName n0))) : ns0, fs0)
         else (ns0, L l0 (fl n0) : fs0)
    f _ acc = acc
    -- XXX: Does not support DuplicateRecordFields.
    fl x = FieldLabel { flLabel = x
                      , flIsOverloaded = False
                      , flSelector = mkRdrName x }
    iEThingWith = IEThingWith NOEXT
{-# INLINE b_ieWith #-}

b_ieMdl :: [Code] -> Builder HIE
b_ieMdl xs =
  case xs of
    [LForm (L l (Atom (ASymbol name)))] -> return (thing l name)
    _                                   -> builderError
  where
    thing l n = L l (iEModuleContents (L l (mkModuleNameFS n)))
    iEModuleContents = IEModuleContents NOEXT
{-# INLINE b_ieMdl #-}

b_importD :: (Code, Bool, Maybe Code) -> (Bool, Maybe [HIE])
          -> Builder HImportDecl
b_importD (name, qualified, mb_as) (hiding, mb_entities) =
  case name of
    LForm (L l (Atom (ASymbol m))) ->
      let decl = simpleImportDecl (mkModuleNameFS m)
          decl' = decl { ideclQualified = qualified'
                       , ideclAs = fmap asModName mb_as
                       , ideclHiding = hiding' }
#if MIN_VERSION_ghc(8,10,0)
          qualified' | qualified = QualifiedPre
                     | otherwise = NotQualified
#else
          qualified' = qualified
#endif
          asModName (LForm (L l' (Atom (ASymbol x)))) =
            L l' (mkModuleNameFS x)
          asModName _ = error "b_importD.asModName"
          hiding' =
            case mb_entities of
              Nothing       -> Nothing
              Just entities -> Just (hiding, L l entities)
            in  return (L l decl')
    _ -> builderError
{-# INLINE b_importD #-}


-- ------------------------------------------------------------------------
--
-- Auxiliary
--
-- ------------------------------------------------------------------------

iEThingAbs :: SrcSpan -> FastString -> HIE
iEThingAbs l name =
  L l (IEThingAbs NOEXT (L l (IEName (L l (mkUnqual tcClsName name)))))
{-# INLINE iEThingAbs #-}
