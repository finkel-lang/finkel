{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
-- | Syntax for module header, import and export entities.
module Language.SK.Syntax.HIE where

-- base
import Data.Char (isUpper)

-- ghc
import FastString (headFS)
import FieldLabel (FieldLbl(..))
import HsDoc (LHsDocString)
import HsImpExp (IE(..), IEWildcard(..), IEWrappedName(..), ImportDecl(..)
                , simpleImportDecl )
import HsSyn (HsModule(..))
import Module (mkModuleNameFS)
import OccName (tcName)
import OrdList (toOL)
import RdrHsSyn (cvTopDecls)
import RdrName (mkQual, mkUnqual)
import SrcLoc (noLoc)

#if MIN_VERSION_ghc(8,6,0)
import HsExtension (noExt)
#endif

-- Internal
import Language.SK.Builder
import Language.SK.Form

#include "Syntax.h"

-- ---------------------------------------------------------------------
--
-- Module
--
-- ---------------------------------------------------------------------

-- In GHC source code, there is a file "compiler/hsSyn/Convert.hs".
-- This module contains codes converting Template Haskell data types to
-- GHC's internal data type, which is a helpful resource for
-- understanding the values and types for constructing Haskell AST data.

b_module :: Code -> [HIE] ->  Maybe LHsDocString -> [HImportDecl]
         -> [HDecl] -> HModule
b_module form exports mbdoc imports decls =
  HsModule { hsmodName = Just (L l (mkModuleNameFS name))
           , hsmodExports = exports'
           , hsmodImports = imports
           -- Function `cvTopDecls' is used for mergeing multiple
           -- top-level FunBinds, which possibly taking different
           -- patterns in its arguments.
           , hsmodDecls = cvTopDecls (toOL decls)
           , hsmodDeprecMessage = Nothing
           , hsmodHaddockModHeader = mbdoc }
  where
    LForm (L l (Atom (ASymbol name))) = form
    exports'
      | null exports = Nothing
      | otherwise    = Just (L l exports)
{-# INLINE b_module #-}

b_implicitMainModule :: [HImportDecl] -> [HDecl] -> HModule
b_implicitMainModule =
  b_module (LForm (noLoc (Atom (aSymbol "Main")))) [] Nothing
{-# INLINE b_implicitMainModule #-}

b_ieSym :: Code -> HIE
b_ieSym (LForm (L l (Atom (ASymbol name)))) = thing
  where
    thing = L l (iEVar (L l (IEName (L l (mkRdrName name)))))
    iEVar = IEVar NOEXT
{-# INLINE b_ieSym #-}

b_ieAbs :: Code -> HIE
b_ieAbs (LForm (L l (Atom (ASymbol name)))) = thing
  where
    thing = L l (iEThingAbs (L l (IEName (L l (mkUnqual tcName name)))))
    iEThingAbs = IEThingAbs NOEXT
{-# INLINE b_ieAbs #-}

b_ieAll :: Code -> HIE
b_ieAll (LForm (L l (Atom (ASymbol name)))) = thing
  where
    thing = L l (iEThingAll (L l (IEName (L l (mkUnqual tcName name)))))
    iEThingAll = IEThingAll NOEXT
{-# INLINE b_ieAll #-}

b_ieWith :: Code -> [Code] -> HIE
b_ieWith (LForm (L l (Atom (ASymbol name)))) names = thing
  where
    thing = L l (iEThingWith (L l (IEName (L l name'))) wc ns fs)
    name' = case splitQualName name of
              Just qual -> mkQual tcName qual
              Nothing   -> mkUnqual tcName name
    wc = NoIEWildcard
    (ns, fs) = foldr f ([],[]) names
    f (LForm (L l0 (Atom (ASymbol n0)))) (ns0, fs0)
      | isUpper c || c == ':' =
        (L l0 (IEName (L l (mkUnqual tcName n0))) : ns0, fs0)
      | otherwise             = (ns0, L l0 (fl n0) : fs0)
      where
        c = headFS n0
    -- Does not support DuplicateRecordFields.
    fl x = FieldLabel { flLabel = x
                      , flIsOverloaded = False
                      , flSelector = mkRdrName x }
    iEThingWith = IEThingWith NOEXT
{-# INLINE b_ieWith #-}

b_ieMdl :: [Code] -> HIE
b_ieMdl [LForm (L l (Atom (ASymbol name)))] = L l thing
  where
    thing = iEModuleContents (L l (mkModuleNameFS name))
    iEModuleContents = IEModuleContents NOEXT
{-# INLINE b_ieMdl #-}

b_importD :: (Code, Bool, Maybe Code) -> (Bool, Maybe [HIE])
          -> HImportDecl
b_importD (name, qualified, mb_as) (hiding, mb_entities) =
  case name of
    LForm (L l (Atom (ASymbol m))) ->
      let decl = simpleImportDecl (mkModuleNameFS m)
          decl' = decl { ideclQualified = qualified
                       , ideclAs = fmap asModName mb_as
                       , ideclHiding = hiding' }
          asModName (LForm (L l' (Atom (ASymbol x)))) =
            L l' (mkModuleNameFS x)
          hiding' =
            case mb_entities of
              Nothing       -> Nothing
              Just entities -> Just (hiding, L l entities)
      in  L l decl'
{-# INLINE b_importD #-}

b_isAs :: Code -> Builder Code
b_isAs form =
  case form of
    LForm (L _ (Atom (ASymbol as))) | as == fsLit "as" -> return form
    _                               -> builderError
{-# INLINE b_isAs #-}
