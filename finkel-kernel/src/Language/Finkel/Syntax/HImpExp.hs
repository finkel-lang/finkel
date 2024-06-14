{-# LANGUAGE CPP          #-}
{-# LANGUAGE TypeFamilies #-}

-- | Syntax for module header, import and export entities.
module Language.Finkel.Syntax.HImpExp where

#include "Syntax.h"
#include "ghc_modules.h"

-- ghc
import GHC_Data_FastString              (FastString, unpackFS)
import GHC_Data_OrdList                 (toOL)
import GHC_Hs                           (HsModule (..))
import GHC_Hs_Doc                       (LHsDocString)
import GHC_Hs_ImpExp                    (IE (..), IEWildcard (..),
                                         IEWrappedName (..), ImportDecl (..),
                                         simpleImportDecl)
import GHC_Parser_PostProcess           (cvTopDecls)
import GHC_Types_Name_Occurrence        (tcClsName)
import GHC_Types_Name_Reader            (RdrName, mkQual, mkUnqual)
import GHC_Types_SrcLoc                 (GenLocated (..), SrcSpan)
import GHC_Unit_Module                  (mkModuleNameFS)
import GHC_Utils_Lexeme                 (isLexCon)
import GHC_Hs_ImpExp                    (ImportDeclQualifiedStyle (..))

#if !MIN_VERSION_ghc(9,10,0) && MIN_VERSION_ghc(9,6,0)
import Language.Haskell.Syntax.Concrete (LayoutInfo (..))
#elif !MIN_VERSION_ghc(9,10,0) && MIN_VERSION_ghc(9,0,0)
import GHC_Types_SrcLoc                 (LayoutInfo (..))
#endif

#if MIN_VERSION_ghc(9,6,0)
import GHC.Hs                           (XModulePs (..))
import Language.Haskell.Syntax.ImpExp   (ImportListInterpretation (..))
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

type ModFn = Maybe LHsDocString -> [HImportDecl] -> [HDecl] -> HModule

b_module :: Maybe Code -> [HIE] -> Builder ModFn
b_module mb_form exports =
  case mb_form of
    Nothing -> return (modfn Nothing)
    Just (LForm (L l form)) | Atom (ASymbol name) <- form
      -> return (modfn (Just (lA l (mkModuleNameFS name))))
    _ -> builderError
  where
    modfn mb_name mbdoc imports decls =
      HsModule { hsmodName = mb_name
               , hsmodExports = if null exports
                                   then Nothing
#if MIN_VERSION_ghc(9,10,0)
                                   else Just (mkLocatedListA exports)
#else
                                   else Just (la2la (mkLocatedListA exports))
#endif
               , hsmodImports = imports
               -- Function `cvTopDecls' is used for mergeing multiple top-level
               -- FunBinds, which may take different patterns in its arguments.
               , hsmodDecls = cvTopDecls (toOL decls)
#if MIN_VERSION_ghc(9,6,0)
               , hsmodExt = XModulePs
                   { hsmodAnn = NOEXT
#  if MIN_VERSION_ghc(9,10,0)
                   , hsmodLayout = unused
#  else
                   , hsmodLayout = NoLayoutInfo
#  endif
                   , hsmodDeprecMessage = Nothing
                   , hsmodHaddockModHeader = fmap lHsDocString2LHsDoc mbdoc
                   }
#else
               -- XXX: Does not support DEPRECATED message.
               , hsmodDeprecMessage = Nothing
#  if MIN_VERSION_ghc(9,2,0)
               , hsmodAnn = NOEXT
#  endif
#  if MIN_VERSION_ghc(9,0,0)
               , hsmodLayout = NoLayoutInfo
#  endif
               , hsmodHaddockModHeader = fmap lHsDocString2LHsDoc mbdoc
#endif
               }
{-# INLINABLE b_module #-}

b_implicitMainModule :: Builder ([HImportDecl] -> [HDecl] -> HModule)
b_implicitMainModule = b_module Nothing [] <*> pure Nothing
{-# INLINABLE b_implicitMainModule #-}

b_ieSym :: Code -> Builder HIE
b_ieSym form@(LForm (L l _)) = do
  name <- getVarOrConId form
  let con = iEThingAbs l
#if MIN_VERSION_ghc(9,10,0)
  let var x = lA l (IEVar Nothing (lA l (ieName l (mkRdrName x))) Nothing)
#elif MIN_VERSION_ghc(9,8,0)
  let var x = lA l (IEVar Nothing (lA l (ieName l (mkRdrName x))))
#else
  let var x = lA l (IEVar NOEXT (lA l (ieName l (mkRdrName x))))
#endif
  pure (if isLexCon name
          then con name
          else var name)
{-# INLINABLE b_ieSym #-}

b_ieGroup :: Int -> Code -> Builder HIE
b_ieGroup n form@(LForm (L l body))
  | List [_, doc_code] <- body
  , Atom (AString _ doc) <- unCode doc_code
  = return $! lA l (IEGroup NOEXT (fromIntegral n) (mkLHsDoc l doc))
  | otherwise
  = setLastToken form >> failB "Invalid group documentation"
{-# INLINABLE b_ieGroup #-}

b_ieDoc :: Code -> Builder HIE
b_ieDoc (LForm (L l form)) =
  case form of
    Atom (AString _ str) -> return $! lA l (IEDoc NOEXT (mkLHsDoc l str))
    _                    -> builderError
{-# INLINABLE b_ieDoc #-}

b_ieDocNamed :: Code -> Builder HIE
b_ieDocNamed (LForm (L l form))
  | List [_,name_code] <- form
  , Atom (ASymbol name) <- unCode name_code
  = return $! lA l (IEDocNamed NOEXT (unpackFS name))
  | otherwise = builderError
{-# INLINABLE b_ieDocNamed #-}

b_ieAbs :: Code -> Builder HIE
b_ieAbs form@(LForm (L l _)) = iEThingAbs l <$> getConId form
{-# INLINABLE b_ieAbs #-}

b_ieAll :: Code -> Builder HIE
b_ieAll form@(LForm (L l _)) = do
  name <- getConId form
#if MIN_VERSION_ghc(9,10,0)
  -- XXX: Does not support ExportDoc.
  let iEThingAll ie_name = IEThingAll (Nothing, NOEXT) ie_name Nothing
#elif MIN_VERSION_ghc(9,8,0)
  let iEThingAll = IEThingAll (Nothing, NOEXT)
#else
  let iEThingAll = IEThingAll NOEXT
#endif
  return $ lA l (iEThingAll (lA l (ieName l (mkUnqual tcClsName name))))
{-# INLINABLE b_ieAll #-}

b_ieWith :: Code -> [Code] -> Builder HIE
b_ieWith (LForm (L l form)) names =
  case form of
    Atom (ASymbol name) -> return (thing name)
    _                   -> builderError
  where
#if MIN_VERSION_ghc(9,10,0)
    -- XXX: Does not support ExportDoc.
    thing name = lA l (iEThingWith (wrapped name) wc ns Nothing)
#elif MIN_VERSION_ghc(9,2,0)
    -- XXX: Does not support DuplicateRecordFields.
    thing name = lA l (iEThingWith (wrapped name) wc ns)
#else
    thing name = L l (iEThingWith (wrapped name) wc ns _fs)
#endif
    wrapped name = lA l (ieName l (qn name))
    qn name =
      maybe (mkUnqual tcClsName name) (mkQual tcClsName) (splitQualName name)
    (ns, _fs) = foldr f ([],[]) names
    f (LForm (L l0 (Atom (ASymbol n0)))) (ns0, fs0) =
      (lA l0 (ieName l (mkRdrName n0)) : ns0, fs0)
    f _ acc = acc
#if MIN_VERSION_ghc(9,8,0)
    iEThingWith = IEThingWith (Nothing, NOEXT)
#else
    iEThingWith = IEThingWith NOEXT
#endif
    wc = NoIEWildcard
{-# INLINABLE b_ieWith #-}

b_ieMdl :: [Code] -> Builder HIE
b_ieMdl xs =
  case xs of
    [LForm (L l (Atom (ASymbol name)))] -> return (thing l name)
    _                                   -> builderError
  where
    thing l n = lA l (iEModuleContents (lA l (mkModuleNameFS n)))
#if MIN_VERSION_ghc(9,8,0)
    iEModuleContents = IEModuleContents (Nothing, NOEXT)
#else
    iEModuleContents = IEModuleContents NOEXT
#endif
{-# INLINABLE b_ieMdl #-}

b_importD :: (Code, Bool, Maybe Code) -> (Bool, Maybe [HIE])
          -> Builder HImportDecl
b_importD (name, qualified, mb_as) (hiding, mb_entities) =
  case name of
    LForm (L l (Atom (ASymbol m))) ->
      let decl = simpleImportDecl mname
          decl' = decl { ideclQualified = qualified'
                       , ideclAs = fmap asModName mb_as
                       , ideclName = lA l mname
#if MIN_VERSION_ghc(9,6,0)
                       , ideclImportList = hiding'
#else
                       , ideclHiding = hiding'
#endif
                       }
          mname = mkModuleNameFS m
          qualified' | qualified = QualifiedPre
                     | otherwise = NotQualified
          asModName (LForm (L l' (Atom (ASymbol x)))) =
            lA l' (mkModuleNameFS x)
          asModName _ = error "b_importD.asModName"
          hiding' =
            case mb_entities of
              Nothing -> Nothing
              Just es -> Just (interp, lL l es)
                where
#if MIN_VERSION_ghc(9,6,0)
                  interp = if hiding then EverythingBut else Exactly
#else
                  interp = hiding
#endif
      in  return (lA l decl')
    _ -> builderError
{-# INLINABLE b_importD #-}


-- ------------------------------------------------------------------------
--
-- Auxiliary
--
-- ------------------------------------------------------------------------

iEThingAbs :: SrcSpan -> FastString -> HIE
iEThingAbs l name =
#if MIN_VERSION_ghc(9,10,0)
  -- XXX: Does not support ExportDoc.
  lA l (IEThingAbs (Nothing, NOEXT) (lA l (ieName l (mkUnqual tcClsName name)))
                   Nothing)
#elif MIN_VERSION_ghc(9,8,0)
  lA l (IEThingAbs (Nothing, NOEXT) (lA l (ieName l (mkUnqual tcClsName name))))
#else
  lA l (IEThingAbs NOEXT (lA l (ieName l (mkUnqual tcClsName name))))
#endif
{-# INLINABLE iEThingAbs #-}

#if MIN_VERSION_ghc(9,6,0)
ieName :: SrcSpan -> RdrName -> IEWrappedName PARSED
#else
ieName :: SrcSpan -> RdrName -> IEWrappedName RdrName
#endif
#if MIN_VERSION_ghc(9,6,0)
ieName l x = IEName NOEXT (lN l x)
#else
ieName l x = IEName (lN l x)
#endif
