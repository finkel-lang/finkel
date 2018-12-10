-- | Module exporting the @runSkc@ function, and some utilities.
module Language.SK.Run
  ( runSkc
  , compileSkModule
  , compileSkModuleForm
  , compileWithSymbolConversion
  , getDynFlagsFromSPState
  , setDynFlagsFromSPState
  , parseSexprs
  , buildHsSyn
  , macroFunction
  , mkModSummary
  , mkModSummary'
  , tcHsModule
  ) where

-- base
import Control.Monad.IO.Class (MonadIO(..))
import Data.Maybe (fromMaybe, maybeToList)

-- bytestring
import qualified Data.ByteString.Lazy as BL

-- containers
import qualified Data.Map as Map

-- ghc
import DynFlags ( DynFlags(..), GhcLink(..), HscTarget(..)
                , getDynFlags, parseDynamicFilePragma, thisPackage )
import DriverPhases (HscSource(..))
import Exception (tryIO)
import FastString (headFS, unpackFS)
import Finder (mkHomeModLocation)
import GHC ( ParsedModule(..), TypecheckedModule(..)
           , typecheckModule, runGhc )
import GhcMonad (GhcMonad(..), getSessionDynFlags)
import HeaderInfo (getOptionsFromFile)
import HscTypes ( HsParsedModule(..), ModSummary(..) )
import HsImpExp (ImportDecl(..))
import HsSyn (HsModule(..))
import Module (ModLocation(..), ModuleName, mkModule, mkModuleName)
import SrcLoc (Located)
import Util (getModificationUTCTime)

-- ghc-paths
import GHC.Paths (libdir)

-- time
import Data.Time (getCurrentTime)

-- Internal
import Language.SK.Builder (HModule)
import Language.SK.Expand
import Language.SK.Form
import Language.SK.SKC
import Language.SK.Syntax
import Language.SK.Lexer
import Language.SK.Reader


-- | Run 'Skc' with given environment and 'skcErrrorHandler'.
runSkc :: Skc a -> SkEnv -> IO a
runSkc m sk_env = runGhc (Just libdir) (fmap fst (toGhc m sk_env))

-- | Parse sexpressions.
parseSexprs :: Maybe FilePath -- ^ Name of input file.
            -> BL.ByteString  -- ^ Contents to parse.
            -> Skc ([Code], SPState)
parseSexprs mb_file contents =
  case runSP sexprs mb_file contents of
    Right a  -> return a
    Left err -> failS err

-- | Run given builder.
buildHsSyn :: Builder a -- ^ Builder to use.
           -> [Code]    -- ^ Input codes.
           -> Skc a
buildHsSyn bldr forms =
  case evalBuilder bldr forms of
    Right a  -> return a
    Left err -> failS err

-- | Compile a file containing SK module.
compileSkModule :: FilePath -> Skc (HModule, SPState)
compileSkModule file = do
  (form, sp) <- parseFile file
  mdl <- compileSkModuleForm form
  return (mdl, sp)

compileWithSymbolConversion :: FilePath -> Skc (HModule, SPState)
compileWithSymbolConversion file = do
  -- XXX: Might remove this function.
  (form, sp) <- parseFile file
  form' <- withExpanderSettings (expands form)
  mdl <- buildHsSyn parseModule (map asHaskellSymbols form')
  return (mdl, sp)

asHaskellSymbols :: Code -> Code
asHaskellSymbols = f1
  where
    f1 orig@(LForm (L l form)) =
      case form of
        List forms         -> li (List (map f1 forms))
        HsList forms       -> li (HsList (map f1 forms))
        Atom (ASymbol sym) -> li (Atom (ASymbol (f2 sym)))
        _                  -> orig
      where
        li = LForm . (L l)
    f2 sym
      | headFS sym `elem` haskellOpChars = sym
      | otherwise = fsLit (replace (unpackFS sym))
    replace = map (\x -> case x of
                           '-' -> '_'
                           _   -> x)

parseFile :: FilePath -> Skc ([Code], SPState)
parseFile file = do
  contents <- liftIO (BL.readFile file)
  (form, sp) <- parseSexprs (Just file) contents
  _ <- setDynFlagsFromSPState sp
  return (form, sp)

-- | Compile 'HModule' from given list of codes.
compileSkModuleForm :: [Code] -> Skc HModule
compileSkModuleForm form = do
  expanded <- withExpanderSettings (expands form)
  buildHsSyn parseModule expanded

-- | Get language extensions in current 'Skc' from given 'SPState'.
getDynFlagsFromSPState :: SPState -> Skc DynFlags
getDynFlagsFromSPState sp = do
  dflags0 <- getSessionDynFlags
  -- Adding "-X" to 'String' representation of 'LangExt' data type, as
  -- done in 'HeaderInfo.checkExtension'.
  let mkx = fmap ("-X" ++)
      exts = map mkx (langExts sp)
  (dflags1,_,_) <- parseDynamicFilePragma dflags0 exts
  (dflags2,_,_) <- parseDynamicFilePragma dflags1 (ghcOptions sp)
  return dflags2

-- | Set language extensions in current 'Skc' from given 'SPState'.
setDynFlagsFromSPState :: SPState -> Skc DynFlags
setDynFlagsFromSPState sp = do
  dflags <- getDynFlagsFromSPState sp
  setDynFlags dflags
  return dflags

-- | Extract function from macro and apply to given code. Uses
-- 'emptySkEnv' with 'specialForms' to unwrap the macro from 'Skc'.
macroFunction :: Macro -> Code -> IO Code
macroFunction mac form = do
  let fn = case mac of
             Macro f       -> f
             SpecialForm f -> f
      sk_env = emptySkEnv { envMacros = specialForms
                          , envDefaultMacros = specialForms }
  runSkc (fn form) sk_env

-- | Action to type check module.
--
-- Error location are derived from 'HsModule', locations precisely match
-- with S-expression source code, pretty much helpful.
---
tcHsModule :: Maybe FilePath   -- ^ Source of the module.
           -> Maybe ModSummary -- ^ Old ModSummary, if any.
           -> Bool -- ^ True to generate files, otherwise False.
           -> HModule -- ^ Module to typecheck.
           -> Skc TypecheckedModule
tcHsModule mbfile mb_ms genFile mdl = do
  dflags0 <- getDynFlags
  let fn = fromMaybe "anon" mbfile
      dflags1 =
       if genFile
          then dflags0
          else dflags0 { hscTarget = HscNothing
                       , ghcLink = NoLink }
  setDynFlags dflags1
  ms <- maybe (mkModSummary mbfile mdl) return mb_ms
  let ann = (Map.empty, Map.empty)
      r_s_loc = mkSrcLoc (fsLit fn) 1 1
      r_s_span = mkSrcSpan r_s_loc r_s_loc
      pm = ParsedModule { pm_mod_summary = ms
                        , pm_parsed_source = L r_s_span mdl
                        , pm_extra_src_files = [fn]
                        , pm_annotations = ann }
  tc <- typecheckModule pm
  setDynFlags dflags0
  return tc

-- | Make 'ModSummary'. 'UnitId' is main unit.
mkModSummary :: GhcMonad m => Maybe FilePath -> HModule -> m ModSummary
mkModSummary mbfile mdl =
  let modName = case hsmodName mdl of
                  Just name -> unLoc name
                  Nothing -> mkModuleName "Main"
      imports = map (ideclName . unLoc) (hsmodImports mdl)
      emptyAnns = (Map.empty, Map.empty)
      file = fromMaybe "<unknown>" mbfile
      r_s_loc = mkSrcLoc (fsLit file) 1 1
      r_s_span = mkSrcSpan r_s_loc r_s_loc
      pm = HsParsedModule
        { hpm_module = L r_s_span mdl
        , hpm_src_files = maybeToList mbfile
        , hpm_annotations = emptyAnns }
  in  mkModSummary' mbfile modName imports (Just pm)

-- | Make 'ModSummary' from source file, module name, and imports.
mkModSummary' :: GhcMonad m
              => Maybe FilePath -> ModuleName
              -> [Located ModuleName] -> Maybe HsParsedModule
              -> m ModSummary
mkModSummary' mbfile modName imports mb_pm = do
  dflags0 <- getSessionDynFlags
  let fn = fromMaybe "anonymous" mbfile
      unitId = thisPackage dflags0
      mmod = mkModule unitId modName
      imported = map (\x -> (Nothing, x)) imports
      tryGetTimeStamp x = liftIO (tryIO (getModificationUTCTime x))
  mloc <- liftIO (mkHomeModLocation dflags0 modName fn)
  hs_date <-
    liftIO (maybe getCurrentTime getModificationUTCTime mbfile)
  e_obj_date <- tryGetTimeStamp (ml_obj_file mloc)
  e_hi_date <- tryGetTimeStamp (ml_hi_file mloc)
  let e2mb e = case e of Right a -> Just a; _ -> Nothing
      obj_date = e2mb e_obj_date
      iface_date = e2mb e_hi_date
  dflags1 <-
    if isHsSource fn
      then do
        opts <- liftIO (getOptionsFromFile dflags0 fn)
        (dflags1,_,_) <- liftIO (parseDynamicFilePragma dflags0 opts)
        return dflags1
      else return dflags0
  -- XXX: Have not tested with complex module importing modules from
  -- non-standard packages.
  return ModSummary { ms_mod = mmod
                    , ms_hsc_src = HsSrcFile
                    , ms_location = mloc
                    , ms_hs_date = hs_date
                    , ms_obj_date = obj_date
                    , ms_iface_date = iface_date
                    , ms_parsed_mod = mb_pm
                    , ms_srcimps = []
                    , ms_textual_imps = imported
                    , ms_hspp_file = fn
                    , ms_hspp_opts = dflags1
                    , ms_hspp_buf = Nothing }

isHsSource :: FilePath -> Bool
isHsSource path = "hs" == suffix
  where
    suffix = reverse (takeWhile (/= '.') (reverse path))
