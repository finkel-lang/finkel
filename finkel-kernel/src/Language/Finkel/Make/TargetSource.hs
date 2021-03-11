{-# LANGUAGE CPP #-}
-- | Module for source code file path look up.
module Language.Finkel.Make.TargetSource
  (
  -- * Target unit
    TargetUnit
  , emptyTargetUnit
  , findTargetUnit
  , findTargetUnitMaybe
  , targetUnitName

  -- * Target source
  , TargetSource(..)
  , targetSourcePath

  -- * Finder functions
  , findTargetModuleName
  , findTargetModuleNameMaybe
  , findTargetSource
  , findTargetSourceMaybe
  , findFileInImportPaths

   -- * File path related functions
  , asModuleName
  , isFnkFile
  , isHsFile
  ) where

#include "ghc_modules.h"

-- base
import Control.Exception      (SomeException, try)
import Control.Monad          (mplus)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Char              (isUpper)

-- directory
import System.Directory       (doesFileExist)

-- filepath
import System.FilePath        (dropExtension, normalise, pathSeparator,
                               replaceExtension, splitPath, takeExtension,
                               (<.>), (</>))

-- ghc
import GHC_Driver_Phases      (Phase)
import GHC_Driver_Session     (DynFlags (..))
import GHC_Driver_Types       (throwOneError)
import GHC_Types_SrcLoc       (GenLocated (..), Located)
import GHC_Unit_Module        (ModuleName, mkModuleName, moduleNameSlashes,
                               moduleNameString)
import GHC_Utils_Error        (mkErrMsg)
import GHC_Utils_Misc         (looksLikeModuleName)
import GHC_Utils_Outputable   (Outputable (..), neverQualify, sep, text)


-- ---------------------------------------------------------------------
--
-- Target unit
--
-- ---------------------------------------------------------------------

-- | Unit for compilation target.
--
-- Simply a 'TargetSource' paired with 'Maybe' 'Phase'.
type TargetUnit = (TargetSource, Maybe Phase)

-- | Make empty 'TargetUnit' from 'TargetSource'
emptyTargetUnit :: TargetSource -> TargetUnit
emptyTargetUnit ts = (ts, Nothing)

-- | Get 'TargetUnit' from pair of module name or file path, and phase.
findTargetUnit
  :: MonadIO m => DynFlags -> (Located String, Maybe Phase) -> m TargetUnit
findTargetUnit dflags (lpath,mbp) =
  (\t -> (t, mbp)) <$> findTargetSource dflags lpath

findTargetUnitMaybe
  :: MonadIO m
  => DynFlags -> (Located String, Maybe Phase) -> m (Maybe TargetUnit)
findTargetUnitMaybe dflags (lpath,mbp) =
  fmap (\t -> (t, mbp)) <$> findTargetSourceMaybe dflags lpath

-- | Get 'ModuleName' from given 'TargetUnit'.
targetUnitName :: TargetUnit -> ModuleName
targetUnitName (ts, _) =
  case ts of
    FnkSource _ mn -> mn
    HsSource _ mn  -> mn
    _              -> mkModuleName "module-name-unknown"


-- ---------------------------------------------------------------------
--
-- Target source
--
-- ---------------------------------------------------------------------

-- | Data type to represent target source.
data TargetSource
  = FnkSource FilePath ModuleName
  -- ^ Finkel source.
  --
  -- Holds file path of the source code, module name, parsed form data, and
  -- 'SPState' including required module names.
  | HsSource FilePath ModuleName
  -- ^ Haskell source with file path of the source code.
  | OtherSource FilePath
  -- ^ Other source with file path of other contents.

instance Show TargetSource where
  show s = case s of
    FnkSource path _ -> "FnkSource " ++ show path
    HsSource path _  -> "HsSource " ++ show path
    OtherSource path -> "OtherSource " ++ show path

instance Outputable TargetSource where
  ppr s =
    case s of
      FnkSource path mdl -> sep [text "FnkSource", text path, ppr mdl]
      HsSource path _    -> sep [text "HsSource", text path]
      OtherSource path   -> sep [text "OtherSource", text path]

-- | Get the file path of given 'TargetSource'.
targetSourcePath :: TargetSource -> FilePath
targetSourcePath mt =
  case mt of
    FnkSource path _ -> path
    HsSource path _  -> path
    OtherSource path -> path

-- | True if given file has Finkel extension.
isFnkFile :: FilePath -> Bool
isFnkFile path = takeExtension path == ".fnk"

-- | True if given file has Haskell extension.
isHsFile :: FilePath -> Bool
isHsFile path = takeExtension path `elem` [".hs", ".lhs"]

-- | Construct module name from given 'String'.
asModuleName :: String -> String
asModuleName name =
  if looksLikeModuleName name
     then name
     else map sep_to_dot (concat names)
  where
    -- Taking the directory names from last to first, to support auto generated
    -- modules made by stack.
    names = reverse (takeWhile (isUpper . head)
                               (reverse (splitPath (dropExtension name))))
    sep_to_dot c =
      if c == pathSeparator
         then '.'
         else c

-- | Find source code file path by module name.
--
-- Current approach for source code lookup is search for file with @*.fnk@
-- suffix first. Return it if found, otherwise search file with @*.hs@ suffix.
--
-- This searching strategy can used when compiling cabal package containing
-- mixed codes with '*.fnk' and '*.hs' suffixes.
--
findFileInImportPaths :: MonadIO m
                      => [FilePath] -- ^ Directories to look for.
                      -> String -- ^ Module name or file name.
                      -> m (Maybe FilePath)
                      -- ^ File path of the module, if found.
findFileInImportPaths dirs modName = do
  let suffix = takeExtension modName
      moduleFileName = moduleNameSlashes (mkModuleName modName)
      moduleFileName' = if suffix `elem` [".fnk", ".hs", ".c"]
                           then modName
                           else moduleFileName <.> "fnk"
      search mb_hs ds =
        case ds of
          []    -> return mb_hs
          d:ds' -> do
            -- Extension not yet sure for `aPath', so searching both '.fnk' and
            -- '.hs' files.
            let aPath = normalise (d </> moduleFileName')
                hsPath = replaceExtension aPath ".hs"
            exists <- liftIO (doesFileExist aPath)
            if exists
               then return $! Just aPath
               else do
                 exists' <- liftIO (doesFileExist hsPath)
                 if exists'
                    then search (mb_hs `mplus` Just hsPath) ds'
                    else search mb_hs ds'
      dirs' = if "." `elem` dirs
                 then dirs
                 else dirs ++ ["."]
  search Nothing dirs'

-- | Like 'findTargetSource', but takes 'ModuleName' argument.
findTargetModuleName
  :: MonadIO m => DynFlags -> Located ModuleName -> m TargetSource
findTargetModuleName dflags (L l mname) =
  findTargetSource dflags (L l (moduleNameString mname))

-- | Like 'findTargetSourceMaybe', but takes 'ModuleName' argument.
findTargetModuleNameMaybe
  :: MonadIO m => DynFlags -> Located ModuleName -> m (Maybe TargetSource)
findTargetModuleNameMaybe dflags (L l mname) =
  findTargetSourceMaybe dflags (L l (moduleNameString mname))

-- | Like 'findTargetSource', but the result wrapped in 'Maybe'.
findTargetSourceMaybe
  :: MonadIO m => DynFlags -> Located String -> m (Maybe TargetSource)
findTargetSourceMaybe dflags modName = do
  et_ret <- liftIO (try (findTargetSource dflags modName))
  case et_ret of
    Right found -> return (Just found)
    Left _err   -> let _err' = _err :: SomeException
                   in  return Nothing

-- | Find 'TargetSource' from command line argument. This function throws
-- 'FinkelException' when the target source was not found.
findTargetSource
  :: MonadIO m => DynFlags -> Located String -> m TargetSource
findTargetSource dflags (L l modNameOrFilePath)= do
  mb_inputPath <- findFileInImportPaths (importPaths dflags) modNameOrFilePath
  let detectSource path
        | isFnkFile path =
          do let modName = mkModuleName (asModuleName modNameOrFilePath)
             return (FnkSource path modName)
        | isHsFile path =
          return (HsSource path (mkModuleName (asModuleName path)))
        | otherwise = return (OtherSource path)
  case mb_inputPath of
    Just path -> detectSource path
    Nothing   -> do
      let err = mkErrMsg dflags l neverQualify doc
          doc = text ("cannot find target source: " ++ modNameOrFilePath)
      throwOneError err