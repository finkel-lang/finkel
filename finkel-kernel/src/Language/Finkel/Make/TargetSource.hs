{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE CPP           #-}
{-# LANGUAGE TupleSections #-}
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
  , findTargetSourceWithPragma
  , findFileInImportPaths

   -- * File type related functions
  , asModuleName
  , isFnkFile
  , isHsFile
  , findPragmaString
  ) where

#include "ghc_modules.h"

-- base
import Control.Exception      (SomeException, try)
import Control.Monad          (mplus)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Char              (isUpper)
import Data.List              (isSubsequenceOf)

-- directory
import System.Directory       (doesFileExist)

-- filepath
import System.FilePath        (dropExtension, normalise, pathSeparator,
                               replaceExtension, splitPath, takeExtension,
                               (<.>), (</>))

-- ghc
import GHC_Data_StringBuffer  (StringBuffer, atEnd, hGetStringBuffer, nextChar)
import GHC_Driver_Phases      (Phase)
import GHC_Driver_Session     (DynFlags (..))
import GHC_Types_SourceError  (throwOneError)
import GHC_Types_SrcLoc       (GenLocated (..), Located)
import GHC_Unit_Module        (ModuleName, mkModuleName, moduleNameSlashes,
                               moduleNameString)
import GHC_Utils_Misc         (looksLikeModuleName)
import GHC_Utils_Outputable   (Outputable (..), sep, text)

-- Internal
import Language.Finkel.Error

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
  (,) <$> findTargetSource dflags lpath <*> pure mbp

findTargetUnitMaybe
  :: MonadIO m
  => DynFlags -> (Located String, Maybe Phase) -> m (Maybe TargetUnit)
findTargetUnitMaybe dflags (lpath,mbp) =
  fmap (, mbp) <$> findTargetSourceMaybe dflags lpath

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
  -- ^ Finkel source with file path of the source code and module name.
  | HsSource FilePath ModuleName
  -- ^ Haskell source with file path of the source code and module name.
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
    names = reverse (takeWhile startsWithUpper
                               (reverse (splitPath (dropExtension name))))
    startsWithUpper cs = case cs of
      []  -> False
      c:_ -> isUpper c
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
               then return (Just aPath)
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
findTargetModuleName dflags =
  findTargetSource dflags . fmap moduleNameString

-- | Like 'findTargetSourceMaybe', but takes 'ModuleName' argument.
findTargetModuleNameMaybe
  :: MonadIO m => DynFlags -> Located ModuleName -> m (Maybe TargetSource)
findTargetModuleNameMaybe dflags =
  findTargetSourceMaybe dflags . fmap moduleNameString

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
-- 'SourceError' when the target source was not found.
findTargetSource :: MonadIO m => DynFlags -> Located String -> m TargetSource
findTargetSource = findTargetSourceWithPragma ";;;"

-- | Like 'findTargetSource', but with given pragma string.
findTargetSourceWithPragma
  :: MonadIO m => String -> DynFlags -> Located String -> m TargetSource
findTargetSourceWithPragma pragma dflags (L l modNameOrFilePath)= do
  mb_inputPath <- findFileInImportPaths (importPaths dflags) modNameOrFilePath
  let detectSource path
        | isFnkFile path = return (FnkSource path modName)
        | isHsFile path = do
          buf <- liftIO (hGetStringBuffer path)
          if findPragmaString pragma buf
            then return (FnkSource path modName)
            else return (HsSource path modName)
        | otherwise = return (OtherSource path)
        where
          modName = mkModuleName (asModuleName path)
  case mb_inputPath of
    Just path -> detectSource path
    Nothing   ->
      let doc = text ("cannot find target source: " ++ modNameOrFilePath)
      in  throwOneError (mkPlainWrappedMsg dflags l doc)


-- ------------------------------------------------------------------------
--
-- Finkel buffer detection
--
-- ------------------------------------------------------------------------

findPragmaString :: String -> StringBuffer -> Bool
findPragmaString pragma buf = findInFirstNLines buf 3 (isSubsequenceOf pragma)
{-# INLINABLE findPragmaString #-}

findInFirstNLines :: StringBuffer -> Int -> (String -> Bool) -> Bool
findInFirstNLines buf n test = go n buf
  where
    -- False when the source code contained less number of lines than
    -- the number specified by the argument.
    go i buf0 =
      not (i == 0 || atEnd buf0) &&
      (case getStringBufferLine buf0 of
          (l, buf1) -> test l || go (i-1) buf1)
{-# INLINABLE findInFirstNLines #-}

getStringBufferLine :: StringBuffer -> (String, StringBuffer)
getStringBufferLine = go []
  where
    go !acc buf0 =
      let (c, buf1) = nextChar buf0
      in if c == '\n'
         then (reverse acc, buf1)
         else go (c:acc) buf1
{-# INLINABLE getStringBufferLine #-}
