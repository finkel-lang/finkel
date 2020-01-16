-- | Module for source code file path look up.

module Language.Finkel.TargetSource
  ( -- * Target source
    TargetSource(..)
  , targetSourcePath
  , isOtherSource

  -- * Finder functions
  , findTargetSource
  , findTargetSourceMaybe
  , findFileInImportPaths

   -- * File path related functions
  , asModuleName
  , isFnkFile
  , isHsFile
  ) where

-- base
import           Control.Exception          (SomeException)
import           Control.Monad              (mplus)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Data.Char                  (isUpper)

-- bytestring
import qualified Data.ByteString.Lazy.Char8 as BL

-- directory
import           System.Directory           (doesFileExist)

-- filepath
import           System.FilePath            (dropExtension, normalise,
                                             pathSeparator,
                                             replaceExtension, splitPath,
                                             takeExtension, (<.>), (</>))

-- ghc
import           DynFlags                   (DynFlags (..),
                                             HasDynFlags (..))
import           ErrUtils                   (mkErrMsg)
import           Exception                  (gtry)
import           HscTypes                   (throwOneError)
import           Module                     (mkModuleName,
                                             moduleNameSlashes)
import           Outputable                 (neverQualify, text)
import           SrcLoc                     (GenLocated (..), Located)
import           Util                       (looksLikeModuleName)

-- Internal
import           Language.Finkel.Fnk
import           Language.Finkel.Form
import           Language.Finkel.Lexer
import           Language.Finkel.Reader


-- ---------------------------------------------------------------------
--
-- Target source
--
-- ---------------------------------------------------------------------

-- | Data type to represent target source.
data TargetSource
  = FnkSource FilePath String [Code] SPState
  -- ^ Finkel source. Holds file path of the source code, original
  -- string input, parsed form data, and required module names.
  | HsSource FilePath
  -- ^ Haskell source with file path of the source code.
  | OtherSource FilePath
  -- ^ Other source with file path of other contents.
  deriving (Eq)

instance Show TargetSource where
  show s = case s of
    FnkSource path mdl _ _sp ->
      concat ["FnkSource ", show path, " ", mdl, " "]
    HsSource path -> "HsSource " ++ show path
    OtherSource path -> "OtherSource " ++ show path

-- | Get the file path of given 'TargetSource'.
targetSourcePath :: TargetSource -> FilePath
targetSourcePath mt =
  case mt of
    FnkSource path _ _ _ -> path
    HsSource path        -> path
    OtherSource path     -> path

-- | 'True' is the 'TargetSource' is 'OtherSource'.
isOtherSource :: TargetSource -> Bool
isOtherSource ts =
  case ts of
    OtherSource{} -> True
    _             -> False

-- | True if given file has Finkel extension.
isFnkFile :: FilePath -> Bool
isFnkFile path = takeExtension path == ".fnk"

-- | True if given file has Haskell extension.
isHsFile :: FilePath -> Bool
isHsFile path = takeExtension path `elem` [".hs", ".lhs"]

-- | Construct module name from given 'String'.
asModuleName :: String -> String
asModuleName name
   | looksLikeModuleName name = name
   | otherwise                = map sep_to_dot (concat names)
   where
     names = dropWhile (not . isUpper . head)
                       (splitPath (dropExtension name))
     sep_to_dot c
       | c == pathSeparator = '.'
       | otherwise          = c

-- | Find source code file path by module name.
--
-- Current approach for source code lookup is search for file with
-- @*.fnk@ suffix first. Return it if found, otherwise search file with
-- @*.hs@ suffix.
--
-- This searching strategy can used when compiling cabal package
-- containing mixed codes with '*.fnk' and '*.hs' suffixes.
--
findFileInImportPaths :: MonadIO m
                      => [FilePath] -- ^ Directories to look for.
                      -> String -- ^ Module name or file name.
                      -> m (Maybe FilePath)
                      -- ^ File path of the module, if found.
findFileInImportPaths dirs modName = do
  let suffix = takeExtension modName
      moduleFileName = moduleNameSlashes (mkModuleName modName)
      moduleFileName'
        | suffix `elem` [".fnk", ".hs", ".c"] = modName
        | otherwise = moduleFileName <.> "fnk"
      search mb_hs ds =
        case ds of
          []    -> return mb_hs
          d:ds' -> do
            -- Extension not yet sure for `aPath', so searching both of
            -- '.fnk' and '.hs' files.
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
      dirs' | "." `elem` dirs = dirs
            | otherwise       = dirs ++ ["."]
  mb_found <- search Nothing dirs'
  return mb_found

-- | Find 'TargetSource' from command line argument. This function
-- throws 'FinkelException' when the target source was not found.
findTargetSource :: Located String -> Fnk TargetSource
findTargetSource (L l modNameOrFilePath)= do
  dflags <- getDynFlags
  mb_inputPath <-
    findFileInImportPaths (importPaths dflags) modNameOrFilePath
  let detectSource path
        | isFnkFile path =
          do contents <- liftIO (BL.readFile path)
             (forms, sp) <- parseSexprs (Just path) contents
             let modName = asModuleName modNameOrFilePath
             return (FnkSource path modName forms sp)
        | isHsFile path = return (HsSource path)
        | otherwise = return (OtherSource path)
  case mb_inputPath of
    Just path -> detectSource path
    Nothing   -> do
      let err = mkErrMsg dflags l neverQualify doc
          doc = text ("cannot find target source: " ++ modNameOrFilePath)
      throwOneError err

-- | Like 'findTargetSource', but the result wrapped in 'Maybe'.
findTargetSourceMaybe :: Located String -> Fnk (Maybe TargetSource)
findTargetSourceMaybe modName = do
  et_ret <- gtry (findTargetSource modName)
  case et_ret of
    Right found -> return (Just found)
    Left _err   -> let _err' = _err :: SomeException
                   in  return Nothing
