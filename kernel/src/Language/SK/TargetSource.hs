-- | Module for source code file path look up.

module Language.SK.TargetSource
  ( -- * Target source
    TargetSource(..)
  , targetSourcePath
  , isOtherSource

    -- * File path related functions
  , asModuleName
  , findFileInImportPaths
  , isSkFile
  , isHsFile
  ) where

-- base
import Control.Monad (mplus)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Char (isUpper)

-- directory
import System.Directory (doesFileExist)

-- filepath
import System.FilePath ( dropExtension, pathSeparator
                       , replaceExtension , splitPath
                       , takeExtension, (<.>), (</>))

-- ghc
import Module (mkModuleName, moduleNameSlashes)
import Util (looksLikeModuleName)

-- Internal
import Language.SK.Form
import Language.SK.Lexer
import Language.SK.SKC


-- ---------------------------------------------------------------------
--
-- Target source
--
-- ---------------------------------------------------------------------

-- | Data type to represent target source.
data TargetSource
  = SkSource FilePath String [Code] SPState
  -- ^ SK source. Holds file path of the source code, original string
  -- input, parsed form data, and required module names.
  | HsSource FilePath
  -- ^ Haskell source with file path of the source code.
  | OtherSource FilePath
  -- ^ Other source with file path of other contents.
  deriving (Eq)

instance Show TargetSource where
  show s = case s of
    SkSource path mdl _ _sp ->
      concat ["SkSource ", show path, " ", mdl, " "]
    HsSource path -> "HsSource " ++ show path
    OtherSource path -> "OtherSource " ++ show path

-- | Get the file path of given 'TargetSource'.
targetSourcePath :: TargetSource -> FilePath
targetSourcePath mt =
  case mt of
    SkSource path _ _ _ -> path
    HsSource path       -> path
    OtherSource path    -> path

-- | 'True' is the 'TargetSource' is 'OtherSource'.
isOtherSource :: TargetSource -> Bool
isOtherSource ts =
  case ts of
    OtherSource{} -> True
    _             -> False

-- | Find source code file path by module name.
--
-- Current approach for source code lookup is search for file with
-- @*.sk@ suffix first. Return it if found, otherwise search file with
-- @*.hs@ suffix.
--
-- This searching strategy can used when compiling cabal package
-- containing mixed codes with '*.sk' and '*.hs' suffixes.
--
findFileInImportPaths :: [FilePath] -- ^ Directories to look for.
                      -> String -- ^ Module name or file name.
                      -> Skc (Maybe FilePath)
                      -- ^ The file path of module, if found.
findFileInImportPaths dirs modName = do
  let suffix = takeExtension modName
      moduleFileName = moduleNameSlashes (mkModuleName modName)
      moduleFileName'
        | suffix `elem` [".sk", ".hs", ".c"] = modName
        | otherwise = moduleFileName <.> "sk"
      search mb_hs ds =
        case ds of
          []    -> return mb_hs
          d:ds' -> do
            -- Extension not yet sure for `aPath', so searching both of
            -- '.sk' and '.hs' files.
            let aPath = d </> moduleFileName'
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
  debugSkc (";;; moduleName: " ++ show modName)
  mb_found <- search Nothing dirs'
  debugSkc (case mb_found of
              Just found -> ";;; File found: " ++ found
              Nothing    -> ";;; File not found")
  return mb_found

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

-- | True if given file has sk extension.
isSkFile :: FilePath -> Bool
isSkFile path = takeExtension path == ".sk"

-- | True if given file has haskell extension.
isHsFile :: FilePath -> Bool
isHsFile path = takeExtension path `elem` [".hs", ".lhs"]
