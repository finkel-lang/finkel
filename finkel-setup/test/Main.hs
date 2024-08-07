{-# LANGUAGE CPP #-}
module Main where

-- base
import Control.Exception                 (catch, throw)
import Data.List                         (isSubsequenceOf)
import System.Environment                (getExecutablePath, lookupEnv, setEnv,
                                          unsetEnv, withArgs)

-- ghc
#if MIN_VERSION_ghc(9,0,0)
import GHC.Settings.Config               (cProjectVersion)
#else
import Config                            (cProjectVersion)
#endif

-- directory
import System.Directory                  (doesDirectoryExist,
                                          getCurrentDirectory,
                                          removeDirectoryRecursive,
                                          setCurrentDirectory)
import System.Directory.Internal.Prelude (isDoesNotExistError)

-- filepath
import System.FilePath                   (isSearchPathSeparator, joinPath,
                                          splitDirectories, takeDirectory,
                                          (</>))

-- hspec
import Test.Hspec

-- Internal
import Distribution.Simple.Finkel

main :: IO ()
main = do
  executable <- getExecutablePath
  pkgdbs <- getPackageDbs executable
  cwd <- getCurrentDirectory

  putChar '\n'
  putStrLn ("executable: " ++ executable)
  putStrLn (unlines ("pkgdbs:" : map ("  - " ++) pkgdbs))
  putStrLn ("cwd: " ++ cwd)

  -- Required to unset "GHC_PACKAGE_PATH" environment variable before
  -- invoking setup script, otherwise the setup script will complain.
  unsetEnv "GHC_PACKAGE_PATH"

  -- Setting the `null' package environment for ghc >= 8.4.0, to support
  -- building executable in test packages. In ghc 8.2.x, the use of "-"
  -- in GHC_ENVIRONMENT will show "No such package environment" error,
  -- so "executable" and "test" stanzas in ".cabal" file are disabled.
  setEnv "GHC_ENVIRONMENT" "-"

  hspec (afterAll_ (setCurrentDirectory cwd)
                   (beforeAll_ (removeDistIfExist cwd)
                               (do buildPackage cwd pkgdbs "p01"
                                   buildPluginPackage cwd pkgdbs "p02")))

buildPackage :: String -> [String] -> String -> Spec
buildPackage = buildPackageWith setup
  where
    setup args = do
      putStrLn (unwords ("running:" : args))
      withArgs args fnkMain

buildPluginPackage :: String -> [String] -> String -> Spec
buildPluginPackage = buildPackageWith plugin_setup
  where
    plugin_setup args = do
      putStrLn (unwords ("running:" : args))
      withArgs args fnkPluginMainForHaddock

buildPackageWith :: ([String] -> IO ()) -> String -> [String] -> String -> Spec
buildPackageWith my_main cwd pkgdbs name =
  describe ("package " ++ name) $
    it "should compile and pass the tests" $ do
      let pkgdir = joinPath [cwd, "test", "data", name]
          pkgdb_flags =
            [ "--package-db=clear"
            , "--package-db=global"
            ] ++ fmap ("--package-db=" ++) pkgdbs
          configure_args =
            "configure" : pkgdb_flags ++ ["--enable-tests", "-v2"]
          run act = act `shouldReturn` ()
      mapM_ run
            [ setCurrentDirectory pkgdir
            , my_main configure_args
            , my_main ["build"]
            , my_main ["test"]
            , my_main ["haddock"]
            ]

getPackageDbs :: String -> IO [String]
getPackageDbs executable_path
  | ".stack-work" `isSubsequenceOf` executable_path
  = getStackPackageDbs
  | "dist-newstyle" `isSubsequenceOf` executable_path
  = getCabalPackageDbs executable_path
  | otherwise
  = getPackageConfD executable_path

getStackPackageDbs :: IO [String]
getStackPackageDbs = do
  -- Getting package database paths from "GHC_PACKAGE_PATH" environment
  -- variable, so that we can get the package database paths without
  -- knowing which "stack.yaml" file were used.
  mb_paths <- lookupEnv "GHC_PACKAGE_PATH"
  case mb_paths of
    Just paths -> return (reverse (sepBySearchPathSeparator paths))
    Nothing    -> return []

sepBySearchPathSeparator :: String -> [String]
sepBySearchPathSeparator xs =
  case dropWhile isSearchPathSeparator xs of
    "" -> []
    ys -> case break isSearchPathSeparator ys of
           (w, ys') -> w : sepBySearchPathSeparator ys'

getCabalPackageDbs :: String -> IO [String]
getCabalPackageDbs executable_path = do
  let dirs = splitDirectories executable_path
      distdir = takeWhile (/= "dist-newstyle") dirs
      ghc_ver = "ghc-" ++ cProjectVersion
      localdb = joinPath distdir </>
                joinPath ["dist-newstyle", "packagedb", ghc_ver]
  return [localdb]

getPackageConfD :: FilePath -> IO [FilePath]
getPackageConfD path = go path (takeDirectory path)
  where
    go prev current =
        if prev == current
           then return []
           else do
             let pkg_conf_d = current </> "package.conf.d"
             found <- doesDirectoryExist pkg_conf_d
             if found
                then return [pkg_conf_d]
                else go current (takeDirectory current)

removeDistIfExist :: FilePath -> IO ()
removeDistIfExist cwd = mapM_ remove_dir ["p01", "p02"]
  where
    remove_dir pkg =
      catch (let dir = cwd </> "test" </> "data" </> pkg </> "dist"
             in  removeDirectoryRecursive dir)
        (\e -> if isDoesNotExistError e
                 then return ()
                 else throw e)
