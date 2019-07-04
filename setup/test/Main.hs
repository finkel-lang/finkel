module Main where

-- base
import Control.Exception (SomeException(..), catch)
import Data.List (isSubsequenceOf)
import System.Environment (getEnv, getExecutablePath, unsetEnv, withArgs)

-- ghc
import Config

-- directory
import System.Directory (getCurrentDirectory, setCurrentDirectory)

-- filepath
import System.FilePath ((</>), joinPath, splitDirectories)

-- hspec
import Test.Hspec

-- process
import System.Process (readProcess)

-- Internal
import Distribution.Simple.SK

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

  hspec (afterAll_ (setCurrentDirectory cwd)
                   (buildPackage cwd pkgdbs "p01"))

buildPackage :: String -> [String] -> String -> Spec
buildPackage cwd pkgdbs name =
  describe ("package " ++ name) $
    it "should compile and pass the tests" $ do
      let pkgdir = cwd </> "test" </> "data" </> name
          pkgdb_flags =
            [ "--package-db=clear"
            , "--package-db=global"
            ] ++ fmap ("--package-db=" ++) pkgdbs
          configure_args =
            "configure" : pkgdb_flags ++ ["--enable-tests"]
          run act = act `shouldReturn` ()
      mapM_ run
            [ setCurrentDirectory pkgdir
            , setup configure_args
            , setup ["build"]
            , setup ["test"]
            , setup ["haddock"]
            , setup ["clean"]
            ]

setup :: [String] -> IO ()
setup = flip withArgs skkcMain

getPackageDbs :: String -> IO [String]
getPackageDbs executable_path =
  if ".stack-work" `isSubsequenceOf` executable_path
     then getStackPackageDbs
     else getCabalPackageDbs executable_path

getStackPackageDbs :: IO [String]
getStackPackageDbs = do
  -- Getting package database paths from "GHC_PACKAGE_PATH" environment
  -- variable, so that we can get the package database paths without
  -- knowing which "stack.yaml" file were used.
  paths <- catch (getEnv "GHC_PACKAGE_PATH")
                 (\(SomeException _) -> return "")
  return (reverse (sepByColon paths))

sepByColon :: String -> [String]
sepByColon xs =
  case dropWhile (== ':') xs of
    "" -> []
    ys -> case break (== ':') ys of
           (w, ys') -> w : sepByColon ys'

getCabalPackageDbs :: String -> IO [String]
getCabalPackageDbs executable_path = do
  home <- getEnv "HOME"
  let dirs = splitDirectories executable_path
      distdir = takeWhile (/= "dist-newstyle") dirs
      ghc_ver = "ghc-" ++ cProjectVersion
      localdb = joinPath distdir </>
                joinPath ["dist-newstyle", "packagedb", ghc_ver]
      storedb = joinPath [home, ".cabal", "store", ghc_ver, "package.db"]
  return [storedb, localdb]
