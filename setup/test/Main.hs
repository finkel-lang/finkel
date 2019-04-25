module Main where

-- base
import System.Environment (unsetEnv, withArgs)

-- directory
import System.Directory (getCurrentDirectory, setCurrentDirectory)

-- filepath
import System.FilePath ((</>))

-- hspec
import Test.Hspec

-- process
import System.Process (readProcess)

-- Internal
import Distribution.Simple.SK

main :: IO ()
main = do
  pkgdbs <- getStackPackageDbs
  cwd <- getCurrentDirectory

  -- Required to unset "GHC_PACKAGE_PATH" environment variable before
  -- invoking setup script, otherwise the setup script will complain.
  unsetEnv "GHC_PACKAGE_PATH"

  hspec (afterAll_ (setCurrentDirectory cwd)
                   (buildPackage cwd pkgdbs "p01"))

buildPackage :: String -> [String] -> String -> Spec
buildPackage cwd pkgdbs name =
  describe ("package " <> name) $
    it "should compile and pass the tests" $ do
      let pkgdir = cwd </> "test" </> "data" </> name
          pkgdb_flags =
            [ "--package-db=clear"
            , "--package-db=global"
            ] ++ fmap ("--package-db=" <>) pkgdbs
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
setup args = withArgs args (defaultMainWithHooks skkcHooks)

getStackPackageDbs :: IO [String]
getStackPackageDbs = do
  snapshot_pkgdb <- getStackOutput ["path", "--snapshot-pkg-db"]
  local_pkgdb <- getStackOutput ["path", "--local-pkg-db"]
  let removeTrailingNewline = takeWhile (/= '\n')
  return (map removeTrailingNewline [snapshot_pkgdb, local_pkgdb])

getStackOutput :: [String] -> IO String
getStackOutput args = readProcess "stack" args ""
