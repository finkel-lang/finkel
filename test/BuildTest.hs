module BuildTest where

-- base
import Control.Monad (when)
import Data.List (intersperse)
import System.Directory ( doesFileExist, getDirectoryContents
                        , removeFile)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), replaceExtension, takeExtension)
import System.Process (rawSystem)

-- hspec
import Test.Hspec

-- sk-core
import Language.SK.GHC
import Language.SK.Make
import Language.SK.Run

buildTests :: Spec
buildTests = do
  buildFile ["main1.sk"]
  buildFile ["main2.sk"]
  buildFile ["main3.sk"]
  buildFile ["main4.sk", "M3.sk"]
  buildPackage "p01"

buildFile :: [FilePath] -> Spec
buildFile paths =
  before_ removeArtifacts $
  describe ("files " ++ concat (intersperse ", " paths)) $
    it "should compile successfully" $ do
      ret <- runSkc (make' targets False Nothing) initialSkEnv
      ret `shouldBe` Right ()
  where
     removeArtifacts = do
       contents <- getDirectoryContents odir
       mapM_ removeObjAndHi contents
     removeObjAndHi file =
       when (takeExtension file `elem` [".o", ".hi"])
            (removeFile (odir </> file))
     targets = map (\path -> (path, Nothing)) paths
     odir = "test" </> "data" </> "build"
     make' targets link out = do
       dflags <- getSessionDynFlags
       let dflags' = dflags {importPaths = [".", odir]}
       setSessionDynFlags dflags'
       make targets link out

buildPackage :: String -> Spec
buildPackage name =
  describe ("package " ++ name) $
    it "should compile and pass the tests" $ do
      let s = stack name
      _ <- s ["setup"]
      _ <- s ["build"]
      let target = name ++ ":test:" ++ name ++ "-test"
      exitCode <- s ["test", target]
      _ <- s ["clean", name]
      exitCode `shouldBe` ExitSuccess

stack :: String -> [String] -> IO ExitCode
stack projectName args = do
  let yaml = "test" </> "data" </> "build" </> projectName </>
             "stack.yaml"
  rawSystem "stack" (("--stack-yaml=" ++ yaml):"--silent":args)
