module BuildTest where

-- base
import Control.Monad (when)
import System.Directory (doesFileExist, removeFile)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), replaceExtension)
import System.Process (rawSystem)

-- hspec
import Test.Hspec

-- sk-core
import SK.Core.Make
import SK.Core.Run

removeIfExist :: FilePath -> IO ()
removeIfExist path = do
  exist <- doesFileExist path
  when exist (removeFile path)

buildFile :: [FilePath] -> Spec
buildFile paths =
  describe ("compile files: " ++ show paths) $
    it "should compile successfully" $ do
      let targets = map (\path -> (path, Nothing)) paths
          rm path ext = removeIfExist (replaceExtension path ext)
      mapM_ (\path -> rm path ".o" >> rm path ".hi") paths
      ret <- runSkc (make targets True Nothing) initialSkEnv
      ret `shouldBe` Right ()

stack :: String -> [String] -> IO ExitCode
stack projectName args = do
  let yaml = "test" </> "data" </> "build" </> projectName </>
             "stack.yaml"
  rawSystem "stack" (("--stack-yaml=" ++ yaml):"--silent":args)

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

buildTests :: Spec
buildTests =
  do buildFile ["test/data/build/m1.sk"]
     buildPackage "p01"
