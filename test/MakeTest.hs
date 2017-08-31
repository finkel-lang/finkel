module MakeTest (makeTests, removeArtifacts) where

-- base
import Control.Monad (void, when)
import Data.List (intercalate)
import System.Directory (getDirectoryContents, removeFile)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeExtension)
import System.Process (rawSystem)

-- hspec
import Test.Hspec

-- sk-core
import Language.SK.GHC
import Language.SK.Make
import Language.SK.Run
import Language.SK.SKC

makeTests :: Spec
makeTests = do
  buildSk ["main1.sk"]
  buildSk ["main2.sk"]
  buildSk ["main3.sk"]
  buildSk ["main4.sk", "M3.sk"]
  buildC ["cbits1.c"]
  buildPackage "p01"

buildSk :: [FilePath] -> Spec
buildSk = buildFile (return ())

buildC :: [FilePath] -> Spec
buildC =
  buildFile (do dflags <- getSessionDynFlags
                void (setSessionDynFlags (dflags {ghcLink=NoLink})))

buildFile :: Skc () -> [FilePath] -> Spec
buildFile pre paths =
  before_ (removeArtifacts odir) $
  describe ("files " ++ intercalate ", " paths) $
    it "should compile successfully" $ do
      ret <- runSkc (pre >> make' targets False Nothing)
                    (initialSkEnv {envSilent = True})
      ret `shouldBe` Right ()
  where
    targets = map (\path -> (path, Nothing)) paths
    odir = "test" </> "data" </> "build"
    make' sources doLink out = do
      dflags <- getSessionDynFlags
      let dflags' = dflags {importPaths = [".", odir]}
      _ <- setSessionDynFlags dflags'
      make sources doLink out

removeArtifacts :: FilePath -> IO ()
removeArtifacts dir = do
  contents <- getDirectoryContents dir
  mapM_ removeObjAndHi contents
  where
    removeObjAndHi file =
      when (takeExtension file `elem` [".o", ".hi"])
           (removeFile (dir </> file))

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
