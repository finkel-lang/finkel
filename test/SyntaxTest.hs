-- | Tests for syntax.
--
-- All files under "test/data" directory with '.sk' extension (i.e.:
-- "test/data/*.sk") are read and compiled, then type checked.
--
module SyntaxTest (syntaxTests) where

import System.Directory (getTemporaryDirectory)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Test.Hspec

import Language.SK.Make
import Language.SK.Run

readCode :: FilePath -> IO Bool
readCode src = do
  let go = do (mdl, _st) <- compileSkModule src
              tcHsModule (Just src) False mdl
  compiled <- runSkc go initialSkEnv
  case compiled of
    Right _tc -> return True
    Left e -> putStrLn e >> return False

mkTest :: FilePath -> Spec
mkTest path = do
  tmpdir <- runIO getTemporaryDirectory
  let dotO = tmpdir </> "a.out"
  describe path $ do
    it "should type check" $ do
      result <- readCode path
      result `shouldBe` True
    it "should compile with skc" $ do
      let task = make [(path, Nothing)] False (Just dotO)
      ret <- runSkc task initialSkEnv
      ret `shouldBe` Right ()
    it "should run the executable from skc successfully" $ do
      (ecode, _stdout, _stderr) <- readProcessWithExitCode dotO [] ""
      ecode `shouldBe` ExitSuccess

syntaxTests :: [FilePath] -> Spec
syntaxTests = mapM_ mkTest
