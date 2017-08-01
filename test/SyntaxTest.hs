-- | Tests for syntax.
--
-- All files under "test/data" directory with '.sk' extension (i.e.:
-- "test/data/*.sk") are read and compiled, then type checked.
--
module SyntaxTest (syntaxTests, getTestFiles) where

import Data.List
import System.FilePath
import System.Directory
import Test.Hspec

import Language.SK.Run

getTestFiles :: IO [FilePath]
getTestFiles =
  let dir = "test" </> "data" </> "syntax"
      f x acc = if takeExtension x == ".sk"
                  then (dir </> x) : acc
                  else acc
      files = getDirectoryContents dir
  in  sort <$> foldr f [] <$> files

readCode :: FilePath -> IO Bool
readCode src = do
  let go = do (mdl, _st) <- compileSkModule src
              tcHsModule (Just src) False mdl
  compiled <- runSkc go initialSkEnv
  case compiled of
    Right _tc -> return True
    Left e -> putStrLn e >> return False

mkTest :: String -> Spec
mkTest name =
  before (readCode name) $
    describe name $
      it "should compile and type check"
         (\result -> result `shouldBe` True)

syntaxTests :: [FilePath] -> Spec
syntaxTests = mapM_ mkTest
