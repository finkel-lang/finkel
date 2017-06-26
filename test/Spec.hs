-- | Test for S-expression.
--
-- All files under test data directory with '.sk' extension (i.e.:
-- "test/data/*.lisp") are read and compiled, then type checked.
--
module Main where

import System.FilePath
import System.Directory
import Test.Hspec

import SK.Core.Emit
import SK.Core.Run
import SK.Core.Typecheck

getTestFiles :: IO [FilePath]
getTestFiles =
  let dir = "test" </> "data"
      f = (\x acc -> if takeExtension x == ".sk"
                                 then (dir </> x) : acc
                                 else acc)
      files = getDirectoryContents dir
  in  foldr f [] <$> files

readCode :: FilePath -> IO Bool
readCode src = do
  contents <- readFile src
  let go = do (mdl, st) <- compile (Just src) contents
              tcHsModule (Just src) False mdl
  compiled <- runSkc go initialSkEnv
  case compiled of
    Right _tc -> return True
    Left e -> putStrLn e >> return False

stripTrailingNewlines :: String -> String
stripTrailingNewlines str = reverse (dropWhile (== '\n') (reverse str))

mkTest :: String -> Spec
mkTest name =
  before (readCode name) $ do
    describe name $ do
      it "should compile and type check"
         (\result -> result `shouldBe` True)

main :: IO ()
main = do
  files <- getTestFiles
  hspec (mapM_ mkTest files)
