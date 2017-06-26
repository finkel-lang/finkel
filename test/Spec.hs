-- | Test for S-expression.
--
-- All files under test data directory with '.lisp' extension (i.e.:
-- "test/data/*.lisp") are read and compiled, then compare the resulting
-- Haskell code with the file with '.out' extension.
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
  let f = (\x acc -> if takeExtension x == ".lisp"
                         then dropExtension x : acc
                         else acc)
      files = getDirectoryContents ("test" </> "data")
  in  foldr f [] <$> files

readCode :: String -> IO Bool
readCode name = do
  let input = "test" </> "data" </> name <.> "lisp"
  contents <- readFile input
  let go = do (mdl, st) <- compile (Just input) contents
              tcHsModule (Just input) False mdl
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
