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

import SK.Core.Run

getTestFiles :: IO [FilePath]
getTestFiles =
  let f = (\x acc -> if takeExtension x == ".lisp"
                         then dropExtension x : acc
                         else acc)
      files = getDirectoryContents ("test" </> "data")
  in  foldr f [] <$> files

readCode :: String -> IO (String, String)
readCode name = do
  let input = "test" </> "data" </> name <.> "lisp"
  contents <- readFile input
  expected <- readFile ("test" </> "data" </> name <.> "hs")
  compiled <- compileAndEmit (Just input) contents
  case compiled of
    Right code -> return (code, expected)
    Left e -> putStrLn e >> return ("", expected)

stripTrailingNewlines :: String -> String
stripTrailingNewlines str = reverse (dropWhile (== '\n') (reverse str))

mkTest :: String -> Spec
mkTest name =
  before (readCode name) $ do
    describe name $ do
      it "compiles to expected"
         (\(code,expected) ->
            code `shouldBe` stripTrailingNewlines expected)

main :: IO ()
main = do
     files <- getTestFiles
     hspec (mapM_ mkTest files)
