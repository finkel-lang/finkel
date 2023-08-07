module PreprocessTest where

-- base
import Control.Exception          (SomeException (..), bracket)
import Data.List                  (intercalate)
import System.Environment         (withArgs)
import System.Exit                (ExitCode (..))

-- directory
import System.Directory           (createDirectoryIfMissing,
                                   getTemporaryDirectory,
                                   removeDirectoryRecursive)

-- filepath
import System.FilePath            (takeDirectory, (</>))

-- hspec
import Test.Hspec

-- finkel-kernel
import Language.Finkel.Preprocess (defaultPreprocess)

-- Internal
import TestAux


-- ------------------------------------------------------------------------
--
-- Preprocessor tests
--
-- ------------------------------------------------------------------------

preprocessTests :: Spec
preprocessTests = around withTemporaryFile $
  describe "preprocess" $ do
    let ul = intercalate "\n"
        pp args = quietly (withArgs args defaultPreprocess)
        anyExitFailure e = case e of
          ExitFailure _ -> True
          _             -> False

    it "should show help message" $
      const (pp ["--help"])

    it "should show result to stdout" $
      const (pp ["--verbose=3", pdir </> "fnk01.hs"])

    it "should write out to file" $ \opath ->
      pp [pdir </> "fnk01.hs", opath]

    it "should fail when no files were specified" $
      const (pp [] `shouldThrow` anyExitFailure)

    it "should fail when no arg were passed to pragma option" $
      const (pp ["fnk01.hs", "--pragma"] `shouldThrow` anyExitFailure)

    -- Finkel source code
    -- Parsing plugin01.hs, header only.
    doPreprocess ["--verbose=3"] "fnk01.hs"
      "module Main where\n\n"

    -- Parsing plugin01.hs, full module.
    doPreprocess ["--full", "--warn-interp"] "fnk01.hs"
      (ul [ "module Main where"
          , "main :: IO ()"
          , "main = putStrLn \"preprocess/fnk01.hs\""
          , "\n"
          ])

    doPreprocess ["--warn-interp=False"] "fnk02.hs"
      "module Main where\n\n"

    doPreprocess [] "fnk03.hs"
      "\n" -- empty contents

    doPreprocess [] "fnk04.hs"
      (ul [ "module Main ("
          , "        foo, main"
          , "    ) where"
          , "import Data.Maybe ( fromMaybe )"
          , "import qualified Control.Monad as M"
          , "\n" ])

    doPreprocess ["--pragma=DEADBEEF"] "fnk05.hs"
      (ul [ "module Main where"
          , "import Control.Monad"
          , "\n"
          ])

    doPreprocess ["--warn-interp=False"] "fnk06.hs"
      "module Main where\n\n"

    -- Haskell source code
    doPreprocess ["--verbose=3"] "hs01.hs"
      (ul [ "module Main where"
          , ""
          , "main :: IO ()"
          , "main = putStrLn \"preprocess/hs01.hs\"\n"
          ])

    doPreprocess ["--ignore"] "hs02.hs"
      (ul [ "-- Haskell code containing \";;;\" in the first line."
          , ""
          , "module Main where"
          , ""
          , "main :: IO ()"
          , "main = putStrLn \"preprocess/hs02.hs\"\n"
          ])

    -- Finkel source code containing `defmodule'
    doPreprocess [] "fnk11.hs"
      "module Main where\n\n"

    doPreprocess [] "fnk12.hs"
      (ul [ "module Main where"
          , "import Control.Monad ( forM_, when )"
          , "import qualified Data.ByteString as BS"
          , "import Data.Maybe hiding ( fromJust )"
          , "import Control.Applicative ( liftA3 )"
          , "\n"
          ])

    doPreprocess [] "fnk13.hs"
      "module Main where\n\n"

    -- Failures
    doPreprocessAndFail ["--verbose=max"] "fnk01.hs"
    doPreprocessAndFail ["--warn-interp=3"] "fnk01.hs"
    doPreprocessAndFail [] "fnk14.hs"
    doPreprocessAndFail [] "fnk15.hs"

doPreprocess :: [String] -> String -> String -> SpecWith FilePath
doPreprocess extra_args basename expected =
  it ("should parse module header of " ++ basename) $ \opath -> do
    let ipath = pdir </> basename
        path_args = [ipath, ipath, opath]
        args = path_args ++ extra_args
    withArgs args (quietly defaultPreprocess)
    contents <- readFile opath
    contents `shouldBe` expected

doPreprocessAndFail :: [String] -> String -> SpecWith FilePath
doPreprocessAndFail extra_args basename =
  it ("should preprocess and fail with " ++ basename) $ \opath -> do
    let ipath = pdir </> basename
        path_args = [ipath, ipath, opath]
        args = path_args ++ extra_args
    withArgs args (quietly defaultPreprocess)
      `shouldThrow` (\(SomeException _) -> True)

withTemporaryFile :: (FilePath -> IO a) -> IO a
withTemporaryFile = bracket acquire cleanup
  where
    acquire = do
      tmp_dir <- getTemporaryDirectory
      let dir = tmp_dir </> "preprocess"
          file = dir </> "tmp.hs"
      createDirectoryIfMissing True dir
      pure file
    cleanup =
      removeDirectoryRecursive . takeDirectory

pdir :: FilePath
pdir = "test" </> "data" </> "preprocess"
