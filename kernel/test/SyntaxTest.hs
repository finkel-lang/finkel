{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Tests for syntax.
--
-- All files under "test/data" directory with '.fnk' extension (i.e.:
-- "test/data/*.fnk") are read and compiled, then type checked.
--
module SyntaxTest (syntaxTests) where

-- base
import Control.Monad    (when)
import Data.IORef       (newIORef, readIORef, writeIORef)
import System.Exit      (ExitCode (..))
import System.Info      (os)

-- directory
import System.Directory (createDirectoryIfMissing, doesFileExist,
                         getTemporaryDirectory, removeFile)

-- filepath
import System.FilePath  (takeBaseName, (<.>), (</>))

-- ghc
import Config           (cProjectVersionInt)

-- hspec
import Test.Hspec

-- process
import System.Process   (readProcessWithExitCode)

-- Internal
import TestAux

mkTest :: FilePath -> Spec
mkTest path
  | os == "mingw32"
  , base_name `elem` ["0002-lexical", "0004-decls", "1001-quote"]
  = describe path (it "is pending under Windows"
                       (pendingWith "Unicode not supported yet"))
  | cProjectVersionInt == "810"
  , os == "mingw32"
  , base_name `elem` ["1002-macro", "1003-eval-when-compile"]
  = describe path (it "is pending with ghc-8.10.1 under Windows"
                      (pendingWith "Macro expansion not yet supported"))
  | otherwise = mkTest' path
  where
    base_name = takeBaseName path

mkTest' :: FilePath -> Spec
mkTest' path = do
  let mkRef = runIO . newIORef . error
      removeWhenExist file = do
        exist <- doesFileExist file
        when exist (removeFile file)
  tmpdir <- runIO getTemporaryDirectory
  fnkORef <- mkRef "fnkORef"
  hsORef <- mkRef "hsORef"
  let odir = tmpdir </> "fnk_mk_test"
      aDotOut = odir </> "a.out"
      dotHs = odir </> takeBaseName path <.> "hs"
      dotTix = "a.out.tix"
      syndir = "test" </> "data" </> "syntax"
      runDotO = readProcessWithExitCode aDotOut [] ""
      prepare =
        do removeArtifacts syndir
           mapM_ removeWhenExist [dotTix, aDotOut, dotHs]
  runIO (createDirectoryIfMissing True odir)
  beforeAll_ prepare $ describe path $ do
    it "should compile .fnk file" $ do
      let args = [ "--fnk-hsdir=" ++ odir, "-o", aDotOut, "-v0" , path]
      ret <- runDefaultMain args
      ret `shouldBe` ()

    it "should run executable compiled with Fnk" $ do
      removeWhenExist dotTix
      (ecode, stdout, _stderr) <- runDotO
      writeIORef fnkORef stdout
      ecode `shouldBe` ExitSuccess

    it "should dump Haskell source" $ do
      exist <- doesFileExist dotHs
      exist `shouldBe` True

    it "should compile dumped Haskell code" $ do
      let task = do
            ret <- runDefaultMain ["-o", aDotOut, "-v0", dotHs]
            ret `shouldBe` ()
#if MIN_VERSION_ghc(8,8,0)
          skipThisTest _ = (False, "")
#elif MIN_VERSION_ghc(8,6,0)
          skipThisTest p =
            ( takeBaseName p == "2019-overlabel"
            , "Generated Haskell code is malformed")
#else
          skipThisTest p =
            ( or [ b == "1004-doccomment-01"
                 , b == "2012-typeop"
                 , b == "2019-overlabel"]
            , "Generated Haskell code is malformed" )
            where b = takeBaseName p
#endif
      case skipThisTest path of
        (True, reason) -> pendingWith reason
        _              -> task

    it "should run executable compiled from Haskell code" $ do
      removeWhenExist dotTix
      (ecode, stdout, _stderr) <- runDotO
      writeIORef hsORef stdout
      ecode `shouldBe` ExitSuccess

    it "should have same output from fnkc and ghc executables" $ do
      fnko <- readIORef fnkORef
      hso <- readIORef hsORef
      fnko `shouldBe` hso

syntaxTests :: [FilePath] -> Spec
syntaxTests = mapM_ mkTest
