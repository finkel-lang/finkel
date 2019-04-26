{-# LANGUAGE CPP #-}
-- | Tests for syntax.
--
-- All files under "test/data" directory with '.sk' extension (i.e.:
-- "test/data/*.sk") are read and compiled, then type checked.
--
module SyntaxTest (syntaxTests) where

-- base
import Control.Monad (when)
import Data.IORef (newIORef, readIORef, writeIORef)
import System.Exit (ExitCode(..))

-- directory
import System.Directory ( createDirectoryIfMissing, doesFileExist
                        , getTemporaryDirectory, removeFile )

-- filepath
import System.FilePath ((</>), (<.>), takeBaseName)

-- hspec
import Test.Hspec

-- process
import System.Process (readProcessWithExitCode)

-- sk-kernel
import Language.SK.Make
import Language.SK.SKC

-- Internal
import TestAux

mkTest :: FilePath -> Spec
mkTest path = do
  let mkRef = runIO . newIORef . error
      removeWhenExist file = do
        exist <- doesFileExist file
        when exist (removeFile file)
  tmpdir <- runIO getTemporaryDirectory
  skORef <- mkRef "skORef"
  hsORef <- mkRef "hsORef"
  let skEnv = defaultSkEnv { envSilent = True
                           , envHsDir = Just odir }
      odir = tmpdir </> "sk_mk_test"
      aDotOut = odir </> "a.out"
      dotHs = odir </> takeBaseName path <.> "hs"
      dotTix = "a.out.tix"
      syndir = "test" </> "data" </> "syntax"
      runDotO = readProcessWithExitCode aDotOut [] ""
  runIO (createDirectoryIfMissing True odir)
  beforeAll_ (removeArtifacts syndir) $ describe path $ do
    it "should compile with skc" $ do
      let task = do
            initSessionForTest
            make [(path, Nothing)] False True (Just aDotOut)
      ret <- runSkc task skEnv
      ret `shouldBe` ()

    it "should run executable compiled with skc" $ do
      removeWhenExist dotTix
      (ecode, stdout, _stderr) <- runDotO
      writeIORef skORef stdout
      ecode `shouldBe` ExitSuccess

    it "should dump Haskell source" $ do
      exist <- doesFileExist dotHs
      exist `shouldBe` True

    it "should compile resulting Haskell code" $ do
      let task = do
            initSessionForTest
            make [(dotHs, Nothing)] False True (Just aDotOut)
      ret <- runSkc task skEnv
      ret `shouldBe` ()

    it "should run executable compiled from Haskell code" $ do
      removeWhenExist dotTix
      (ecode, stdout, _stderr) <- runDotO
      writeIORef hsORef stdout
      ecode `shouldBe` ExitSuccess

    it "should have same output from skc and ghc executables" $ do
      sko <- readIORef skORef
      hso <- readIORef hsORef
      sko `shouldBe` hso

syntaxTests :: [FilePath] -> Spec
syntaxTests = mapM_ mkTest
