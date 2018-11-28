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
import System.Directory ( doesFileExist
                        , getTemporaryDirectory
                        , removeFile )

-- filepath
import System.FilePath ((</>))

-- hspec
import Test.Hspec

-- process
import System.Process (readProcessWithExitCode)

-- sk-kernel
import Language.SK.Builder (HModule)
import Language.SK.Emit
import Language.SK.Lexer
import Language.SK.Make
import Language.SK.Run
import Language.SK.SKC

-- Internal
import TestAux (ifUsingStack, initSessionForTest)
import MakeTest (removeArtifacts)

mkTest :: FilePath -> Spec
mkTest path = do
  let mkRef = runIO . newIORef . error
      skEnv = defaultSkEnv {envSilent = True}
      removeWhenExist file = do
        exist <- doesFileExist file
        when exist (removeFile file)
  tmpdir <- runIO getTemporaryDirectory
  skORef <- mkRef "skORef"
  hsORef <- mkRef "hsORef"
  let aDotOut = tmpdir </> "a.out"
      dotHs = tmpdir </> "tmp.hs"
      dotTix = "a.out.tix"
      syndir = "test" </> "data" </> "syntax"
      runDotO = readProcessWithExitCode aDotOut [] ""
  beforeAll_ (removeArtifacts syndir) $ describe path $ do
    it "should compile with skc" $ do
      let task = do
            initSessionForTest
            make [(path, Nothing)] False True (Just aDotOut)
      ret <- runSkc task skEnv
      ret `shouldBe` Right ()

    it "should run executable compiled with skc" $ do
      removeWhenExist dotTix
      (ecode, stdout, _stderr) <- runDotO
      writeIORef skORef stdout
      ecode `shouldBe` ExitSuccess

    it "should emit Haskell source" $ do
      let gen = do
            initSessionForTest
            (mdl, sp) <- compileWithSymbolConversion path
            genHsSrc sp (Hsrc mdl)
      src <- runSkc gen skEnv
      case src of
        Right src' -> do
          writeFile dotHs src'
          src' `shouldNotBe` ""
        Left err -> expectationFailure err

    it "should compile resulting Haskell code" $ do
      let task = do
            initSessionForTest
            make [(dotHs, Nothing)] False True (Just aDotOut)
      ret <- runSkc task skEnv
      ret `shouldBe` Right ()

    it "should run executable compiled with ghc" $ do
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
