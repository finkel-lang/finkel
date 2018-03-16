-- | Tests for syntax.
--
-- All files under "test/data" directory with '.sk' extension (i.e.:
-- "test/data/*.sk") are read and compiled, then type checked.
--
module SyntaxTest (syntaxTests) where

import Control.Monad (when)
import Data.IORef (newIORef, readIORef, writeIORef)
import System.Directory ( doesFileExist
                        , getTemporaryDirectory
                        , removeFile )
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Test.Hspec

import Language.SK.Builder (HModule)
import Language.SK.Emit
import Language.SK.Lexer
import Language.SK.Make
import Language.SK.Run
import Language.SK.SKC

import MakeTest (removeArtifacts)

readCode :: FilePath -> IO (Bool, Maybe HModule, Maybe SPState)
readCode src = do
  let go = do initSessionForMake
              (mdl, st) <- compileSkModule src
              ret <- tcHsModule (Just src) False mdl
              return (ret, mdl, st)
  compiled <- runSkc go initialSkEnv
  case compiled of
    Right (_tc, mdl, st) -> return (True, Just mdl, Just st)
    Left e -> putStrLn e >> return (False, Nothing, Nothing)

mkTest :: FilePath -> Spec
mkTest path = do
  let mkRef = runIO . newIORef . error
      skEnv = initialSkEnv {envSilent = True}
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
    it "should type check" $ do
      (result, _mdl, _sp) <- readCode path
      result `shouldBe` True

    it "should compile with skc" $ do
      let task = do
            initSessionForMake
            make [(path, Nothing)] False (Just aDotOut)
      ret <- runSkc task skEnv
      ret `shouldBe` Right ()

    it "should run executable compiled with skc" $ do
      removeWhenExist dotTix
      (ecode, stdout, _stderr) <- runDotO
      writeIORef skORef stdout
      ecode `shouldBe` ExitSuccess

    it "should emit Haskell source" $ do
      let gen = do
            initSessionForMake
            (mdl, sp) <- compileWithSymbolConversion path
            genHsSrc sp (Hsrc mdl)
      src <- runSkc gen skEnv
      case src of
        Right src' -> do
          writeFile dotHs src'
          src' `shouldNotBe` ""
        Left err -> expectationFailure err

    it "should compile resulting Haskell code" $ do
      let args = ["--silent", "exec", "ghc", "--", "-o", aDotOut, dotHs]
      (ecode, _, stderr) <- readProcessWithExitCode "stack" args ""
      case stderr of
        [] -> ecode `shouldBe` ExitSuccess
        _  -> expectationFailure stderr

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
