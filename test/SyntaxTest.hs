-- | Tests for syntax.
--
-- All files under "test/data" directory with '.sk' extension (i.e.:
-- "test/data/*.sk") are read and compiled, then type checked.
--
module SyntaxTest (syntaxTests) where

import Data.IORef (newIORef, readIORef, writeIORef)
import System.Directory (getTemporaryDirectory)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Test.Hspec

import Language.SK.Builder
import Language.SK.Emit
import Language.SK.Lexer
import Language.SK.Make
import Language.SK.Run

import MakeTest (removeArtifacts)

readCode :: FilePath -> IO (Bool, Maybe HModule, Maybe SPState)
readCode src = do
  let go = do (mdl, st) <- compileSkModule src
              ret <- tcHsModule (Just src) False mdl
              return (ret, mdl, st)
  compiled <- runSkc go initialSkEnv
  case compiled of
    Right (_tc, mdl, st) -> return (True, Just mdl, Just st)
    Left e -> putStrLn e >> return (False, Nothing, Nothing)

mkTest :: FilePath -> Spec
mkTest path = do
  let mkRef = runIO . newIORef . error
  tmpdir <- runIO getTemporaryDirectory
  mdlRef <- mkRef "mdlRef"
  spRef <- mkRef "spRef"
  skORef <- mkRef "skORef"
  hsORef <- mkRef "hsORef"
  let dotO = tmpdir </> "a.out"
      dotHs = tmpdir </> "tmp.hs"
      syndir = "test" </> "data" </> "syntax"
      runDotO = readProcessWithExitCode dotO [] ""
  beforeAll_ (removeArtifacts syndir) $ describe path $ do
    it "should type check" $ do
      (result, mdl, sp) <- readCode path
      writeIORef mdlRef mdl
      writeIORef spRef sp
      result `shouldBe` True

    it "should compile with skc" $ do
      let task = make [(path, Nothing)] False (Just dotO)
      ret <- runSkc task initialSkEnv
      ret `shouldBe` Right ()

    it "should compile executable via skc successfully" $ do
      (ecode, stdout, _stderr) <- runDotO
      writeIORef skORef stdout
      ecode `shouldBe` ExitSuccess

    it "should emit Haskell source" $ do
      mbMdl <- readIORef mdlRef
      mbSp <- readIORef spRef
      case (mbMdl, mbSp) of
        (Just mdl, Just sp) -> do
          et_src <- runSkc (genHsSrc sp (Hsrc mdl)) initialSkEnv
          case et_src of
            Left err -> do
              let msg = "code generation failed:\n"
              expectationFailure (msg ++ err)
            Right src -> do
              writeFile dotHs src
              src `shouldNotBe` ""
        _ -> "" `shouldNotBe` ""

    it "should compile resulting Haskell code" $ do
      let args = ["exec", "ghc", "--", "-o", dotO, dotHs]
      (ecode, _, stderr) <- readProcessWithExitCode "stack" args ""
      case stderr of
        [] -> ecode `shouldBe` ExitSuccess
        _  -> expectationFailure stderr

    it "should run executable compiled with ghc successfully" $ do
      (ecode, stdout, _stderr) <- runDotO
      writeIORef hsORef stdout
      ecode `shouldBe` ExitSuccess

    it "should have same output from skc and ghc executables" $ do
      sko <- readIORef skORef
      hso <- readIORef hsORef
      sko `shouldBe` hso

syntaxTests :: [FilePath] -> Spec
syntaxTests = mapM_ mkTest
