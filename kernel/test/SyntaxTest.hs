{-# LANGUAGE CPP #-}
-- | Tests for syntax.
--
-- All files under "test/data" directory with '.fnk' extension (i.e.:
-- "test/data/*.fnk") are read and compiled, then type checked.
--
module SyntaxTest (syntaxTests) where

-- base
import Control.Monad        (when)
import Data.IORef           (newIORef, readIORef, writeIORef)
import System.Exit          (ExitCode (..))
import System.Info          (os)

-- directory
import System.Directory     (createDirectoryIfMissing, doesFileExist,
                             getTemporaryDirectory, removeFile)

-- filepath
import System.FilePath      (takeBaseName, (<.>), (</>))

-- ghc
import SrcLoc               (noLoc)

-- hspec
import Test.Hspec

-- process
import System.Process       (readProcessWithExitCode)

-- finkel-kernel
import Language.Finkel.Fnk
import Language.Finkel.Make

-- Internal
import TestAux

mkTest :: FilePath -> Spec
mkTest path =
  if os == "mingw32" && or (let b = takeBaseName path
                            in  [ b == "0002-lexical"
                                , b == "0004-decls"
                                , b == "1001-quote" ])
     then describe path
                   (it "is pending under Windows"
                       (pendingWith "Unicode not supported yet"))
     else mkTest' path

mkTest' :: FilePath -> Spec
mkTest' path = do
  let mkRef = runIO . newIORef . error
      removeWhenExist file = do
        exist <- doesFileExist file
        when exist (removeFile file)
  tmpdir <- runIO getTemporaryDirectory
  fnkORef <- mkRef "fnkORef"
  hsORef <- mkRef "hsORef"
  let fnkEnv = defaultFnkEnv { envSilent = True
                           , envHsDir = Just odir }
      odir = tmpdir </> "fnk_mk_test"
      aDotOut = odir </> "a.out"
      dotHs = odir </> takeBaseName path <.> "hs"
      dotTix = "a.out.tix"
      syndir = "test" </> "data" </> "syntax"
      runDotO = readProcessWithExitCode aDotOut [] ""
  runIO (createDirectoryIfMissing True odir)
  beforeAll_ (removeArtifacts syndir) $ describe path $ do
    it "should compile with Fnk" $ do
      let task = do
            initSessionForTest
            make [(noLoc path, Nothing)] False True (Just aDotOut)
      ret <- runFnk task fnkEnv
      ret `shouldBe` ()

    it "should run executable compiled with Fnk" $ do
      removeWhenExist dotTix
      (ecode, stdout, _stderr) <- runDotO
      writeIORef fnkORef stdout
      ecode `shouldBe` ExitSuccess

    it "should dump Haskell source" $ do
      exist <- doesFileExist dotHs
      exist `shouldBe` True

    it "should compile resulting Haskell code" $ do
      let task = do
            initSessionForTest
            make [(noLoc dotHs, Nothing)] False True (Just aDotOut)
#if MIN_VERSION_ghc(8,4,0)
          skipThisTest _ = (False, "")
#else
          skipThisTest p =
            ( or [ b == "2012-typeop"
                 , b == "1004-doccomment-01"]
            , "Generated Haskell code is malformed" )
            where b = takeBaseName p
#endif
      case skipThisTest path of
        (True, reason) -> pendingWith reason
        _ -> runFnk task fnkEnv >>= \ret -> ret `shouldBe` ()

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
