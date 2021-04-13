{-# LANGUAGE CPP               #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Tests for syntax.
--
-- All files under "test/data" directory with '.fnk' extension (i.e.:
-- "test/data/*.fnk") are read and compiled, then type checked.
--
module SyntaxTest
  ( syntaxTests
  , syntaxFnkTests
  , ignored_84x
  , ignored_82x
  ) where

#include "ghc_modules.h"

-- base
import Control.Monad          (unless, when)
import Data.IORef             (atomicWriteIORef, newIORef, readIORef)
import GHC.Exts               (unsafeCoerce#)
import System.IO              (BufferMode (..), hSetBuffering, stdout)
import System.Info            (os)

-- directory
import System.Directory       (createDirectoryIfMissing, doesFileExist,
                               getTemporaryDirectory, removeFile)

-- filepath
import System.FilePath        (takeBaseName, (<.>), (</>))

-- ghc
import GHC                    (setContext, setTargets)
import GHC_Data_StringBuffer  (stringToStringBuffer)
import GHC_Driver_Types       (Target (..), TargetId (..))
import GHC_Settings_Config    (cProjectVersionInt)
import GHC_Types_Basic        (SuccessFlag (..))

-- hspec
import Test.Hspec

-- silently
import System.IO.Silently     (capture_)

-- finkel-kernel
import Language.Finkel.Eval   (evalExpr)
import Language.Finkel.Fnk    (Fnk, FnkEnv (..), modifyFnkEnv,
                               prepareInterpreter, runFnk)
import Language.Finkel.Syntax (parseExpr)

-- Internal
import TestAux


syntaxTests :: Spec
syntaxTests = beforeAll getFnkTestResource syntaxFnkTests

syntaxFnkTests :: FnkSpec
syntaxFnkTests = runIO (getTestFiles "syntax") >>= mapM_ mkTest

mkTest :: FilePath -> FnkSpec
mkTest path
  | os == "mingw32"
  , base_name `elem` ["0002-lexical", "0004-decls", "1001-quote"]
  = describe path (it "is pending under Windows"
                       (const (pendingWith "Unicode not supported yet")))
  | cProjectVersionInt == "810"
  , os == "mingw32"
  , base_name `elem` ["1002-macro", "1003-eval-when-compile"]
  = describe path (it "is pending with ghc-8.10.1 under Windows"
                      (const (pendingWith "Macro expansion not yet supported")))
  | base_name `elem` ignored
  = describe path (it "is not supported in this version of ghc"
                      (const (pendingWith "Not supported")))
  | otherwise = mkTest' path
  where
    base_name = takeBaseName path
#if MIN_VERSION_ghc(8,6,0)
    ignored = []
#elif MIN_VERSION_ghc(8,4,0)
    ignored = ignored_84x
#else
    ignored = ignored_82x
#endif

ignored_84x, ignored_82x :: [String]
ignored_84x = ["2024-derivingvia"]
ignored_82x = ["2020-emptyderiv"] ++ ignored_84x

mkTest' :: FilePath -> FnkSpec
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
      -- runDotO = readProcessWithExitCode aDotOut [] ""
      prepare =
        do removeArtifacts syndir
           mapM_ removeWhenExist [dotTix, aDotOut, dotHs]
      toNativeCompile =
         takeBaseName path `elem` ["0008-ffi"]
      compile =
          if toNativeCompile
            then nativeCompile
            else byteCompile

#if MIN_VERSION_ghc(8,8,0)
      skipThisTest _ = (False, "")
#elif MIN_VERSION_ghc(8,6,0)
      skipThisTest p =
          ( takeBaseName p == "2019-overlabel"
          , "Generated Haskell code is malformed" )
#else
      skipThisTest p =
          ( or [ b == "1004-doccomment-01"
               , b == "2012-typeop"
               , b == "2019-overlabel" ]
          , "Generated Haskell code is malformed" )
          where b = takeBaseName p
#endif
      skipOr act =
        case skipThisTest path of
          (True, reason) -> pendingWith reason
          _              -> act

  runIO (do createDirectoryIfMissing True odir
            hSetBuffering stdout NoBuffering)
  beforeAll_ prepare $ describe path $ do
    it "should compile .fnk file" $ \ftr -> do
      io <- runFnk (compile ftr path (Just odir)) fnkTestEnv
      unless toNativeCompile $ do
        capture_ io >>= atomicWriteIORef fnkORef

    it "should dump Haskell source" $ \_ -> do
      exist <- doesFileExist dotHs
      exist `shouldBe` True

    it "should compile dumped Haskell code" $ \ftr -> skipOr $ do
      io <- runFnk (compile ftr dotHs Nothing) fnkTestEnv
      unless toNativeCompile $
        capture_ io >>= atomicWriteIORef hsORef

    it "should have same output" $ \_ -> skipOr $ do
      unless toNativeCompile $ do
        fnk <- readIORef fnkORef
        hs <- readIORef hsORef
        fnk `shouldBe` hs

nativeCompile :: FnkTestResource -> FilePath -> Maybe FilePath -> Fnk (IO ())
nativeCompile = compileWith False []

byteCompile :: FnkTestResource -> FilePath -> Maybe FilePath -> Fnk (IO ())
byteCompile = compileWith True ["-no-link", "-fbyte-code"]

compileWith
  :: Bool -> [String] -> FnkTestResource -> FilePath -> Maybe FilePath
  -> Fnk (IO ())
compileWith is_interpreting ini_args ftr file mb_dir = do
  parseAndSetDynFlags ini_args
  ftr_init ftr
  when is_interpreting prepareInterpreter
  let update_dir dir = modifyFnkEnv (\e -> e {envHsOutDir = Just dir})
  mapM_ update_dir mb_dir
  parseAndSetDynFlags ["-v0"]
  setTargets [Target (TargetFile file Nothing) (not is_interpreting) Nothing]
  success_flag <- ftr_load ftr [file]
  case success_flag of
    Failed    -> error $ "Failed to compile: " ++ file
    Succeeded ->
      if is_interpreting
        then do
          -- Flush the stdout used by the compiled expression to get the string
          -- output, which is captured later.
          setContext [mkIIDecl "Main", mkIIDecl "System.IO"]
          let act = unsafeCoerce# . evalExpr
              buf = stringToStringBuffer "(>> main (hFlush stdout))"
          evalWith (file ++ ":main") parseExpr act buf
        else return (return ())
