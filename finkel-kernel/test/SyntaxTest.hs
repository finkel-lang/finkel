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
  ) where

#include "ghc_modules.h"

-- base
import Control.Monad          (unless, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.IORef             (atomicWriteIORef, newIORef, readIORef)
import Data.Maybe             (fromMaybe)
import GHC.Exts               (unsafeCoerce#)
import System.Environment     (getExecutablePath)
import System.Info            (os)
import System.IO              (BufferMode (..), hSetBuffering, stdout)

-- directory
import System.Directory       (createDirectoryIfMissing, doesFileExist,
                               getTemporaryDirectory, removeFile)

-- filepath
import System.FilePath        (takeBaseName, (<.>), (</>))

-- ghc
import GHC                    (setContext, setTargets)
import GHC_Data_StringBuffer  (stringToStringBuffer)
import GHC_Driver_Monad       (GhcMonad (..))
import GHC_Settings_Config    (cProjectVersionInt)
import GHC_Types_Basic        (SuccessFlag (..))
import GHC_Types_Target       (Target (..), TargetId (..))

#if MIN_VERSION_ghc(9,4,0)
import GHC.Driver.Env         (hscActiveUnitId)
#endif

-- hspec
import Test.Hspec

-- silently
import System.IO.Silently     (capture_)

-- finkel-kernel
import Language.Finkel.Eval   (evalExpr)
import Language.Finkel.Fnk    (Fnk, prepareInterpreter, runFnk)
import Language.Finkel.Plugin (plugin, setFinkelPluginWithArgs)
import Language.Finkel.Syntax (parseExpr)

-- Internal
import TestAux

syntaxTests :: Spec
syntaxTests = beforeAll getFnkTestResource syntaxFnkTests

syntaxFnkTests :: FnkSpec
syntaxFnkTests = runIO (getTestHsFiles "syntax") >>= mapM_ mkTest

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
  | base_name == "0003-expressions-2"
  , ghcVersion < 900
  = describe path (it "is pending under ghc < 9.0"
                    (const (pendingWith "Generated Haskell code not working")))
  | base_name `elem` ["1004-doccomment-03", "2005-gadts-02"]
  , ghcVersion >= 904
  = describe path (it "is pending under ghc >= 9.4"
                   (const (pendingWith "Warns with unusable UNPACK")))
  | base_name == "0003-expressions-3"
  , ghcVersion >= 910
  = describe path (it "is not supported in ghc >= 9.10"
                    (const (pendingWith "`forall' is keyword by default")))
  | base_name == "2028-standalonekind"
  , ghcVersion < 810
  = describe path (it "is not supported in ghc < 8.10.1"
                    (const (pendingWith "Not supported")))
  | base_name == "2029-impredicative"
  , ghcVersion < 902
  = describe path (it "is not reliable in ghc < 9.2"
                    (const (pendingWith "Not supported")))
  | otherwise = mkTest' path
  where
    base_name = takeBaseName path

ghcVersion :: Int
ghcVersion = __GLASGOW_HASKELL__

optsToSuppressWarnings :: [(String, [String])]
optsToSuppressWarnings =
  let flag test opt = if test then [opt] else []
      no_forall_identifier = flag (ghcVersion >= 904) "-Wno-forall-identifier"
      no_star_is_type = flag (ghcVersion >= 906) "-Wno-star-is-type"
      no_deprecated = flag (ghcVersion >= 906) "-Wno-deprecated-flags"
  in [ ("0003-expressions-3", no_forall_identifier)
     , ("2010-kindsig", no_star_is_type)
     , ("2015-typefam", no_star_is_type)

      -- TypeInType is deprecated in ghc >= 9.6
     , ("2017-polykinds", no_deprecated)
     ]

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
      prepare = do
        removeArtifacts syndir
        mapM_ removeWhenExist [dotTix, aDotOut, dotHs]
      toNativeCompile = takeBaseName path == "0008-ffi"
      extra_opts =
        fromMaybe [] (lookup (takeBaseName path) optsToSuppressWarnings)
      compile =
          if toNativeCompile
            then nativeCompile extra_opts
            else byteCompile extra_opts

  runIO (do createDirectoryIfMissing True odir
            hSetBuffering stdout NoBuffering)

  beforeAll_ prepare $ describe path $ do
    it "should compile Finkel code" $ \ftr -> do
      io <- runFnk (compile ftr path (Just odir)) fnkTestEnv
      unless toNativeCompile $ do
        capture_ io >>= atomicWriteIORef fnkORef

    it "should dump Haskell source" $ \_ -> do
      exist <- doesFileExist dotHs
      exist `shouldBe` True

    it "should compile dumped Haskell code" $ \ftr -> do
      io <- runFnk (compile ftr dotHs Nothing) fnkTestEnv
      unless toNativeCompile $
        capture_ io >>= atomicWriteIORef hsORef

    it "should have same output" $ \_ -> do
      unless toNativeCompile $ do
        fnk <- readIORef fnkORef
        hs <- readIORef hsORef
        fnk `shouldBe` hs

nativeCompile
  :: [String] -> FnkTestResource -> FilePath -> Maybe FilePath -> Fnk (IO ())
nativeCompile = compileWith False

byteCompile
  :: [String] -> FnkTestResource -> FilePath -> Maybe FilePath -> Fnk (IO ())
byteCompile opts = compileWith True (["-no-link", "-fbyte-code"] ++ opts)

compileWith
  :: Bool -> [String] -> FnkTestResource -> FilePath -> Maybe FilePath
  -> Fnk (IO ())
compileWith is_interpreting ini_args ftr file mb_dir = do
  parseAndSetDynFlags ini_args
  ftr_init ftr
  when is_interpreting prepareInterpreter
  let plugin_opts = maybe [] (\d -> ["--hsdir=" <> d]) mb_dir
  me <- liftIO getExecutablePath
  parseAndSetDynFlags ["-v0", "-F", "-pgmF", me, "-optF", "--no-warn-interp"]
  setFinkelPluginWithArgs plugin plugin_opts
  _hsc_env <- getSession
  let target = Target { targetId = TargetFile file Nothing
                      , targetAllowObjCode = not is_interpreting
#if MIN_VERSION_ghc(9,4,0)
                      , targetUnitId = hscActiveUnitId _hsc_env
#endif
                      , targetContents = Nothing }
  setTargets [target]
  success_flag <- ftr_load ftr [file]
  case success_flag of
    Failed    -> error $ "Failed to compile: " ++ file
    Succeeded ->
      if is_interpreting
        then do
          -- Flush the stdout used by the compiled expression to get the string
          -- output, which is captured later.
          setContext [mkIIDecl "Main", mkIIDecl "System.IO"]
          let act = fmap unsafeCoerce# . evalExpr
              buf = stringToStringBuffer "(>> main (hFlush stdout))"
          evalWith (file ++ ":main") parseExpr act buf
        else return (return ())
