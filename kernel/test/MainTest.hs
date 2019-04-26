{-# LANGUAGE CPP #-}
-- | Tests for "Language.SK.Main"
module MainTest
  ( mainTests
  ) where

-- base
import Control.Exception (catch, throw)
import System.Environment (withArgs)
import System.Exit (ExitCode(..))

-- filepath
import System.FilePath ((</>))

-- hspec
import Test.Hspec

-- process
-- import System.Process (readProcess)

-- sk-kernel
import Language.SK.Main

-- Internal
import TestAux

mainTests :: Spec
mainTests =
  beforeAll_ (removeArtifacts odir) $ do
    packageEnvFlag <- runIO getPackageEnvFlag
    let common_flags = "-v0" : packageEnvFlag
    compileFile common_flags "m001.sk"
    compileFile common_flags "m002.hs"
    compileFile ("-c" : common_flags) "m003.c"
    rawGhcTest
    skHelpTest
    skVersionTest

compileFile :: [String] -> FilePath -> Spec
compileFile args file = describe ("file " <> file) $
  it "should compile successfully" $
    runDefaultMain (args <> pure (odir </> file)) `shouldReturn` ()

rawGhcTest :: Spec
rawGhcTest =
  trivialTest "option --version"
              "should show project-version"
              ["--version"]

skHelpTest :: Spec
skHelpTest =
  trivialTest "option --sk-help"
              "should show sk help"
              ["--sk-help"]

skVersionTest :: Spec
skVersionTest =
  trivialTest "option --sk-version"
              "should show sk-kernel package version"
              ["--sk-version"]

trivialTest :: String -> String -> [String] -> Spec
trivialTest desc label flags = describe desc $
  it label (runDefaultMain flags `shouldReturn` ())

runDefaultMain :: [String] -> IO ()
runDefaultMain args =
  -- "defaultMain" uses "rawSystem" to delegate non-sk related works to
  -- ghc, which throws "ExitSuccess" when successfully done.
  catch (withArgs args defaultMain)
        (\e -> case e of
                 ExitSuccess -> return ()
                 _ -> throw e)

odir :: FilePath
odir = "test" </> "data" </> "main"

getPackageEnvFlag :: IO [String]
getPackageEnvFlag =
#if MIN_VERSION_ghc (8,4,4)
  ifUsingStack (return ["-package-env=-"]) (return [])
#else
  return []
#endif
