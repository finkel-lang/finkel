{-# LANGUAGE CPP #-}
-- | Tests for "Language.Finkel.Main"
module MainTest
  ( mainTests
  ) where

-- base
import System.Exit     (ExitCode (..))

-- filepath
import System.FilePath ((</>))

-- hspec
import Test.Hspec

-- Internal
import TestAux

mainTests :: Spec
mainTests =
  beforeAll_ (removeArtifacts odir) $ do
    packageEnvFlag <- runIO getPackageEnvFlag
    let common_flags = "-v0" : "-fno-code" : packageEnvFlag
    compileFile common_flags "m001.fnk"
    compileFile common_flags "m002.hs"
    compileFile ("-c" : common_flags) "m003.c"
    rawGhcTest
    finkelHelpTest
    finkelVersionTest
    finkelSupportedLanguagesTest
    finkelUnknownFlagTest

compileFile :: [String] -> FilePath -> Spec
compileFile args file = describe ("file " ++ file) $
  it "should compile successfully" $
    runDefaultMain (args ++ pure (odir </> file)) `shouldReturn` ()

rawGhcTest :: Spec
rawGhcTest =
  trivialTest "option --version"
              "should show project-version"
              ["--version"]

finkelHelpTest :: Spec
finkelHelpTest =
  trivialTest "option --fnk-help"
              "should show Finkel help"
              ["--fnk-help"]

finkelVersionTest :: Spec
finkelVersionTest =
  trivialTest "option --fnk-version"
              "should show finkel-kernel package version"
              ["--fnk-version"]

finkelSupportedLanguagesTest :: Spec
finkelSupportedLanguagesTest =
  trivialTest "option --fnk-languages"
              "should show supported language extensions"
              ["--fnk-languages"]

finkelUnknownFlagTest :: Spec
finkelUnknownFlagTest =
  describe "invalid flag" $ do
    it "should exit with failure with unknown flag" $
      runDefaultMain ["--fnk-foo"] `shouldThrow` (== ExitFailure 1)
    it "should exit with failure with invalid verbosity level" $
      runDefaultMain ["--fnk-verbose=foo"] `shouldThrow` (== ExitFailure 1)

trivialTest :: String -> String -> [String] -> Spec
trivialTest desc label flags = describe desc $
  it label (runDefaultMain flags `shouldReturn` ())

odir :: FilePath
odir = "test" </> "data" </> "main"

getPackageEnvFlag :: IO [String]
getPackageEnvFlag =
#if MIN_VERSION_ghc(8,4,4)
  ifUsingStack (return ["-package-env=-"]) (return [])
#else
  return []
#endif
