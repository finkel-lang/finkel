{-# LANGUAGE CPP #-}
-- | Tests for "Language.Finkel.Main"
module MainTest
  ( mainFnkTests
  ) where

-- base
import System.Exit     (ExitCode (..))

-- filepath
import System.FilePath ((</>))

-- hspec
import Test.Hspec

-- Internal
import TestAux

mainFnkTests :: FnkSpec
mainFnkTests =
  beforeAll_ (removeArtifacts odir) $ do
    let common_flags = ["-v0", "-fno-code"]
    compileFile common_flags "m001.hs"
    compileFile common_flags "m002.hs"
    compileFile ("-c" : common_flags) "m003.c"
    compileFile ("-main-is" : "MyMain.my-main" : common_flags) "MyMain.hs"
    rawGhcTest
    finkelHelpTest
    finkelVersionTest
    finkelSupportedLanguagesTest
    finkelInfoTest
    finkelUnknownFlagTest

compileFile :: [String] -> FilePath -> FnkSpec
compileFile args file = describe ("file " ++ file) $
  it "should compile successfully" $ \ftr ->
    ftr_main ftr (args ++ pure (odir </> file))

rawGhcTest :: FnkSpec
rawGhcTest =
  trivialTest "option --version"
              "should show project-version"
              ["--version"]

finkelHelpTest :: FnkSpec
finkelHelpTest =
  trivialTest "option --fnk-help"
              "should show Finkel help"
              ["--fnk-help"]

finkelVersionTest :: FnkSpec
finkelVersionTest =
  trivialTest "option --fnk-version"
              "should show finkel-kernel package version"
              ["--fnk-version"]

finkelSupportedLanguagesTest :: FnkSpec
finkelSupportedLanguagesTest =
  trivialTest "option --fnk-languages"
              "should show supported language extensions"
              ["--fnk-languages"]

finkelInfoTest :: FnkSpec
finkelInfoTest =
  trivialTest "option --info"
              "should show info of DynFlags"
              ["--info"]

finkelUnknownFlagTest :: FnkSpec
finkelUnknownFlagTest =
  describe "invalid flag" $ do
    it "should exit with failure with unknown flag" $ \ftr ->
      ftr_main ftr ["--fnk-foo"] `shouldThrow` (== ExitFailure 1)
    it "should exit with failure with invalid verbosity level" $ \ftr ->
      ftr_main ftr ["--fnk-verbose=foo"] `shouldThrow` (== ExitFailure 1)

trivialTest :: String -> String -> [String] -> FnkSpec
trivialTest desc label flags = describe desc $
  it label (`ftr_main` flags)

odir :: FilePath
odir = "test" </> "data" </> "main"
