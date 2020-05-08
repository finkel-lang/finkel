{-# LANGUAGE CPP #-}
-- | Tests for "Language.Finkel.Main"
module MainTest
  ( mainTests
  ) where

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
