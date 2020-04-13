{-# LANGUAGE CPP #-}
-- | Tests for "Language.Finkel.Main"
module MainTest
  ( mainTests
  ) where

-- base
import Control.Exception    (catch, throw)
import System.Environment   (withArgs)
import System.Exit          (ExitCode (..))

-- filepath
import System.FilePath      ((</>))

-- hspec
import Test.Hspec

-- process
-- import System.Process (readProcess)

-- finkel-kernel
import Language.Finkel.Main

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

runDefaultMain :: [String] -> IO ()
runDefaultMain args =
  -- "defaultMain" uses "System.Process.rawSystem" to delegate non-finkel
  -- related works to ghc, which throws "ExitSuccess" when successfully done.
  catch (withArgs args defaultMain)
        (\e -> case e of
                 ExitSuccess -> return ()
                 _           -> throw e)

odir :: FilePath
odir = "test" </> "data" </> "main"

getPackageEnvFlag :: IO [String]
getPackageEnvFlag =
#if MIN_VERSION_ghc(8,4,4)
  ifUsingStack (return ["-package-env=-"]) (return [])
#else
  return []
#endif
