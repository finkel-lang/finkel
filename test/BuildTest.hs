module BuildTest where

import System.Exit (ExitCode(..))
import System.Process (rawSystem)

import Test.Hspec

stack :: [String] -> IO ExitCode
stack args = do
  let yaml = "test/data/build/p01/stack.yaml"
  rawSystem "stack" (("--stack-yaml=" ++ yaml):args)

buildTests :: Spec
buildTests =
  describe "package p01" $ do
    it "should compile and pass the tests" $ do
      _ <- stack ["--silent", "setup"]
      exitCode <- stack ["--silent", "build"]
      _ <- stack ["clean", "sample-one"]
      exitCode `shouldBe` ExitSuccess
