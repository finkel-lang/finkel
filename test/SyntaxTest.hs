-- | Tests for syntax.
--
-- All files under "test/data" directory with '.sk' extension (i.e.:
-- "test/data/*.sk") are read and compiled, then type checked.
--
module SyntaxTest (syntaxTests) where

import Test.Hspec
import Language.SK.Run

readCode :: FilePath -> IO Bool
readCode src = do
  let go = do (mdl, _st) <- compileSkModule src
              tcHsModule (Just src) False mdl
  compiled <- runSkc go initialSkEnv
  case compiled of
    Right _tc -> return True
    Left e -> putStrLn e >> return False

mkTest :: String -> Spec
mkTest name =
  before (readCode name) $
    describe name $
      it "should compile and type check"
         (\result -> result `shouldBe` True)

syntaxTests :: [FilePath] -> Spec
syntaxTests = mapM_ mkTest
