module ExceptionTest
  ( exceptionTests
  , exceptionFnkTests
  ) where

-- base
import Control.Exception   (bracket)
import System.Environment  (lookupEnv, setEnv)
import System.Exit         (ExitCode (..))

-- hspec
import Test.Hspec

-- -- finkel-kernel
import Language.Finkel.Fnk (initializeLibDirFromGhc)

-- Internal
import TestAux

exceptionTests :: Spec
exceptionTests = beforeAll getFnkTestResource exceptionFnkTests

exceptionFnkTests :: FnkSpec
exceptionFnkTests = do
  noGhcTest
  compileErrorTests

noGhcTest :: FnkSpec
noGhcTest =
  describe "No ghc found in current PATH" $
    it "should fail with non-0 exit code" $ \_ -> do
      let act = withEmptyPATH initializeLibDirFromGhc
      act `shouldThrow` exitFailureSelector

compileErrorTests :: FnkSpec
compileErrorTests = runIO (getTestFiles "exception") >>= mapM_ mkTest

mkTest :: FilePath -> FnkSpec
mkTest path =
  describe path $
    it "should not compile successfully" $ \ftr ->
      let go = ftr_main ftr ["-fno-code", path]
      in  go `shouldThrow` exitFailureSelector

exitFailureSelector :: ExitCode -> Bool
exitFailureSelector (ExitFailure _) = True
exitFailureSelector _               = False

withEmptyPATH :: IO a -> IO a
withEmptyPATH = bracket acquire restore . const
  where
    acquire = do
      mb_path <- lookupEnv "PATH"
      case mb_path of
        Nothing   -> return ""
        Just path -> setEnv "PATH" "/" >> return path
    restore = setEnv "PATH"
