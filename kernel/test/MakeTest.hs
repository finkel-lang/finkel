-- | Tests for 'make'.

module MakeTest
  ( makeTests
  , removeArtifacts
  ) where

-- base
import Control.Monad (void, when)
import Data.List (intercalate, isPrefixOf, tails)
import System.Directory (getDirectoryContents, removeFile)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeExtension)
import System.Process (rawSystem)

-- ghc
import DynFlags ( DynFlags(..), GhcLink(..), Way(..), interpWays
                , parseDynamicFlagsCmdLine )
import FastString (fsLit)
import GHC (setSessionDynFlags )
import GhcMonad (getSessionDynFlags)
import SrcLoc (noLoc)

-- hspec
import Test.Hspec

-- sk-kernel
import Language.SK.Lexer
import Language.SK.Make
import Language.SK.Run
import Language.SK.SKC
import Language.SK.TargetSource

-- Internal
import TestAux

makeTests :: Spec
makeTests = do
  showTargetTest
  buildSk ["main1.sk"]
  buildSk ["main2.sk"]
  buildSk ["main3.sk"]
  buildSk ["main4.sk", "M3.sk"]
  buildC ["cbits1.c"]
  buildPackage "p01"

showTargetTest :: Spec
showTargetTest = do
  let sksrc = SkSource "path1" "Foo" [] sp
      hssrc = HsSource "path2"
      otsrc = OtherSource "path3"
      sp = initialSPState (fsLit "showTargetTest") 1 1
  describe "show TargetSource" $
    it "should contain filepath" $ do
      let subseq xs ys = any (isPrefixOf xs) (tails ys)
      show sksrc `shouldSatisfy` subseq "path1"
      show hssrc `shouldSatisfy` subseq "path2"
      show otsrc `shouldSatisfy` subseq "path3"
  describe "eq TargetSource" $
    it "should return True iff comparing with itself" $ do
      sksrc `shouldBe` sksrc
      sksrc `shouldNotBe` hssrc
      sksrc `shouldNotBe` otsrc

buildSk :: [FilePath] -> Spec
buildSk = buildFile initSessionForTest

buildC :: [FilePath] -> Spec
buildC = buildFile
           (do initSessionForTest
               dflags <- getSessionDynFlags
               void (setSessionDynFlags (dflags {ghcLink=NoLink})))

buildFile :: Skc () -> [FilePath] -> Spec
buildFile pre paths =
  before_ (removeArtifacts odir) $
  describe ("files " ++ intercalate ", " paths) $
    it "should compile successfully" $ do
      ret <- runSkc
               (do pre
                   -- Use dflags setttings for profile when running test
                   -- executable with "+RTS -p" option.
                   if WayProf `elem` interpWays
                      then make_profile targets False Nothing
                      else make_simple targets False Nothing)
                    (defaultSkEnv { envSilent = True })
      ret `shouldBe` Right ()
  where
    targets = map (\path -> (path, Nothing)) paths
    odir = "test" </> "data" </> "build"
    make' flags sources doLink out = do
      dflags0 <- getSessionDynFlags
      let dflags1 = dflags0 {importPaths = [".", odir]}
          flags' = map noLoc flags
      (dflags2,_,_) <- parseDynamicFlagsCmdLine dflags1 flags'
      _ <- setSessionDynFlags dflags2
      make sources doLink True out
    make_simple = make' []
    make_profile = make' ["-prof", "-fprof-auto", "-fprof-cafs"
                         , "-hisuf", "p_hi", "-osuf", "p_o"]

removeArtifacts :: FilePath -> IO ()
removeArtifacts dir = do
  contents <- getDirectoryContents dir
  mapM_ removeObjAndHi contents
  where
    removeObjAndHi file =
      when (takeExtension file `elem` [".o", ".hi", ".p_o", ".p_hi"])
           (removeFile (dir </> file))

buildPackage :: String -> Spec
buildPackage name =
  describe ("package " ++ name) $
    it "should compile and pass the tests" $ do
      _ <- stack ["setup"]
      _ <- stack ["build", name]
      exitCode <- stack ["test", name]
      _ <- stack ["clean", name]
      exitCode `shouldBe` ExitSuccess

stack :: [String] -> IO ExitCode
stack args = rawSystem "stack" ("--silent" : args)
