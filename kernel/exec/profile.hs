{-# LANGUAGE CPP #-}
-- | Simple executable for profiling.
--
-- Simple executable to wrap some simple actions.
module Main where

-- base
import Control.Monad.IO.Class (MonadIO(..))
import System.Environment (getArgs)
import System.IO (stdout)

-- bytestring
import qualified Data.ByteString.Lazy.Char8 as BL

-- filepath
import qualified System.FilePath as FilePath

-- ghc
import DynFlags (HasDynFlags(..), GeneralFlag(..), gopt_set)
import HsSyn (hsmodHaddockModHeader)
import Outputable (Outputable(..), neverQualify, printForUser)
import SrcLoc (unLoc)
import qualified GHC as GHC


-- sk-kernel
import qualified Language.SK.Builder as Builder
import qualified Language.SK.Emit as Emit
import qualified Language.SK.Expand as Expand
import qualified Language.SK.Lexer as Lexer
import qualified Language.SK.Make as Make
import qualified Language.SK.Reader as Reader
import qualified Language.SK.Run as Run
import qualified Language.SK.SKC as SKC
import qualified Language.SK.Syntax as Syntax

main :: IO ()
main =
  do args <- getArgs
     case args of
       ["parse", file] -> printNumSexprs file
       ["ppr", file]   -> pprFile file
       ["hsrc", file]  -> printHsrc file
       "make" : files  -> doMake files
       _               -> usage

usage :: IO ()
usage =
  putStrLn
    (unlines
       ["usage: profile MODE ARGS"
       ,""
       ,"MODE:"
       ,"  parse - parse file and print number of forms"
       ,"  ppr   - pretty print haskell or sk module with `ppr'"
       ,"  hsrc  - convert SK source to Haskell source"
       ,"  make  - compile given files to object code"])

printNumSexprs :: FilePath -> IO ()
printNumSexprs path =
  do contents <- BL.readFile path
     case Lexer.evalSP Reader.sexprs (Just path) contents of
       Right forms -> print (sum (map length forms))
       Left err    -> putStrLn err

pprFile :: FilePath -> IO ()
pprFile path
  | ext == ".sk" = pprSkModule path
  | ext == ".hs" = pprHsModule path
  | otherwise    = putStrLn "ppr: expeting .sk or .hs file"
  where
    ext = FilePath.takeExtension path

pprSkModule :: FilePath -> IO ()
pprSkModule =
  parseSkModuleWith
    (\m _ ->
       do dflags <- getDynFlags
          liftIO (printForUser dflags stdout neverQualify (ppr m)))

pprHsModule :: FilePath -> IO ()
pprHsModule path =
  do result <- Run.runSkc go Make.defaultSkEnv
     case result of
       Right _  -> return ()
       Left err -> putStrLn err
  where
    go =
      do Make.initSessionForMake
         contents <- liftIO (readFile path)
         dflags0 <- getDynFlags
         let dflags1 = gopt_set dflags0 Opt_Haddock
#if MIN_VERSION_ghc(8,4,0)
             (_warnings, ret) = GHC.parser contents dflags1 path
#else
             ret = case GHC.parser contents dflags1 path of
                       Right (_, lmdl) -> Right lmdl
                       Left err        -> Left err
#endif
         case ret of
           Right lmdl ->
             liftIO
               (do case hsmodHaddockModHeader (unLoc lmdl) of
                     Just _ -> putStrLn "module has header doc."
                     _      -> putStrLn "no header doc."
                   printForUser dflags1 stdout neverQualify (ppr lmdl))
           Left _err  -> liftIO (putStrLn "pprHsModule: error")

printHsrc :: FilePath -> IO ()
printHsrc =
  parseSkModuleWith
    (\mdl sp -> do sk_str <- Emit.genHsSrc sp (Emit.Hsrc mdl)
                   liftIO (putStrLn sk_str))

parseSkModuleWith ::
  (Builder.HModule -> Lexer.SPState -> SKC.Skc ()) -> FilePath -> IO ()
parseSkModuleWith act path =
  do result <- Run.runSkc go Make.defaultSkEnv
     case result of
       Right _  -> return ()
       Left err -> putStrLn err
  where
    go =
     do Make.initSessionForMake
        contents <- liftIO (BL.readFile path)
        case Lexer.runSP Reader.sexprs (Just path) contents of
          Right (forms, sp) -> do
            forms' <- Expand.withExpanderSettings (Expand.expands forms)
            case Syntax.evalBuilder Syntax.parseModule forms' of
              Right mdl -> act mdl sp
              Left  err -> liftIO (putStrLn ("error: " ++ err))
          Left err -> liftIO (putStrLn ("error: " ++ err))

doMake :: [FilePath] -> IO ()
doMake files =
  do let act = do
           Make.initSessionForMake
           Make.make (zip files (repeat Nothing)) False False Nothing
     ret <- Run.runSkc act Make.defaultSkEnv
     case ret of
       Left err -> putStrLn err
       Right _  -> return ()
