module Main where

import Control.Monad.IO.Class (MonadIO(..))
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as BL

import qualified Language.SK.Expand as Expand
import qualified Language.SK.Lexer as Lexer
import qualified Language.SK.Make as Make
import qualified Language.SK.Reader as Reader
import qualified Language.SK.Run as Run
import qualified Language.SK.Syntax as Syntax

main :: IO ()
main =
  do args <- getArgs
     case args of
       ["hsrc", file] -> parseHsModule file
       "make" : files -> doMake files
       ["parse", file] -> printNumSexprs file
       _ -> usage

usage :: IO ()
usage =
  putStrLn
    (unlines
       ["usage: profile MODE ARGS"
       ,""
       ,"MODE:"
       ,"  parse - parse file and print number of forms"
       ,"  hsrc  - convert SK source to Haskell source"
       ,"  make  - compile given files to object code"])

parseHsModule :: FilePath -> IO ()
parseHsModule path =
  do result <- Run.runSkc go Run.initialSkEnv
     case result of
       Right _  -> return ()
       Left err -> putStrLn err
  where
    go =
     do contents <- liftIO (BL.readFile path)
        case Lexer.evalSP Reader.sexprs (Just path) contents of
          Right forms -> do
            expanded <-
              Expand.withExpanderSettings (Expand.expands forms)
            case Syntax.evalBuilder Syntax.parseModule expanded of
              Right _   -> liftIO (putStrLn "done.")
              Left  err -> liftIO (putStrLn ("error: " ++ err))
          Left err -> liftIO (putStrLn ("error: " ++ err))

doMake :: [FilePath] -> IO ()
doMake files =
  do let act = do
           Make.initSessionForMake
           Make.make (zip files (repeat Nothing)) False False Nothing
     ret <- Run.runSkc act Run.initialSkEnv
     case ret of
       Left err -> putStrLn err
       Right _  -> return ()

printNumSexprs :: FilePath -> IO ()
printNumSexprs path =
  do contents <- BL.readFile path
     case Lexer.evalSP Reader.sexprs (Just path) contents of
       Right forms -> print (sum (map length forms))
       Left err -> putStrLn err
