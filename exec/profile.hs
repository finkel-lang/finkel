module Main where

import Control.Monad.IO.Class (MonadIO(..))
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as BL

import qualified Language.SK.Lexer as Lexer
import qualified Language.SK.Macro as Macro
import qualified Language.SK.Reader as Reader
import qualified Language.SK.Run as Run
import qualified Language.SK.Syntax as Syntax

main :: IO ()
main =
  do args <- getArgs
     case args of
       ["sexpr", file] -> printNumSexprs file
       ["incr", file] -> incrementalCount file
       ["hsrc", file] -> parseHsModule file
       _ -> usage

usage :: IO ()
usage = putStrLn "usage: profile MODE FILE"

printNumSexprs :: FilePath -> IO ()
printNumSexprs path =
  do contents <- BL.readFile path
     case Lexer.evalSP Reader.sexprs (Just path) contents of
       Right forms -> print (sum (map length forms))
       Left err -> putStrLn err

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
              Macro.withExpanderSettings (Macro.expands forms)
            case Syntax.evalBuilder Syntax.parseModule expanded of
              Right _   -> liftIO (putStrLn "done.")
              Left  err -> liftIO (putStrLn ("error: " ++ err))
          Left err -> liftIO (putStrLn ("error: " ++ err))

incrementalCount :: FilePath -> IO ()
incrementalCount file = do
  contents <- BL.readFile file
  let f form acc = acc + length form
  case Lexer.incrSP Reader.psexpr f 0 (Just file) contents of
    Right (n, _) -> print n
    Left err     -> putStrLn err
