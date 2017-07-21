module Main where

import Control.Monad.IO.Class (MonadIO(..))
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as BL

import qualified SK.Core.Lexer as Lexer
import qualified SK.Core.Macro as Macro
import qualified SK.Core.Reader as Reader
import qualified SK.Core.Run as Run
import qualified SK.Core.Syntax as Syntax

main :: IO ()
main =
  do args <- getArgs
     case args of
       ["sexpr", file] -> printNumSexprs file
       ["hsrc", file] -> parseHsModule file
       _ -> usage

usage :: IO ()
usage = putStrLn "usage: profile MODE FILE"

printNumSexprs :: FilePath -> IO ()
printNumSexprs path =
  do contents <- BL.readFile path
     case Lexer.evalSP Reader.sexprs (Just path) contents of
       Right forms -> print (sum (map length forms))
       Left err    -> putStrLn err

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
              Macro.withExpanderSettings (Macro.macroexpands forms)
            case Syntax.evalBuilder Syntax.parseModule expanded of
              Right mdl -> mdl `seq` liftIO (putStrLn "done.")
              Left  err -> liftIO (putStrLn ("error: " ++ err))
          Left err -> liftIO (putStrLn ("error: " ++ err))
