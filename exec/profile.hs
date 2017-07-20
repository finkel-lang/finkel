module Main where

import System.Environment (getArgs)

import qualified SK.Core.Lexer as Lexer
import qualified SK.Core.Reader as Reader

main :: IO ()
main =
  do [file] <- getArgs
     printNumSexprs file

printNumSexprs :: FilePath -> IO ()
printNumSexprs path =
    do contents <- readFile path
       case Lexer.evalSP Reader.sexprs (Just path) contents of
         Right forms -> print (sum (map length forms))
         Left err    -> putStrLn err
