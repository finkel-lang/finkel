module Main where

import Control.Monad (when)
import System.Environment (getArgs)
import System.IO (IOMode(..), hPutStrLn, withFile)

import SK.Core

headerLine :: String -> IO ()
headerLine str = putStrLn ("========== " ++ str ++ " ==========")

pgmF :: [String] -> IO ()
pgmF args = do
  let verbose = any (== "-v") args
      (_:inpath:outpath:_) = args
  contents <- readFile inpath
  when verbose $ do
    headerLine "CLI arguments"
    print args
    headerLine "S-expression source"
    putStrLn contents
  compiled <- compileAndEmit (Just inpath) contents
  case compiled of
    Right hsrc ->
      do when verbose $ do
           headerLine "Haskell source"
           putStrLn hsrc
         let go hdl =
               do hPutStrLn hdl ("{-# LINE 1 \"" ++ inpath ++ "\" #-}")
                  hPutStrLn hdl hsrc
         withFile outpath WriteMode go
    Left err   -> putStrLn err

showCompiled :: (Maybe FilePath -> String -> IO (Either String String))
             -> FilePath -> IO ()
showCompiled compiler file = do
  compiled <- readFile file >>= compiler (Just file)
  case compiled of
    Right hsrc -> putStrLn hsrc
    Left err   -> putStrLn err

main :: IO ()
main = do
  args <- getArgs
  case args of
    []     -> sExpression =<< getContents
    [file] -> sExpression =<< readFile file
    ["--ghc",file] -> showCompiled compileAndEmit file
    _ -> pgmF args
