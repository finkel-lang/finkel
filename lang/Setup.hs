module Main where

import Data.List (isSubsequenceOf)
import System.Environment (getExecutablePath)
import Distribution.Simple.SK (defaultMainWithHooks, skcHooksWith)

main :: IO ()
main = decideHooks >>= defaultMainWithHooks
  where
    -- Deciding whether this package is compiled with "stack" or
    -- "cabal". Assuming that stack uses a build directory path
    -- containing ".stack-work".
    decideHooks = do
      exec_path <- getExecutablePath
      if ".stack-work" `isSubsequenceOf` exec_path
         then return stackHook
         else return cabalHook
    stackHook =
      skcHooksWith "skkc" [] False
    cabalHook =
      skcHooksWith "cabal" ["new-run", "-v0", "--", "skkc"] False
