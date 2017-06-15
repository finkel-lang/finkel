-- | Wrapper executable for sk frontend plugin.
--
module Main where

import GHC.Paths (ghc)
import System.Environment (getArgs)
import System.Process (rawSystem)
import System.Exit (exitWith)

main :: IO ()
main = do
  argIns <- getArgs
  let argOuts = [ "--frontend", "SK.Core.FrontendPlugin"
                , "-plugin-package", "sk-core"]
      argOuts' = case argIns of
                    [orig,input,output] ->
                      argOuts ++ ["-ffrontend-opt", "-pgmF"
                                 ,"-ffrontend-opt", orig
                                 ,"-ffrontend-opt", input
                                 ,"-ffrontend-opt", output]
                    [file] -> argOuts ++ ["-ffrontend-opt", file]
                    _ -> error "sk2hs: malformed args"
  exitWith =<< rawSystem ghc argOuts'
