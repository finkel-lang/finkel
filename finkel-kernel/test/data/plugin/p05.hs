-- File containing the magic finkel pragma in the first line.
{-# OPTIONS_GHC -optF --ignore #-}
{-# OPTIONS_GHC -fplugin-opt=Language.Finkel.Plugin:--ignore #-}

module Main where

main :: IO ()
main = putStrLn "plugin/p05.hs"
