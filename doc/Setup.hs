{-# LANGUAGE CPP #-}
module Main where

-- Cannot use MIN_VERSION_XXX macros in setup, so using __GLASGOW_HASKELL_ _.
#if 806 <= __GLASGOW_HASKELL__
import Distribution.Simple (defaultMain)
main :: IO ()
main = defaultMain
#else
import Distribution.Simple.Finkel (fnkMainWith)
main :: IO ()
main = fnkMainWith "finkel" ["make"]
#endif
