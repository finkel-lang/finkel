-- | Wrapper executable for sk frontend plugin.
module Main where

import Language.SK.Plugin (skPluginMain)

main :: IO ()
main = skPluginMain "Language.SK.Plugin" "sk-kernel"
