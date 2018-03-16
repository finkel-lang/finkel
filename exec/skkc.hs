-- | Wrapper executable for skkc, SK kernel compiler frontend plugin.
module Main where

import Language.SK.Plugin (skPluginMain)

main :: IO ()
main = skPluginMain "Language.SK.Plugin" "sk-kernel"
