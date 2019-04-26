module M2 where

import qualified M1
import Data.List

main :: IO ()
main = do
  M1.main
  putStrLn (concat (intersperse " " ["From", "M2.main"]))
