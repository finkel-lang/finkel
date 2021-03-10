module M2 where

import           Data.List
import qualified M1

main :: IO ()
main = do
  M1.main
  putStrLn (concat (intersperse " " ["From", "M2.main"]))
