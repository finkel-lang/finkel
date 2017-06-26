{-# LINE 1 "test/data/0011-macro.hs" #-}
module Main where
import SK.Core
m1 :: Macro
m1
  = (\ form
       -> case form of {
            (List [x])
              -> return
                   (List
                      [Atom (ASymbol "putStrLn"),
                       List [Atom (ASymbol "++"), Atom (AString "Hello, "), toForm x]])
            _ -> failS
                   ((++)
                      "macroexpand error with `m1' at \"test/data/0011-macro.lisp\":line 7, column 14\n\
                      \arg mismatch: "
                      (show (pForm form))) })
f1 :: Int -> Int -> IO ()
f1 x y = print ((+) x y)
main :: IO ()
main
  = do { putStrLn ((++) "Hello, " "macro");
         f1 11 31 }
