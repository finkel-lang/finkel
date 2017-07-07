{-# LANGUAGE CPP #-}
module P01E where

import P01F

p01e :: [String]
#ifdef DEBUG
p01e = ["debug is defined"]
#else
p01e = "p01e" : p01f
#endif
