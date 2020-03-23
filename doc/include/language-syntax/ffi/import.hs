foreign import ccall safe "string.h strlen" -- Haskell
  cstrlen :: Ptr CChar -> IO CSize
