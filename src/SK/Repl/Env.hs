{-# LANGUAGE ForeignFunctionInterface #-}
module SK.Repl.Env where

import Control.Monad.IO.Class
import Data.IORef
import Language.SK.Form
import Language.SK.Builder
import Language.SK.GHC
import Unsafe.Coerce

import System.IO.Unsafe
import SK.Repl.GHC

-- When `sk' executable was compiled with `-dynamic' option, compiled
-- REPL code and expressions entered in REPL can share the contents of
-- unsafePerformIO'ed IORef and static variable in C code.

type HscEnvRef = IORef (IORef HscEnv)

funcall1 :: Code -> a -> IO r
funcall1 name arg1 = do
  mb_hval <- lookupHValue name
  case mb_hval of
    Just f -> (unsafeCoerce f) arg1
    Nothing -> error ("funcall1: not in scope " ++ show name)

lookupHValue :: Code -> IO (Maybe HValue)
lookupHValue form =
  case unLocLForm form of
    Atom (ASymbol name) -> do
      let rname = noLoc (mkRdrName name)
      env <- getTheHscEnv
      name': _ <- hscTcRnLookupRdrName env rname
      fref <- getHValue env name'
      Just <$> withForeignRef fref localRef
    _ -> return Nothing

getSomeThing :: String -> IO ()
getSomeThing name = do
  let name' = LForm (noLoc (Atom (aSymbol name)))
  mb_hval <- lookupHValue name'
  case mb_hval of
    Just hval -> do
      let thing :: Int
          thing = unsafeCoerce hval
      putStrLn ("got something, thing=" ++ show thing)
    Nothing -> putStrLn "got nothing."

theHscEnv :: HscEnvRef
theHscEnv =
  unsafePerformIO (newIORef (error "theHscEnv: not initialized"))
{-# NOINLINE theHscEnv #-}

putTheHscEnv :: IORef HscEnv -> IO ()
putTheHscEnv = writeIORef theHscEnv

getTheHscEnv :: IO HscEnv
getTheHscEnv = do
  ref <- readIORef theHscEnv
  readIORef ref
