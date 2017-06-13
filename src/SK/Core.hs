-- | Module exporting the Haskell compiler, and some utility functions.
module SK.Core
  ( sExpression
  , compile
  , compileAndEmit
  , emit
  ) where

import GHC (HsModule, RdrName)

import SK.Core.Emit
import qualified SK.Core.Form as F
import qualified SK.Core.FormParser as FP
import qualified SK.Core.Lexer as L
import qualified SK.Core.SPState as SP
import qualified SK.Core.TokenParser as TP
import qualified SK.Core.Macro as M

sExpression :: String -> IO ()
sExpression input =
  case L.evalSP TP.sexprs Nothing input of
    Right forms ->
      do putStrLn "=== pform ==="
         mapM_ (print . F.pForm) (map F.lTFormToForm forms)
         putStrLn "=== pprForm ==="
         print (F.pprForms (map F.lTFormToForm forms))
    Left err -> putStrLn err

compileAndEmit :: Maybe FilePath -> String -> IO (Either String String)
compileAndEmit target input = do
  ret <- compile target input
  case ret of
    Right (mdl, st) -> emit mdl st
    Left err        -> return (Left err)

compile :: Maybe FilePath -> String
        -> IO (Either String (HsModule RdrName, SP.SPState))
compile target input =
  -- do (forms, st) <- L.runSP TP.sexprs target input
  --    expanded <- mapM M.macroexpand forms
  --    mdl <- FP.evalBuilder FP.parse_module expanded
  --    return (mdl, st)
  do let form = L.runSP TP.sexprs target input
     case form of
       Right (form', st) -> do
         expanded <- M.evalExpanded (mapM M.macroexpand form')
         case expanded of
           Right expanded' -> do
             let mdl = FP.evalBuilder FP.parse_module expanded'
             case mdl of
               Right mdl' -> return (Right (mdl', st))
               Left  err  -> return (Left err)
           Left err -> return (Left err)
       Left err -> return (Left err)

emit :: HsModule RdrName -> SP.SPState -> IO (Either String String)
emit mdl st = Right <$> genHsSrc st mdl
