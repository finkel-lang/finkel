-- | Module exporting the @runSkc@, Haskell compiler, and some utility
-- functions.
module SK.Core.Run
  ( sExpression
  , compile
  , compileAndEmit
  , runSkc
  , initialSkEnv
  ) where

import GHC.Paths (libdir)

import Control.Monad.Trans.Class

import SK.Core.Emit
import SK.Core.Form
import SK.Core.GHC
import SK.Core.SKC
import qualified SK.Core.FormParser as FP
import qualified SK.Core.Lexer as L
import qualified SK.Core.SPState as SP
import qualified SK.Core.TokenParser as TP
import qualified SK.Core.Macro as M

-- | Run @Skc@ with given environment.
runSkc :: Skc a -> SkEnv -> IO (Either String a)
runSkc m env =
  defaultErrorHandler
    defaultFatalMessager
    defaultFlushOut
    (runGhc
       (Just libdir)
       (handleSourceError
          (\se -> do
            flags <- getSessionDynFlags
            return (Left (unlines (map (showSDoc flags)
                                       (pprErrMsgBagWithLoc
                                         (srcErrorMessages se))))))
          (do M.setExpanderSettings
              ret <- toGhc m env
              return (fmap fst ret))))

-- | Initial 'SkEnv' for performing computation with 'Skc'.
initialSkEnv :: SkEnv
initialSkEnv = M.specialForms

sExpression :: String -> IO ()
sExpression input =
  case L.evalSP TP.sexprs Nothing input of
    Right forms ->
      do putStrLn "=== pform ==="
         mapM_ (print . pForm) (map lTFormToForm forms)
         putStrLn "=== pprForm ==="
         print (pprForms (map lTFormToForm forms))
    Left err -> putStrLn err

compileAndEmit :: Maybe FilePath -> String -> IO (Either String String)
compileAndEmit target input = runSkc go M.specialForms
  where
    go = do (mdl, st) <- compile target input
            genHsSrc st mdl

compile :: Maybe FilePath -> String
        -> Skc (HsModule RdrName, SP.SPState)
compile target input = do
  (form', st) <- Skc (lift (L.runSP' TP.sexprs target input))
  expanded <- M.withExpanderSettings (M.macroexpands form')
  mdl <- Skc (lift (FP.evalBuilder' FP.parse_module expanded))
  return (mdl, st)
