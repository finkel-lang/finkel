-- | Module exporting the @runSkc@, Haskell compiler, and some utility
-- functions.
module SK.Core.Run
  ( runSkc
  , initialSkEnv
  , sExpression
  , compileSkModule
  , compileAndEmit
  ) where

import GHC.Paths (libdir)

import Control.Monad.Trans.Class

import SK.Core.Emit
import SK.Core.Form
import SK.Core.GHC
import SK.Core.SKC
import qualified SK.Core.Syntax as S
import qualified SK.Core.Lexer as L
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
          (do ret <- toGhc m env
              return (fmap fst ret))))

-- | Initial 'SkEnv' for performing computation with 'Skc'.
initialSkEnv :: SkEnv
initialSkEnv = SkEnv
  { envMacros = M.specialForms
  , envDebug = False
  }

sExpression :: String -> IO ()
sExpression input =
  case L.evalSP TP.sexprs Nothing input of
    Right forms ->
      do putStrLn "=== pform ==="
         mapM_ (print . pForm) (map lTFormToForm forms)
         putStrLn "=== pprForm ==="
         print (pprForms (map lTFormToForm forms))
    Left err -> putStrLn err

compileAndEmit :: FilePath -> IO (Either String String)
compileAndEmit file = runSkc go initialSkEnv
  where
    go = do (mdl, st) <- compileSkModule file
            genHsSrc st mdl

-- | Compile a file containing SK module.
compileSkModule :: FilePath -> Skc (HsModule RdrName, L.SPState)
compileSkModule file = do
  contents <- liftIO (readFile file)
  (form', st) <- Skc (lift (L.runSP' TP.sexprs (Just file) contents))
  expanded <- M.withExpanderSettings (M.macroexpands form')
  mdl <- Skc (lift (S.evalBuilder' S.parse_module expanded))
  return (mdl, st)
