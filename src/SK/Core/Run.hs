-- | Module exporting the @runSkc@, Haskell compiler, and some utility
-- functions.
module SK.Core.Run
  ( runSkc
  , initialSkEnv
  , sExpression
  , compileSkModule
  , compileAndEmit
  , mkModSummary
  , tcHsModule
  ) where

import GHC.Paths (libdir)

-- containers
import qualified Data.Map as Map

-- time
import Data.Time (getCurrentTime)

-- transformer
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

-- | Make 'ModSummary'. 'UnitId' is main unit.
mkModSummary :: GhcMonad m => Maybe FilePath -> HsModule RdrName
             -> m ModSummary
mkModSummary mbfile mdl = do
  let modName = case hsmodName mdl of
                      Just name -> unLoc name
                      Nothing   -> mkModuleName "Main"
      fn = maybe "anonymous" id mbfile
      mmod = mkModule mainUnitId modName
      prelude = noLoc (mkModuleName "Prelude")
      imports = map importedName (hsmodImports mdl)
      importedName lm = ideclName (unLoc lm)
      imported = map (\x -> (Nothing, x)) imports

  dflags0 <- getSessionDynFlags
  mloc <- liftIO (mkHomeModLocation dflags0 modName fn)
  timestamp <-
    liftIO (maybe getCurrentTime getModificationUTCTime mbfile)
  dflags1 <-
    if isHsSource fn
      then do
        opts <- liftIO (getOptionsFromFile dflags0 fn)
        (dflags1,_,_) <- liftIO (parseDynamicFilePragma dflags0 opts)
        return dflags1
      else return dflags0

  -- XXX: Have not tested with complex module importing modules from
  -- non-standard packages.
  return ModSummary { ms_mod = mmod
                    , ms_hsc_src = HsSrcFile
                    , ms_location = mloc
                    , ms_hs_date = timestamp
                    , ms_obj_date = Nothing
                    , ms_iface_date = Nothing
                    , ms_srcimps = []
                    , ms_textual_imps = (Nothing, prelude) : imported
                    , ms_hspp_file = fn
                    , ms_hspp_opts = dflags1
                    , ms_hspp_buf = Nothing }

isHsSource :: FilePath -> Bool
isHsSource path = "hs" == suffix
  where
    suffix = reverse (takeWhile (/= '.') (reverse path))

-- | Action to type check module.
--
-- Error location are derived from 'HsModule', locations precisely match
-- with S-expression source code, pretty much helpful.
---
tcHsModule :: GhcMonad m
           => Maybe FilePath -- ^ Source of the module.
           -> Bool -- ^ True to generate files, otherwise False.
           -> HsModule RdrName -- ^ Module to typecheck.
           -> m TypecheckedModule
tcHsModule mbfile genFile mdl = do
  let fn = maybe "anon" id mbfile
      langExts = languageExtensions (Just Haskell2010)
  dflags0 <- getSessionDynFlags
  -- XXX: Does not take care of user specified DynFlags settings.
  let dflags1 =
       if genFile
          then dflags0 {hscTarget = HscAsm, ghcLink = LinkBinary}
          else dflags0 {hscTarget = HscNothing, ghcLink = NoLink}
  _ <- setSessionDynFlags (foldl xopt_set dflags1 langExts)
  ms <- mkModSummary mbfile mdl
  let ann = (Map.empty, Map.empty)
      r_s_loc = mkSrcLoc (fsLit fn) 1 1
      r_s_span = mkSrcSpan r_s_loc r_s_loc
      pm = ParsedModule { pm_mod_summary = ms
                        , pm_parsed_source = L r_s_span mdl
                        , pm_extra_src_files = [fn]
                        , pm_annotations = ann }
  tc <- typecheckModule pm
  _ <- setSessionDynFlags dflags0
  return tc
