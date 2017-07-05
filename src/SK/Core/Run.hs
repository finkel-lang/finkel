{-# LANGUAGE ScopedTypeVariables #-}
-- | Module exporting the @runSkc@, Haskell compiler, and some utility
-- functions.
module SK.Core.Run
  ( runSkc
  , runSkcWithoutHandler
  , withSourceErrorHandling
  , initialSkEnv
  , skErrorHandler
  , sExpression
  , compileSkModule
  , compileAndEmit
  , parseSexprs
  , buildHsSyn
  , mkModSummary
  , tcHsModule
  ) where

-- base
import Control.Exception
import System.Exit

-- containers
import qualified Data.Map as Map

-- ghc-paths
import GHC.Paths (libdir)

-- time
import Data.Time (getCurrentTime)

-- transformer
import Control.Monad.Trans.Class

import SK.Core.Emit
import SK.Core.Form
import SK.Core.GHC
import SK.Core.SKC
import SK.Core.Syntax
import SK.Core.Lexer as L
import SK.Core.Macro
import SK.Core.Reader

-- | Run 'Skc' with given environment and 'skcErrrorHandler'.
runSkc :: Skc a -> SkEnv -> IO (Either String a)
runSkc m env =
  skErrorHandler
    defaultFatalMessager
    defaultFlushOut
    (runSkcWithoutHandler m env)

-- | Run 'Skc' without exception handler.
runSkcWithoutHandler :: Skc a -> SkEnv -> IO (Either String a)
runSkcWithoutHandler m env =
   runGhc
     (Just libdir)
     (withSourceErrorHandling
        (do ret <- toGhc m env
            return (fmap fst ret)))

-- | Run action with source error handling.
withSourceErrorHandling :: (ExceptionMonad m, GhcMonad m)
                        => m (Either String a) -> m (Either String a)
withSourceErrorHandling m =
  handleSourceError
    (\se -> do
      flags <- getSessionDynFlags
      return (Left (unlines (map (showSDoc flags)
                                 (pprErrMsgBagWithLoc
                                   (srcErrorMessages se))))))
    m


-- | Similar to 'defaultErrorHandler', but won't exit with 'ExitFailure'
-- in exception handler.
skErrorHandler :: ExceptionMonad m
               => FatalMessager -> FlushOut
               -> m (Either String a) -> m (Either String a)
skErrorHandler fm (FlushOut flush) work =
  ghandle
    (\e ->
       liftIO
         (do flush
             case fromException e of
               Just (ioe :: IOException) ->
                 fatalErrorMsg'' fm (show ioe)
               _ ->
                 case fromException e of
                   Just UserInterrupt ->
                     (throwIO UserInterrupt)
                   Just StackOverflow ->
                     fatalErrorMsg'' fm "stack overflow"
                   _ ->
                     case fromException e of
                       Just (ec :: ExitCode) ->
                         (throwIO ec)
                       _ -> fatalErrorMsg'' fm (show e)
             return (Left (show e))))
    (handleGhcException
      (\ge ->
         do liftIO
              (do flush
                  case ge of
                    Signal _ -> fatalErrorMsg'' fm "GhcException signal"
                    _ -> fatalErrorMsg'' fm (show ge))
            return (Left (show ge)))
      work)


-- | Initial 'SkEnv' for performing computation with 'Skc'.
initialSkEnv :: SkEnv
initialSkEnv = SkEnv
  { envMacros = specialForms
  , envDebug = False }

sExpression :: String -> IO ()
sExpression input =
  case evalSP sexprs Nothing input of
    Right forms ->
      do putStrLn "=== pform ==="
         mapM_ (print . pForm) (map unLocForm forms)
         putStrLn "=== pprForm ==="
         print (pprForms (map unLocForm forms))
    Left err -> putStrLn err

compileAndEmit :: FilePath -> IO (Either String String)
compileAndEmit file = runSkc go initialSkEnv
  where
    go = do (mdl, st) <- compileSkModule file
            genHsSrc st mdl

parseSexprs :: Maybe FilePath -> String -> Skc ([LCode], SPState)
parseSexprs mb_file contents =
  Skc (lift (runSP' sexprs mb_file contents))

buildHsSyn :: Builder a -> [LCode] -> Skc a
buildHsSyn bldr forms = Skc (lift (evalBuilder' bldr forms))

-- | Compile a file containing SK module.
compileSkModule :: FilePath -> Skc (HsModule RdrName, SPState)
compileSkModule file = do
  contents <- liftIO (readFile file)
  (form', st) <- parseSexprs (Just file) contents
  expanded <- withExpanderSettings (macroexpands form')
  mdl <- buildHsSyn parseModule expanded
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
