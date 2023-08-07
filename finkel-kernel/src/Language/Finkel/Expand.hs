{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Module for macro expansion.
module Language.Finkel.Expand
  ( expand
  , expand1
  , expands
  , expands'
  , newHscEnvForExpand
  , withExpanderSettings
  , setExpanding
  , isExpanding
  , bcoDynFlags
  , isInterpreted
  , discardInteractiveContext
  ) where

#include "ghc_modules.h"

-- base
import           Control.Concurrent     (MVar, modifyMVar, newMVar)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Char              (isLower)
import           Data.Foldable          (foldlM, for_)
import           Data.IORef             (atomicModifyIORef', newIORef)
import           Data.Maybe             (isJust)
import           System.IO.Unsafe       (unsafePerformIO)

-- containers
import qualified Data.Map               as Map

#if MIN_VERSION_ghc(9,0,0)
import qualified Data.Set               as Set
#endif

-- exception
import           Control.Monad.Catch    (bracket)

-- ghc
import           GHC_Data_FastString    (FastString, headFS)
import           GHC_Driver_Env_Types   (HscEnv (..))
import           GHC_Driver_Main        (newHscEnv)
import           GHC_Driver_Monad       (Ghc (..), GhcMonad (..), Session (..),
                                         getSession, setSession)
import           GHC_Driver_Session     (DynFlags (..), GeneralFlag (..),
                                         GhcLink (..), HasDynFlags (..),
                                         setGeneralFlag', unSetGeneralFlag',
                                         updOptLevel)
import           GHC_Types_SrcLoc       (GenLocated (..))
import           GHC_Utils_Outputable   (Outputable (..), SDoc, cat, fsep, nest,
                                         vcat)

#if MIN_VERSION_ghc(9,4,0)
import           GHC.Driver.Env         (discardIC)
#else
import           GHC_Runtime_Context    (InteractiveContext (..),
                                         emptyInteractiveContext)
import           GHC_Types_Name         (nameIsFromExternalPackage)
#endif

#if MIN_VERSION_ghc(9,2,0) && !MIN_VERSION_ghc(9,4,0)
import           GHC.Driver.Env         (hsc_home_unit)
#endif

#if MIN_VERSION_ghc(9,2,0)
import           GHC.Driver.Backend     (Backend (..))
#else
-- ghc
import           GHC_Driver_Session     (HscTarget (..))
#endif

#if MIN_VERSION_ghc(9,0,0) && !MIN_VERSION_ghc(9,2,0)
import           GHC_Driver_Session     (homeUnit)
#endif

#if MIN_VERSION_ghc(9,0,0)
import           GHC                    (setSessionDynFlags)
import           GHC_Platform_Ways      (Way (..), hostFullWays)
#else
import           GHC_Driver_Session     (Way (..), interpWays, thisPackage)
#endif

#if MIN_VERSION_ghc(9,0,0)
import           GHC.Runtime.Loader     (initializePlugins)
#elif MIN_VERSION_ghc(8,6,0)
import           DynamicLoading         (initializePlugins)
#endif

#if MIN_VERSION_ghc(8,10,0)
import           GHC_Driver_Session     (WarningFlag (..), wopt_unset)
#else
import           GHC_Driver_Session     (Settings (..), rawSettings)
#endif

-- Internal
import           Language.Finkel.Fnk
import           Language.Finkel.Form


-- ---------------------------------------------------------------------
--
-- Session management
--
-- ---------------------------------------------------------------------

-- | Perform given action with 'HscEnv' updated for macroexpansion with
-- interactive evaluation, then reset to the preserved original 'HscEnv'.
withExpanderSettings :: Fnk a -> Fnk a
withExpanderSettings act = do
  fnk_env <- getFnkEnv
  case envInvokedMode fnk_env of
    ExecMode      -> withExpanderSettingsE act
    GhcPluginMode -> withExpanderSettingsG act

-- | Like 'withExpanderSettings', but takes a flag to discard interactive
-- context in the session used for the expansion.
withExpanderSettingsE :: Fnk a -> Fnk a
withExpanderSettingsE act =
  do dflags <- getDynFlags
     -- Switching to the dedicated 'HscEnv' for macro expansion when compiling
     -- object code. If not, assuming current session is using the bytecode
     -- interpreter, using the given action as-is.
     if isInterpreted dflags
        then act
        else bracket (prepare dflags) restore (const act)
  where
    prepare dflags = do
      fnk_env <- getFnkEnv
      hsc_env_old <- getSession

      -- Reusing previous 'HscEnv' for macro expansion if exist, or making a new
      -- 'HscEnv'. When reusing, discarding the previous 'InteractiveContext',
      -- to avoid file local compile time functions to affect other modules.
      case envSessionForExpand fnk_env of
        Just he -> setSession $! discardInteractiveContext he
        Nothing -> do
          he1 <- newHscEnvForExpand dflags
          setSession he1
          postSetSession
          he2 <- getSession
          modifyFnkEnv (\e -> e {envSessionForExpand = Just he2})

      return hsc_env_old

    restore hsc_env_old = do
      hsc_env_new <- getSession
      modifyFnkEnv (\e -> e {envSessionForExpand = Just hsc_env_new})
      setSession hsc_env_old

#if MIN_VERSION_ghc(9,0,0)
    -- To set the "hsc_interp" field in the new session.
    postSetSession = getDynFlags >>= setSessionDynFlags
#else
    postSetSession = return ()
#endif

-- | Make new 'HscEnv' from given 'DynFlags'.
--
-- Adjusting the 'DynFlags' used by the macro expansion session, to support
-- evaluating expressions in dynamic and non-dynamic builds of the Finkel
-- compiler executable.
newHscEnvForExpand :: MonadIO m => DynFlags -> m HscEnv
newHscEnvForExpand dflags0 = do
  let dflags1 = bcoDynFlags dflags0
      dflags2 = if interpHasNoWayDyn
                   then removeWayDyn dflags1
                   else dflags1
  liftIO $! newHscEnv dflags2

-- | Run given 'Fnk' action with macro expansion settings for 'GhcPluginMode'.
withExpanderSettingsG :: Fnk a -> Fnk a
withExpanderSettingsG act = do
  dflags <- getDynFlags
  if isExpanding dflags
    then act
    else withGlobalSession act

-- Note: [Global HscEnv for plugin]
-- --------------------------------
--
-- When compiling with ghc plugin, FnkEnv is unwrapped with "toGhc" and "unGhc"
-- to perform the inner IO action. This way of invokation could not share the
-- FnkEnv when compiling multiple module, so reading from and writing to a
-- global MVar to pass around the "Session" to avoid redundant module compilation
-- when using home package modules during macro expansion.

-- | Wrapper to perform given action with global 'Session', to share the
-- underlying 'HscEnv' when compiling as ghc plugin.
withGlobalSession :: Fnk a -> Fnk a
withGlobalSession act0 = do
  fer <- Fnk pure
  dflags0 <- getDynFlags

  let prepare = initializeGlobalSession
      restore = setSession
      act1 = bracket prepare restore $ \mex0 -> do
        let mex1 = discardInteractiveContext mex0
        setSession mex1

        -- XXX: Shoud keep the old `envDefaultDynFlags' as-is?
        modifyFnkEnv (\e -> e { envSessionForExpand = Just mex1
                              , envDefaultDynFlags = Just (hsc_dflags mex1) })

        retval <- act0
        mex2 <- getSession
        modifyFnkEnv (\e -> e { envSessionForExpand = Just mex2 })
        fnk_env <- getFnkEnv
        pure (retval, fnk_env)

  (retval, fnk_env) <- liftIO $ do
    modifyMVar globalSessionVar $ \mb_s0 -> do
      s1@(Session r1) <- case mb_s0 of
        Just s0 -> pure s0
        Nothing -> newHscEnvForExpand dflags0 >>= fmap Session . newIORef
      (retval, fnk_env) <- unGhc (toGhc act1 fer) s1
      for_ (envSessionForExpand fnk_env) $ \he ->
        atomicModifyIORef' r1 (const (he, ()))
      pure (Just s1, (retval, fnk_env))

  putFnkEnv fnk_env
  pure retval

initializeGlobalSession :: GhcMonad m => m HscEnv
initializeGlobalSession = do
  -- Avoid 'setSessionDynFlags' in ghc < 9.0, since it redundantly loads package
  -- environment.
  --
  -- In ghc >= 9.0, 'setSessionDynFlags' is initializing the interpreter, and
  -- seems like 'setSessionDynFlags' need to be called before initializing
  -- plugins, so calling at the time of session initialization, from GhcMonad.
  initialized <- liftIO $ modifyMVar globalSessionInitializedVar $
    \initialized -> pure (True, initialized)

  if initialized
    then getSession
    else do
#if MIN_VERSION_ghc(9,0,0)
      _ <- getDynFlags >>= setSessionDynFlags
#endif
      getSession >>= initializePlugin'
{-# INLINABLE initializeGlobalSession #-}

-- Version compatible variant of 'initializePlugins'.
initializePlugin' :: MonadIO m => HscEnv -> m HscEnv
#if MIN_VERSION_ghc(9,2,0)
initializePlugin' = liftIO . initializePlugins
#elif MIN_VERSION_ghc(8,6,0)
initializePlugin' hsc_env = do
  plugin_dflags <- liftIO $ initializePlugins hsc_env (hsc_dflags hsc_env)
  return (updateDynFlags plugin_dflags hsc_env)
#else
initializePlugin' = pure
#endif
{-# INLINABLE initializePlugin' #-}

-- | Unsafe global 'Mvar' to track initialization of 'globalSessionVar'.
globalSessionInitializedVar :: MVar Bool
globalSessionInitializedVar = unsafePerformIO (newMVar False)
{-# NOINLINE globalSessionInitializedVar #-}

-- | Unsafe global 'MVar' to share the 'HscEnv' when compiling as plugin.
globalSessionVar :: MVar (Maybe Session)
globalSessionVar = unsafePerformIO (newMVar Nothing)
{-# NOINLINE globalSessionVar #-}

-- XXX: Workaround for passing state to recursively called "load'" function
-- defined in GHC driver. Modifying the "rawSettings" field in the DynFlags with
-- dummy String value, so that recursive call to the "load'" function can tell
-- whether current module is compiled for macro expansion or not.
--
-- The "parMakeCount" field update is a wokaround for concurrent build. Current
-- approach does not work with "-j" ghc option, which could cause race
-- conditions when multiple mudoles were requiring same home package module,
-- since the HscEnv is shared between all home package modules.

-- | Modify given 'DynFlags' as in macro expansion state.
setExpanding :: DynFlags -> DynFlags
setExpanding dflags0 =
  let dflags1 = dflags0 {parMakeCount = Just 1}
      raw_settings = rawSettings dflags1
#if MIN_VERSION_ghc(8,10,0)
      dflags2 = dflags1 {rawSettings = expandingKey : raw_settings}
#else
      add_key s = s {sRawSettings = expandingKey : raw_settings}
      dflags2 = dflags1 {settings = add_key (settings dflags1)}
#endif
  in  dflags2
{-# INLINABLE setExpanding #-}

-- | 'True' if given 'DynFlags' is in macro expansion state.
isExpanding :: DynFlags -> Bool
isExpanding = isJust . lookup (fst expandingKey) . rawSettings
{-# INLINABLE isExpanding #-}

-- | Internally used key value pair to mark macro expansion state.
expandingKey :: (String, String)
expandingKey = ("FNK_MEX", "1")
{-# INLINABLE expandingKey #-}

removeWayDyn :: DynFlags -> DynFlags
#if MIN_VERSION_ghc(9,2,0)
removeWayDyn df = df {targetWays_ = removeDynFromWays (targetWays_ df)}
#else
removeWayDyn df = df {ways = removeDynFromWays (ways df)}
#endif
{-# INLINABLE removeWayDyn #-}

#if MIN_VERSION_ghc(9,0,0)
removeDynFromWays :: Set.Set Way -> Set.Set Way
removeDynFromWays = Set.filter (/= WayDyn)
#else
removeDynFromWays :: [Way] -> [Way]
removeDynFromWays = filter (/= WayDyn)
#endif
{-# INLINABLE removeDynFromWays #-}

-- | From `discardIC'.
discardInteractiveContext :: HscEnv -> HscEnv
#if MIN_VERSION_ghc(9,4,0)
discardInteractiveContext = discardIC
#else
discardInteractiveContext hsc_env =
  let dflags = hsc_dflags hsc_env
      empty_ic = emptyInteractiveContext dflags
      new_ic_monad = keep_external_name ic_monad
      old_ic = hsc_IC hsc_env
      keep_external_name ic_name =
        if nameIsFromExternalPackage this_pkg old_name
           then old_name
           else ic_name empty_ic
        where
         old_name = ic_name old_ic
#  if MIN_VERSION_ghc(9,2,0)
      this_pkg = hsc_home_unit hsc_env
#  elif MIN_VERSION_ghc(9,0,0)
      this_pkg = homeUnit dflags
#  else
      this_pkg = thisPackage dflags
#  endif
  in  hsc_env {hsc_IC = empty_ic {ic_monad = new_ic_monad}}
#endif
{-# INLINABLE discardInteractiveContext #-}

-- | Setup 'DynFlags' for interactive evaluation.
bcoDynFlags :: DynFlags -> DynFlags
-- XXX: See: 'GhcMake.enableCodeGenForUnboxedTupleOrSums'.
bcoDynFlags dflags0 =
  let dflags1 = dflags0 { ghcLink = LinkInMemory
#if MIN_VERSION_ghc(9,2,0)
                        , backend = Interpreter
#else
                        , hscTarget = HscInterpreted
#endif
                        }
      dflags2 = unSetGeneralFlag' Opt_Hpc $
                unSetGeneralFlag' Opt_BuildDynamicToo dflags1
#if MIN_VERSION_ghc(9,2,0)
      dflags3 = setGeneralFlag' Opt_ByteCode dflags2
#elif MIN_VERSION_ghc(8,10,3)
      dflags3 = setGeneralFlag' Opt_ByteCodeIfUnboxed dflags2
#elif MIN_VERSION_ghc(8,10,1)
      dflags3 = setGeneralFlag' Opt_ByteCode dflags2
#else
      dflags3 = dflags2
#endif
      dflags4 = setGeneralFlag' Opt_IgnoreOptimChanges $
                setGeneralFlag' Opt_IgnoreHpcChanges $
                updOptLevel 0 dflags3
#if MIN_VERSION_ghc(8,10,0)
      -- XXX: Warning message for missing home package module is shown with
      -- -Wall option, suppressing for now ...
      dflags5 = wopt_unset dflags4 Opt_WarnMissingHomeModules
#else
      dflags5 = dflags4
#endif
  in  dflags5
{-# INLINABLE bcoDynFlags #-}

interpHasNoWayDyn :: Bool
#if MIN_VERSION_ghc(9,0,0)
interpHasNoWayDyn = WayDyn `notElem` hostFullWays
#else
interpHasNoWayDyn = WayDyn `notElem` interpWays
#endif
{-# INLINABLE interpHasNoWayDyn #-}

-- | 'True' when the 'DynFlags' is using interpreter.
isInterpreted :: DynFlags -> Bool
#if MIN_VERSION_ghc(9,2,0)
isInterpreted dflags = backend dflags == Interpreter
#else
isInterpreted dflags = hscTarget dflags == HscInterpreted
#endif
{-# INLINABLE isInterpreted #-}


-- ---------------------------------------------------------------------
--
-- Macro expander
--
-- ---------------------------------------------------------------------

-- | Returns a list of bounded names in let expression.
boundedNames :: Code -> [FastString]
boundedNames form =
  case unCode form of
    List xs          -> concatMap boundedName xs
    Atom (ASymbol n) -> [n]
    _                -> []
{-# INLINABLE boundedNames #-}

boundedName :: Code -> [FastString]
boundedName form =
  case unCode form of
    List (LForm (L _ (Atom (ASymbol "="))):n:_) -> boundedNameOne n
    _                                           -> []
{-# INLINABLE boundedName #-}

boundedNameOne :: Code -> [FastString]
boundedNameOne form =
  case unCode form of
    Atom (ASymbol n) -> [n]
    List ns          -> concatMap f ns
    HsList ns        -> concatMap f ns
    _                -> []
  where
    f x =
      case unCode x of
        Atom (ASymbol n) | isLower (headFS n) -> [n]
        _                                     -> []
{-# INLINABLE boundedNameOne #-}

-- | Perform 'Fnk' action with temporary shadowed macro environment.
withShadowing :: [FastString] -- ^ Names of macro to shadow.
              -> Fnk a -- ^ Action to perform.
              -> Fnk a
withShadowing toShadow act = do
  fnk_env <- getFnkEnv
  let emacros = envMacros fnk_env
      tmacros = envTmpMacros fnk_env
      f name _ = unMacroName name `notElem` toShadow
  putFnkEnv fnk_env { envMacros = Map.filterWithKey f emacros
                    , envTmpMacros = map (Map.filterWithKey f) tmacros }
  result <- act
  modifyFnkEnv (\e -> e { envMacros = emacros
                        , envTmpMacros = tmacros })
  return result

-- | Expand forms, with taking care of @begin@ special form.
expands :: [Code] -> Fnk [Code]
expands forms = do
  fnk_env <- getFnkEnv
  let macro_names me =
        if null me
           then nest 2 "None"
           else nest 2 (fsep (map (ppr . unMacroName) (Map.keys me)))
      tmp_macros = Map.unions (envTmpMacros fnk_env)
  debug fnk_env
        Nothing
        [ "Global macros:",  macro_names (envMacros fnk_env)
        , "Temporary macros:", macro_names tmp_macros ]
  expands' forms

-- | Internal works for 'expands'.
expands' :: [Code] -> Fnk [Code]
expands' = fmap concat . mapM expand'
{-# INLINABLE expands' #-}

-- | Expand form to list of 'Code', supports special form /begin/.
expand' :: Code -> Fnk [Code]
expand' form =
  case unCode form of
    List (hd:_) | Atom (ASymbol ":quote") <- unCode hd -> return [form]
    _ -> do
     form' <- expand form
     case unCode form' of
       List (LForm (L _ (Atom (ASymbol ":begin"))):rest) ->
         case rest of
           [] -> return []
           _  -> expands' rest
       _ -> return [form']
{-# INLINABLE expand' #-}

-- | Recursively expands the given 'Code'.
expand :: Code -> Fnk Code
expand form =
  case unLForm form of
    L l (List forms) ->
      case forms of
        -- Expand `let' expression, `do' expression, `case' expression, lambda
        -- expression and function binding with shadowing the lexically bounded
        -- names. Expansion of other forms are done without name shadowing.
        -- This function does not expand quoted forms, to preserve the structure
        -- of the quoted forms containing `:begin'.
        kw@(LForm (L _ (Atom (ASymbol x)))):y:rest
          | x == "let"            -> expandLet l kw y rest
          | x == "do"             -> expandDo l kw (y:rest)
          | x == "case"           -> expandCase l kw y rest
          | x == "where"          -> expandWhere l kw y rest
          | x == "=" || x == "\\" -> expandFunBind l kw (y:rest)
          | x == ":quote"         -> return form
        _                         -> expandList l forms

    L l (HsList forms) ->
      -- Without recursively calling 'expand' on the result, cannot expand
      -- macro-generating macros.
      LForm . L l . HsList <$> expands' forms

    -- Non-list forms are untouched.
    _ -> return form
  where
    expandLet l kw binds body = do
      binds' <- expand binds
      let bounded = boundedNames binds'
      body' <- withShadowing bounded (expands' body)
      return $! LForm (L l (List (kw:binds':body')))

    expandDo l kw body = do
      (_, body') <- foldlM expandInDo ([], []) body
      return $! LForm (L l (List (kw:reverse body')))

    expandFunBind l kw rest = do
      let args = init rest
          body = last rest
          bounded = concatMap boundedNameOne args
      args' <- expands' args
      body' <- withShadowing bounded (expand body)
      return $! LForm (L l (List (kw:args'++[body'])))

    expandCase l kw expr rest = do
      let go acc xs =
            case xs of
              -- Pattern may have prefix (e.g. '~' for lazy pattern)
              pat_prefix:pat:expr0:rest0
                | pat_prefix == tilde -> do
                  pat' <- expand pat
                  expr1 <- withShadowing (boundedNameOne pat')
                                         (expand expr0)
                  go (expr1:pat':pat_prefix:acc) rest0
              pat:expr0:rest0 -> do
                pat' <- expand pat
                expr1 <- withShadowing (boundedNameOne pat')
                                       (expand expr0)
                go (expr1:pat':acc) rest0
              _               -> return acc
          tilde = LForm (L l (Atom (ASymbol "~")))
      expr' <- expand expr
      rest' <- go [] rest
      return $! LForm (L l (List (kw:expr':reverse rest')))

    expandWhere l kw expr rest = do
      rest' <- expands' rest
      let bounded = concatMap boundedName rest'
      expr' <- withShadowing bounded (expand expr)
      return $! LForm (L l (List (kw:expr':rest')))

    expandList l forms =
      case forms of
        sym@(LForm (L _ (Atom (ASymbol k)))) : rest -> do
          fnk_env <- getFnkEnv
          case lookupMacro k fnk_env of
            Just m  -> do_expand k (macroFunction m) >>= expand
            Nothing -> LForm . L l . List . (sym:) <$> expands' rest
        _ -> do
          forms' <- expands' forms
          return $! LForm (L l (List forms'))

    do_expand k f =
      do fnk_env <- getFnkEnv
         debug fnk_env (Just "") [vcat ["Expanding:", nest 2 (ppr form)]]
         ret0 <- f form
         debug fnk_env Nothing [cat [ppr k, " ==>"], nest 2 (ppr ret0)]
         return ret0

expandInDo ::
   ([FastString], [Code]) -> Code -> Fnk ([FastString], [Code])
expandInDo (bounded, xs) x = do
  let newbind =
        case x of
          LForm (L _ (List (LForm (L _ (Atom (ASymbol sym))):n:_)))
            | sym == "<-" -> boundedNameOne n
          _               -> []
  x' <- withShadowing bounded (expand x)
  return (newbind ++ bounded, x':xs)
{-# INLINABLE expandInDo #-}

-- | Expand given form once if the form is a macro form, otherwise
-- return the given form.
expand1 :: Code -> Fnk Code
expand1 form =
  case unLForm form of
    L _l (List ((LForm (L _ (Atom (ASymbol k)))) : _)) -> do
      fnk_env <- getFnkEnv
      case lookupMacro k fnk_env of
        Just m  -> macroFunction m form
        Nothing -> return form
    _ -> return form
{-# INLINABLE expand1 #-}

-- | Debug function fot this module.
debug :: FnkEnv -> Maybe SDoc -> [SDoc] -> Fnk ()
debug fnk_env mb_extra msgs0 =
  let msgs1 = maybe msgs0 (: msgs0) mb_extra
  in  debugWhen fnk_env Fnk_trace_expand msgs1
{-# INLINABLE debug #-}
