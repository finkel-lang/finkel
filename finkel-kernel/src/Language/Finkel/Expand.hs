{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Module for macro expansion.
module Language.Finkel.Expand
  ( expand
  , expand1
  , expands
  , expands'
  , withExpanderSettings
  , withExpanderSettings'
  , bcoDynFlags
  , isInterpreted
  , discardInteractiveContext
  ) where

#include "ghc_modules.h"

-- base
import           Control.Monad          (foldM, when)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Char              (isLower)

-- containers
import qualified Data.Map               as Map

#if MIN_VERSION_ghc(9,0,0)
import qualified Data.Set               as Set
#endif

-- exception
import           Control.Monad.Catch    (bracket)

-- ghc
import           GHC_Data_FastString    (FastString, headFS)
import           GHC_Driver_Main        (newHscEnv)
import           GHC_Driver_Monad       (getSession, setSession)
import           GHC_Driver_Session     (DynFlags (..), GeneralFlag (..),
                                         GhcLink (..), HasDynFlags (..),
                                         HscTarget (..), unSetGeneralFlag',
                                         updOptLevel)
import           GHC_Driver_Types       (HscEnv (..), InteractiveContext (..),
                                         emptyInteractiveContext)
import           GHC_Types_Name         (nameIsFromExternalPackage)
import           GHC_Types_SrcLoc       (GenLocated (..))
import           GHC_Utils_Error        (MsgDoc)
import           GHC_Utils_Outputable   (Outputable (..), cat, fsep, nest, vcat)

#if MIN_VERSION_ghc(9,0,0)
import           GHC                    (setSessionDynFlags)
import           GHC_Driver_Session     (homeUnit)
import           GHC_Driver_Ways        (Way (..), hostFullWays)
#else
import           GHC_Driver_Session     (Way (..), interpWays, thisPackage)
#endif

#if MIN_VERSION_ghc(8,10,0)
import           GHC_Driver_Session     (setGeneralFlag')
#endif

-- Internal
import           Language.Finkel.Fnk
import           Language.Finkel.Form


-- ---------------------------------------------------------------------
--
-- Macro expander
--
-- ---------------------------------------------------------------------

-- | Perform given action with 'HscEnv' updated for macroexpansion with
-- interactive evaluation, then reset to the preserved original 'HscEnv'.
withExpanderSettings :: Fnk a -> Fnk a
withExpanderSettings = withExpanderSettings' True

-- | Like 'withExpanderSettings', but takes a flag to discard interactive
-- context in the session used for the expansion.
withExpanderSettings' :: Bool -> Fnk a -> Fnk a
withExpanderSettings' discard_ic act =
  do dflags <- getDynFlags
     -- Switching to the dedicated 'HscEnv' for macro expansion when compiling
     -- object code. If not, assuming current session is using bytecode
     -- interpreter, using the given action as it.
     if isInterpreted dflags
        then act
        else bracket (prepare dflags) restore (const act)
  where
    prepare dflags = do
      fnk_env <- getFnkEnv
      hsc_env_old <- getSession

      -- Reusing revious 'HscEnv' for macro expansion if exist, or making a new
      -- 'HscEnv'. When reusing, discarding the previous 'InteractiveContext',
      -- to avoid file local compile time functions to affect other modules.
      case envSessionForExpand fnk_env of
        Just he -> if discard_ic
                      then setSession $! discardInteractiveContext he
                      else setSession he
        Nothing -> new_hsc_env fnk_env dflags >>= setSession >> postSetSession

      return hsc_env_old

    restore hsc_env_old = do
      hsc_env_new <- getSession
      modifyFnkEnv (\e -> e {envSessionForExpand = Just hsc_env_new})
      setSession hsc_env_old

    -- Adjusting the 'DynFlags' used by the macro expansion session, to support
    -- evaluating expressions in dynamic and non-dynamic builds of the Finkel
    -- compiler executable.
    new_hsc_env fnk_env dflags0 = do
      debug fnk_env Nothing ["Making new session for expand"]
      let dflags1 = bcoDynFlags dflags0
          ways1 = ways dflags1
          dflags2 = if interp_has_no_way_dyn
                       then dflags1 {ways = removeWayDyn ways1}
                       else dflags1

      when interp_has_no_way_dyn $
        debug fnk_env Nothing ["Not using WayDyn in expander session"]
      dumpDynFlags fnk_env "Language.Finkel.Expand.new_hsc_env" dflags2

      liftIO $! newHscEnv dflags2

#if MIN_VERSION_ghc(9,0,0)
    -- To set the "hsc_interp" field in new session.
    postSetSession = getDynFlags >>= setSessionDynFlags
#else
    postSetSession = return ()
#endif

#if MIN_VERSION_ghc(9,0,0)
removeWayDyn :: Set.Set Way -> Set.Set Way
removeWayDyn = Set.filter (/= WayDyn)
#else
removeWayDyn :: [Way] -> [Way]
removeWayDyn = filter (/= WayDyn)
#endif

interp_has_no_way_dyn :: Bool
#if MIN_VERSION_ghc(9,0,0)
interp_has_no_way_dyn = WayDyn `notElem` hostFullWays
#else
interp_has_no_way_dyn = WayDyn `notElem` interpWays
#endif

-- | From `discardIC'.
discardInteractiveContext :: HscEnv -> HscEnv
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
#if MIN_VERSION_ghc(9,0,0)
      this_pkg = homeUnit dflags
#else
      this_pkg = thisPackage dflags
#endif
  in  hsc_env {hsc_IC = empty_ic {ic_monad = new_ic_monad}}

-- | Setup 'DynFlags' for interactive evaluation.
bcoDynFlags :: DynFlags -> DynFlags
-- XXX: See: 'GhcMake.enableCodeGenForUnboxedTupleOrSums'.
bcoDynFlags dflags0 =
  let dflags1 = dflags0 { hscTarget = HscInterpreted
                        , ghcLink = LinkInMemory }
      dflags2 = foldr unSetGeneralFlag' dflags1 [ Opt_Hpc
                                                , Opt_BuildDynamicToo ]
#if MIN_VERSION_ghc(8,10,3)
      dflags3 = setGeneralFlag' Opt_ByteCodeIfUnboxed dflags2
#elif MIN_VERSION_ghc(8,10,1)
      dflags3 = setGeneralFlag' Opt_ByteCode dflags2
#else
      dflags3 = dflags2
#endif
      dflags4 = updOptLevel 0 dflags3
  in  dflags4
{-# INLINABLE bcoDynFlags #-}

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
    List ((LForm (L _ (Atom (ASymbol "=")))):n:_) -> boundedNameOne n
    _                                             -> []
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
      f name _ = name `notElem` toShadow
  putFnkEnv (fnk_env { envMacros = Map.filterWithKey f emacros
                      , envTmpMacros = map (Map.filterWithKey f) tmacros })
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
           else nest 2 (fsep (map ppr (Map.keys me)))
      tmp_macros = Map.unions (envTmpMacros fnk_env)
  debug fnk_env
        Nothing
        [ "Global macros:",  macro_names (envMacros fnk_env)
        , "Temporary macros:", macro_names tmp_macros ]
  expands' forms

-- | Internal works for 'expands'.
expands' :: [Code] -> Fnk [Code]
expands' forms = fmap concat (mapM expand' forms)
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
      (_, body') <- foldM expandInDo ([], []) body
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
            Just (Macro f)       -> do_expand k f >>= expand
            Just (SpecialForm f) -> do_expand k f >>= expand
            Nothing              -> do
              rest' <- expands' rest
              return $! LForm (L l (List (sym:rest')))
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
  return (newbind ++ bounded, (x':xs))
{-# INLINABLE expandInDo #-}

-- | Expand given form once if the form is a macro form, otherwise
-- return the given form.
expand1 :: Code -> Fnk Code
expand1 form =
  case unLForm form of
    L _l (List ((LForm (L _ (Atom (ASymbol k)))) : _)) -> do
      fnk_env <- getFnkEnv
      case lookupMacro k fnk_env of
        Just (Macro f)       -> f form
        Just (SpecialForm f) -> f form
        Nothing              -> return form
    _ -> return form

-- | 'True' when the 'DynFlags' is using interpreter.
isInterpreted :: DynFlags -> Bool
isInterpreted dflags = hscTarget dflags == HscInterpreted
{-# INLINABLE isInterpreted #-}

-- | Debug function fot this module.
debug :: FnkEnv -> Maybe MsgDoc -> [MsgDoc] -> Fnk ()
debug fnk_env mb_extra msgs0 =
  let msgs1 = maybe msgs0 (: msgs0) mb_extra
  in  debugWhen fnk_env Fnk_trace_expand msgs1
