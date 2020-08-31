{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Module for macro expansion.
module Language.Finkel.Expand
  ( expand
  , expand1
  , expands
  , expands'
  , withExpanderSettings
  , bcoDynFlags
  , canExpandWithObj
  ) where

#include "Syntax.h"

-- base
import           Control.Monad          (foldM)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Char              (isLower)

-- containers
import qualified Data.Map               as Map

-- ghc
import           DynFlags               (DynFlags (..), GeneralFlag (..),
                                         GhcLink (..), HasDynFlags (..),
                                         HscTarget (..), unSetGeneralFlag',
                                         updOptLevel)
import           ErrUtils               (MsgDoc)
import           Exception              (gbracket)
import           FastString             (FastString, headFS)
import           GhcMonad               (getSession, setSession)
import           HscMain                (newHscEnv)
import           HscTypes               (HscEnv (..))
import           Outputable             (Outputable (..), cat, fsep, nest, vcat)
import           SrcLoc                 (GenLocated (..))

#if !defined(mingw32_HOST_OS)
import           DynFlags               (Way (..), dynamicGhc, gopt)
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
withExpanderSettings act =
  do dflags <- getDynFlags
     if hscTarget dflags == HscInterpreted
        -- Already using bytecode interpreter, perhaps in the middle of macro
        -- expansion or called from REPL. Using the given action.
        then act

        -- Current target is object code, switching to the bytecode interpreter.
        -- Also, using the dedicated 'HscEnv' when not reusing the object codes.
        else if canExpandWithObj dflags
                then gbracket getSession setSession work
                else gbracket (prepare dflags) restore work
  where
    prepare dflags = do
      fnk_env <- getFnkEnv
      hsc_env_orig <- getSession
      hsc_env_mex <- case envSessionForExpand fnk_env of
                        Just he -> return he
                        Nothing -> liftIO (newHscEnv dflags)
      setSession hsc_env_mex
      return hsc_env_orig

    restore hsc_env_orig =
      do hsc_env_mex <- getSession
         modifyFnkEnv (\e -> e {envSessionForExpand = Just hsc_env_mex})
         setSession hsc_env_orig

    work hsc_env =
      do setDynFlags (bcoDynFlags (hsc_dflags hsc_env))
         act

-- XXX: Constantly 'False' under Windows.
canExpandWithObj :: DynFlags -> Bool
#if defined(mingw32_HOST_OS)
canExpandWithObj _ = False
#else
canExpandWithObj dflags =
  if dynamicGhc
    then buildDynamic && noOptimization
    -- XXX: Currently have not tested with non-dynamic GHC, conservatively not
    -- using object code.
    else False
  where
    buildDynamic = WayDyn `elem` ws || gopt Opt_BuildDynamicToo dflags
    noOptimization = optLevel dflags == 0
    ws = ways dflags
#endif

-- | Setup 'DynFlags' for interactive evaluation.
bcoDynFlags :: DynFlags -> DynFlags
bcoDynFlags dflags0 =
  let dflags1 = dflags0 { hscTarget = HscInterpreted
                        , ghcLink = LinkInMemory }
      dflags2 = foldr unSetGeneralFlag' dflags1 [ Opt_Hpc
                                                , Opt_BuildDynamicToo ]
      dflags3 = updOptLevel 0 dflags2
  in  dflags3
{-# INLINE bcoDynFlags #-}

-- | Returns a list of bounded names in let expression.
boundedNames :: Code -> [FastString]
boundedNames form =
  case unCode form of
    List xs          -> concatMap boundedName xs
    Atom (ASymbol n) -> [n]
    _                -> []
{-# INLINE boundedNames #-}

boundedName :: Code -> [FastString]
boundedName form =
  case unCode form of
    List ((LForm (L _ (Atom (ASymbol "=")))):n:_) -> boundedNameOne n
    _                                             -> []
{-# INLINE boundedName #-}

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
{-# INLINE boundedNameOne #-}

-- | Perform 'Fnk' action with temporary shadowed macro environment.
withShadowing :: [FastString] -- ^ Names of macro to shadow.
              -> Fnk a -- ^ Action to perform.
              -> Fnk a
withShadowing toShadow fnkc = do
  fnkc_env <- getFnkEnv
  let emacros = envMacros fnkc_env
      tmacros = envTmpMacros fnkc_env
      f name _ = name `notElem` toShadow
  putFnkEnv (fnkc_env { envMacros = Map.filterWithKey f emacros
                      , envTmpMacros = map (Map.filterWithKey f) tmacros })
  result <- fnkc
  putFnkEnv fnkc_env
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
{-# INLINE expands' #-}

-- | Expand form to list of 'Code', supports special form /begin/.
expand' :: Code -> Fnk [Code]
expand' form = do
  form' <- expand form
  case unCode form' of
    List (LForm (L _ (Atom (ASymbol ":begin"))):rest) ->
      case rest of
        [] -> return []
        _  -> expands' rest
    _ -> return [form']
{-# INLINE expand' #-}

-- | Recursively expands the given 'Code'.
expand :: Code -> Fnk Code
expand form =
  case unLForm form of
    L l (List forms) ->
      case forms of
        -- Expand `let' expression, `do' expression, `case' expression, lambda
        -- expression and function binding with shadowing the lexically bounded
        -- names. Expansion of other forms are done without name shadowing.
        kw@(LForm (L _ (Atom (ASymbol x)))):y:rest
          | x == "let"            -> expandLet l kw y rest
          | x == "do"             -> expandDo l kw (y:rest)
          | x == "case"           -> expandCase l kw y rest
          | x == "where"          -> expandWhere l kw y rest
          | x == "=" || x == "\\" -> expandFunBind l kw (y:rest)
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
          fnkc_env <- getFnkEnv
          case lookupMacro k fnkc_env of
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
{-# INLINE expandInDo #-}

-- | Expand given form once if the form is a macro form, otherwise
-- return the given form.
expand1 :: Code -> Fnk Code
expand1 form =
  case unLForm form of
    L _l (List ((LForm (L _ (Atom (ASymbol k)))) : _)) -> do
      fnkc_env <- getFnkEnv
      case lookupMacro k fnkc_env of
        Just (Macro f)       -> f form
        Just (SpecialForm f) -> f form
        Nothing              -> return form
    _ -> return form

-- | Debug function fot this module.
debug :: FnkEnv -> Maybe MsgDoc -> [MsgDoc] -> Fnk ()
debug fnk_env mb_extra msgs0 =
  let msgs1 = maybe msgs0 (: msgs0) mb_extra
  in  debugWhen fnk_env Fnk_trace_expand msgs1
