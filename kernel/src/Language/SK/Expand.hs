{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Module for macro expansion.
module Language.SK.Expand
  ( expand
  , expand1
  , expands
  , setExpanderSettings
  , withExpanderSettings
  , specialForms
  , unquoteSplice
  ) where

-- base
import Control.Exception (throw)
import Control.Monad (foldM, when)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Char (isLower)
import Data.Maybe (catMaybes)
import Unsafe.Coerce (unsafeCoerce)

-- containers
import qualified Data.Map as Map

-- ghc
import BasicTypes (FractionalLit(..))
import DynFlags ( DynFlags(..), GeneralFlag(..), GhcLink(..)
                , HscTarget(..), gopt, gopt_unset, updOptLevel
                , xopt_unset )
import ErrUtils (compilationProgressMsg)
import Exception (gbracket)
import FastString (FastString, appendFS, headFS, unpackFS)
import Finder (findImportedModule)
import GHC ( ModuleInfo, getModuleInfo, lookupModule, lookupName
           , modInfoExports, setContext )
import GhcMonad (GhcMonad(..), getSessionDynFlags)
import HscMain (Messager, hscTcRnLookupRdrName, showModuleIndex)
import HscTypes ( InteractiveImport(..), HscEnv(..), FindResult(..)
                , showModMsg )
import HsImpExp (ImportDecl(..), ieName, simpleImportDecl)
import HsSyn (HsModule(..))
import InteractiveEval (getContext)
import Linker (getHValue)
import MkIface (RecompileRequired(..), recompileRequired)
import Module (mkModuleNameFS, moduleNameString)
import Name (nameOccName)
import Outputable (showPpr)
import RdrName (rdrNameOcc)
import TyCoRep (TyThing(..))
import Var (varName)

-- ghc-boot
import qualified GHC.LanguageExtensions as LangExt

-- ghci
import GHCi.RemoteTypes (localRef, withForeignRef)

-- Internal
import Language.SK.Builder (HImportDecl)
import Language.SK.Homoiconic
import Language.SK.Eval
import Language.SK.Form
import Language.SK.SKC
import Language.SK.Syntax ( evalBuilder, parseExpr, parseModule
                          , parseLImport )


-- ---------------------------------------------------------------------
--
-- Quote
--
-- ---------------------------------------------------------------------

quoteAtom :: SrcSpan -> Atom -> Code
quoteAtom l form =
  case form of
    ASymbol s     -> atom [tSym l "aSymbol", tString l (unpackFS s)]
    AChar c       -> atom [tSym l "AChar", tChar l c]
    AString s     -> atom [tSym l "AString", tString l s]
    AInteger n    -> atom [tSym l "AInteger", tInteger l n]
    AFractional n -> atom [tSym l "aFractional", tFractional l n]
    AUnit         -> atom [tSym l "AUnit"]
    _             -> LForm (L l (Atom form))
  where
    atom vals = mkQuoted l (tList l [tSym l "Atom", tList l vals])
{-# INLINE quoteAtom #-}

quote :: Code -> Code
quote orig@(LForm (L l form))  =
  case form of
    Atom atom -> quoteAtom l atom
    List xs   -> quoteList "List" xs
    HsList xs -> quoteList "HsList" xs
    _         -> orig
    where
      quoteList tag xs =
        mkQuoted l (tList l [tSym l tag, tHsList l (map quote xs)])

-- Quasiquote is implemented as special form in Haskell. Though it could
-- be implemented in SK code later. If done in SK code, lexer and reader
-- still need to handle the special case for backtick, comma, and
-- comma-at, because currently there's no way to define read macro.

quasiquote :: Code -> Code
quasiquote orig@(LForm (L l form)) =
  case form of
    List [LForm (L _ (Atom (ASymbol "unquote"))), x]
      | isUnquoteSplice x -> x
      | otherwise         -> tList l [tSym l "toCode", x]
    List forms'
      | [q, body] <- forms'
      , q == tSym l "quasiquote"   -> quasiquote (quasiquote body)
      | any isUnquoteSplice forms' -> splicedList "List" forms'
      | otherwise                  -> nonSplicedList "List" forms'
    HsList forms'
      | any isUnquoteSplice forms' -> splicedList "HsList" forms'
      | otherwise                  -> nonSplicedList "HsList" forms'
    Atom atom                      -> quoteAtom l atom
    TEnd                           -> orig
  where
   splicedList tag forms =
     mkQuoted l (tList l [ tSym l tag
                         , tList l [ tSym l "concat"
                                   , tHsList l (go [] forms)]])
   nonSplicedList tag forms =
     mkQuoted l (tList l [ tSym l tag
                         , tHsList l (map quasiquote forms)])
   go acc forms =
     let (pre, post) = break isUnquoteSplice forms
     in  case post of
           LForm (L ls (List (_:body))):post' ->
             go (acc ++ [tHsList l (map quasiquote pre)
                        ,tList ls [ tSym l "unquoteSplice"
                                  , tList l body]])
                     post'
           _ | null pre  -> acc
             | otherwise -> acc ++ [tHsList l (map quasiquote pre)]

isUnquoteSplice :: Code -> Bool
isUnquoteSplice (LForm form) =
  case form of
    L _ (List (LForm (L _ (Atom (ASymbol "unquote-splice"))):_))
      -> True
    _ -> False
{-# INLINE isUnquoteSplice #-}

unquoteSplice :: Homoiconic a => a -> [Code]
unquoteSplice form =
  case unCode (toCode form) of
    List xs           -> xs
    HsList xs         -> xs
    Atom AUnit        -> []
    Atom (AString xs) -> map toCode xs
    _                 -> throw (SkException
                                  ("unquote splice: got " ++
                                   show (toCode form)))


-- ---------------------------------------------------------------------
--
-- Macro
--
-- ---------------------------------------------------------------------

-- Macro expansion
-- ~~~~~~~~~~~~~~~
--
-- Still considering how to support and what shall be supported with
-- user defined macros. Need to load the form transforming functions
-- defined by the user. May restrict those functions to imported module
-- in current target file. Need to take some kind of GHC environment
-- value to expand macros.

compileMacro :: Bool -> Code -> Code -> Code -> Code
             -> Skc (FastString, [Code], Maybe Macro)
compileMacro doEval form@(LForm (L l _)) self arg body = do
  let LForm (L _ (Atom (ASymbol name))) = self
      name' = appendFS (fsLit "__") name
  expanded <- expand body
  let decls = [tList l [ tSym l "::", self, tSym l "Macro"]
              ,tList l [ tSym l "=", self, expr]]
      expr = tList l [ tSym l "let", tList l [tsig, fn]
                     , tList l [tSym l "Macro", tSym l name']]
      tsig = tList l [ tSym l "::", tSym l name'
                     , tList l [tSym l "->", tSym l "Code"
                               ,tList l [tSym l "Skc", tSym l "Code"]]]
      fn = tList l [tSym l "=", tSym l name', arg, expanded]
  if doEval
    then case evalBuilder parseExpr [expr] of
                Right hexpr -> do
                  macro <- evalExpr hexpr
                  return (name, decls, Just (unsafeCoerce macro))
                Left err -> skSrcError form err
    else return (name, decls, Nothing)

getTyThingsFromIDecl :: HImportDecl -> ModuleInfo -> Skc [TyThing]
getTyThingsFromIDecl (L _ idecl) minfo = do
  -- 'toImportList' borrowed from local definition in
  -- 'TcRnDriver.tcPreludeClashWarn'.
  let exportedNames = modInfoExports minfo
      ieName' (L l ie) = L l (ieName ie)
      toImportList (h, loc) = (h, map ieName' (unLoc loc))
      getNames =
        case fmap toImportList (ideclHiding idecl) of
          -- Import with `hiding' entities. Comparing 'Name' and
          -- 'RdrName' via OccName'.
          Just (True, ns)  -> do
            let f n acc | nameOccName n `elem` ns' = acc
                        | otherwise             = n : acc
                ns' = map (rdrNameOcc . unLoc) ns
            return (foldr f [] exportedNames)

          -- Import with explicit entities.
          Just (False, ns) -> do
            hsc_env <- getSession
            concat <$> mapM (liftIO . hscTcRnLookupRdrName hsc_env) ns

          -- Import whole module.
          Nothing          -> return exportedNames

  catMaybes <$> (getNames >>= mapM lookupName)

addImportedMacro :: TyThing -> Skc ()
addImportedMacro thing = when (isMacro thing) (addImportedMacro' thing)

addImportedMacro' :: TyThing -> Skc ()
addImportedMacro' ty_thing = do
  dflags <- getSessionDynFlags
  case ty_thing of
    AnId var -> do
      debugSkc (";;; adding macro `" ++
                showPpr dflags (varName var) ++
                "' to current compiler session.")
      hsc_env <- getSession
      let name = varName var
      fhv <- liftIO (getHValue hsc_env name)
      hv <- liftIO (withForeignRef fhv localRef)
      let macro = unsafeCoerce hv
      insertMacro (fsLit (showPpr (hsc_dflags hsc_env) name)) macro
      return ()
    _ -> error "addImportedmacro"


-- ---------------------------------------------------------------------
--
-- Special forms
--
-- ---------------------------------------------------------------------

type Mfunc = Code -> Skc Code

m_quote :: Mfunc
m_quote form =
  case unLForm form of
    L l (List [_,body]) ->
      let LForm (L _ body') = quote body
      in  return (LForm (L l body'))
    _ -> skSrcError form ("malformed quote at " ++ showLoc form)

m_quasiquote :: Mfunc
m_quasiquote form =
    case unLForm form of
      L l (List [_,body]) ->
        let LForm (L _ body') = quasiquote body
        in  return (LForm (L l body'))
      _ -> skSrcError form ("malformed quasiquote at " ++ showLoc form)

m_defineMacro :: Mfunc
m_defineMacro form@(LForm (L l _)) = do
  addToContext <- envAddInDefineMacro <$> getSkEnv
  (name, decls, mb_macro) <- add addToContext form
  when addToContext
       (maybe (return ()) (insertMacro name) mb_macro)
  return (tList l (tSym l "begin":decls))
  where
    add doEval x =
      case unCode x of
        List [_,self,arg,body] -> compileMacro doEval x self arg body
        _ -> skSrcError form ("define-macro: malformed args:\n" ++
                              show x)

m_letMacro :: Mfunc
m_letMacro form =
  case unLForm form of
    L l1 (List (_:LForm (L l2 (List forms)):rest)) -> do
      sk_env0 <- getSkEnv

      -- Expand body of `let-macro' with temporary macros.
      macros <- Map.fromList <$> mapM addLetMacro forms
      let tmpMacros0 = envTmpMacros sk_env0
      putSkEnv (sk_env0 {envTmpMacros = macros : tmpMacros0})
      expanded <- expands rest

      -- Getting 'SkEnv' again, so that persistent macros defined inside
      -- the `let-macro' body could be used hereafter.
      sk_env1 <- getSkEnv
      putSkEnv (sk_env1 {envTmpMacros = tmpMacros0})

      case expanded of
        [x] -> return x
        _   -> return (tList l1 (tSym l2 "begin" : expanded))
    _ -> skSrcError form ("let-macro: malformed args:\n" ++ show form)
  where
    addLetMacro x =
      case unCode x of
        List [self,arg,body] -> do
          (name, _decl, mb_macro) <- compileMacro True x self arg body
          case mb_macro of
            Nothing    -> skSrcError x "let-macro: compilation error"
            Just macro -> return (name, macro)
        _ -> skSrcError x ("let-macro: malformed macro: " ++ show x)

m_require :: Mfunc
m_require form =
  -- The special form `require' modifies the HscEnv at the time of macro
  -- expansion, to update the context in compile time session.  The
  -- `require' is implemented as special form, to support dependency
  -- analysis during compilation of multiple modules with `--make'
  -- command.
  --
  -- Note that the form body of `require' is parsed twice, once
  -- in Reader, and again in this module. Parsing twice because the
  -- first parse is done before expanding macro, to analyse the module
  -- dependency graph of home package module.
  --
  case form of
    LForm (L _ (List (_:code))) ->
      case evalBuilder parseLImport code of
        Right lidecl@(L _ idecl) -> do
          hsc_env <- getSession
          sk_env <- getSkEnv

          let dflags = hsc_dflags hsc_env
              recomp = gopt Opt_ForceRecomp dflags
              mname = unLoc (ideclName idecl)
              mname' = moduleNameString mname
          debugSkc (";;; require: " ++ showPpr dflags idecl)

          -- Try finding the required module. Make the module with
          -- the function stored in SkEnv when not found.
          fresult <- liftIO (findImportedModule hsc_env mname Nothing)
          case fresult of
            Found {} -> return ()
            _        ->
              case envMake sk_env of
                Just mk -> withRequiredSettings (mk recomp mname')
                Nothing -> failS "require: no make function"

          -- Add the module to current compilation context.
          contexts <- getContext
          setContext (IIDecl idecl : contexts)

          -- Add required module name to SkEnv.
          let reqs = mname':envRequiredModuleNames sk_env
              sk_env' = sk_env {envRequiredModuleNames = reqs}
          putSkEnv sk_env'

          -- Look up Macros in parsed module, add to SkEnv when found.
          mdl <- lookupModule mname Nothing
          mb_minfo <- getModuleInfo mdl
          case mb_minfo of
            Just minfo -> do
              things <- getTyThingsFromIDecl lidecl minfo
              mapM_ addImportedMacro things
              return emptyForm
            Nothing ->
              skSrcError form ("require: module " ++ mname' ++
                               " not found.")
        Left err -> skSrcError form ("require: " ++ err)
    _ -> skSrcError form "require: malformed body"

m_evalWhenCompile :: Mfunc
m_evalWhenCompile form =
  case unLForm form of
    L l (List (_ : body)) -> do
      expanded <- expands body
      case evalBuilder parseModule expanded of
        Right (HsModule {hsmodDecls = decls}) -> do
          (tythings, _ic) <- evalDecls decls
          mapM_ addImportedMacro tythings
          return emptyForm
        Left err -> skSrcError (LForm (L l (List body))) err
    _ -> skSrcError form ("eval-when-compile: malformed body: " ++
                          show form)

specialForms :: EnvMacros
specialForms =
  makeEnvMacros
    [("define-macro", SpecialForm m_defineMacro)
    ,("eval-when-compile", SpecialForm m_evalWhenCompile)
    ,("let-macro", SpecialForm m_letMacro)
    ,("quote", SpecialForm m_quote)
    ,("quasiquote", SpecialForm m_quasiquote)
    ,("require", SpecialForm m_require)]


-- ---------------------------------------------------------------------
--
-- Macro expander
--
-- ---------------------------------------------------------------------

-- | Set state for macro expansion.
--
-- Add modules used during macro expansion to current context, and set
-- some DynFlags fields.
setExpanderSettings :: Skc ()
setExpanderSettings = do
  flags0 <- getSessionDynFlags
  let flags1 = flags0 { hscTarget = HscInterpreted
                      , ghcLink = LinkInMemory }
      flags2 = gopt_unset flags1 Opt_Hpc
      flags3 = xopt_unset flags2 LangExt.MonomorphismRestriction
      flags4 = updOptLevel 0 flags3

  -- When not saved, save the original DynFlags for compiling required
  -- modules.
  skenv <- getSkEnv
  case envMakeDflags skenv of
    Nothing -> putSkEnv (skenv {envMakeDflags = Just flags0})
    Just _  -> return ()

  setDynFlags flags4
  contextModules <- envContextModules <$> getSkEnv
  setContext (map (mkIIDecl . fsLit) contextModules)

-- | Perform given action with DynFlags set for macroexpansion, used
-- this to preserve original DynFlags.
withExpanderSettings :: Skc a -> Skc a
withExpanderSettings act =
  gbracket getSessionDynFlags
           setDynFlags
           (const (setExpanderSettings >> act))

setRequiredSettings :: Skc ()
setRequiredSettings = do
  skenv <- getSkEnv
  case envMakeDflags skenv of
    Just dflags -> setDynFlags dflags
    Nothing     -> failS "setOrigSettings: missing DynFlags"
  putSkEnv skenv {envMessager = requiredMessager}

withRequiredSettings :: Skc a -> Skc a
withRequiredSettings act =
  gbracket
    (do dflags <- getSessionDynFlags
        skenv <- getSkEnv
        return (dflags, skenv))
    (\(dflags, skenv) -> setDynFlags dflags >> putSkEnv skenv)
    (const (setRequiredSettings >> act))

requiredMessager :: Messager
requiredMessager hsc_env mod_index recomp mod_summary =
  case recomp of
    MustCompile -> showMsg "Compiling " (" [required]")
    UpToDate
      | verbosity dflags >= 2 -> showMsg "Skipping " ""
      | otherwise             -> return ()
    RecompBecause why ->
      showMsg "Compiling " (" [required, " ++ why ++ "]")
  where
    dflags = hsc_dflags hsc_env
    showMsg msg reason =
      compilationProgressMsg
        dflags
        (showModuleIndex mod_index ++
         msg ++
         showModMsg dflags (hscTarget dflags)
                    (recompileRequired recomp)
                     mod_summary ++
         reason)

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

-- | Perform 'SKc' action with temporary shadowed macro environment.
withShadowing :: [FastString] -- ^ Names of macro to shadow.
              -> Skc a -- ^ Action to perform.
              -> Skc a
withShadowing toShadow skc = do
  ske <- getSkEnv
  let emacros = envMacros ske
      tmacros = envTmpMacros ske
      f name _ | name `elem` toShadow = False
               | otherwise            = True
  putSkEnv (ske { envMacros = Map.filterWithKey f emacros
                , envTmpMacros = map (Map.filterWithKey f) tmacros })
  result <- skc
  putSkEnv ske
  return result

-- | Expands form, with taking care of @begin@ special form.
expands :: [Code] -> Skc [Code]
-- XXX: Avoid using 'reverse'.
expands = fmap reverse . foldM f []
  where
    f acc form = do
      expanded@(LForm (L _ form')) <- expand form
      case form' of
        List (LForm (L _ (Atom (ASymbol "begin"))) : rest) -> do
          rest' <- expands rest
          return $! (reverse rest' ++ acc)
        _ -> return $! (expanded : acc)

-- | Recursively expands the given 'Code'.
expand :: Code -> Skc Code
expand form =
  case unLForm form of
    L l (List forms) ->
      case forms of
        -- Expand `let' expression, `case' expression, lambda expression
        -- and function binding with shadowing the lexically bounded
        -- names. Expansion of other forms are done without name
        -- shadowing.
        kw@(LForm (L _ (Atom (ASymbol x)))):y:rest
          | x == "let"            -> expandLet l kw y rest
          | x == "do"             -> expandDo l kw (y:rest)
          | x == "case"           -> expandCase l kw y rest
          | x == "where"          -> expandWhere l kw y rest
          | x == "=" || x == "\\" -> expandFunBind l kw (y:rest)
        _                         -> expandList l List forms

    L l (HsList forms) ->
      -- Without recursively calling 'expand' on the result, cannot
      -- expand macro-generating macros.
      LForm . L l . HsList <$> mapM expand forms

    -- Non-list forms are untouched.
    _ -> return form
  where
    expandLet l kw binds body = do
      binds' <- expand binds
      let bounded = boundedNames binds'
      body' <- withShadowing bounded (mapM expand body)
      return (LForm (L l (List (kw:binds':body'))))

    expandDo l kw body = do
      (_, body') <- foldM expandInDo ([], []) body
      return (LForm (L l (List (kw:reverse body'))))

    expandFunBind l kw rest = do
      let args = init rest
          body = last rest
          bounded = concatMap boundedNameOne args
      args' <- mapM expand args
      body' <- withShadowing bounded (expand body)
      return (LForm (L l (List (kw:args'++[body']))))

    expandCase l kw expr rest = do
      let go acc xs =
            case xs of
              pat:expr0:rest0 -> do
                pat' <- expand pat
                expr1 <- withShadowing (boundedNameOne pat')
                                       (expand expr0)
                go (expr1:pat':acc) rest0
              _               -> return acc
      expr' <- expand expr
      rest' <- go [] rest
      return (LForm (L l (List (kw:expr':reverse rest'))))

    expandWhere l kw expr rest = do
      rest' <- mapM expand rest
      let bounded = concatMap boundedName rest'
      expr' <- withShadowing bounded (expand expr)
      return (LForm (L l (List (kw:expr':rest'))))

    expandList l constr forms =
      case forms of
        sym@(LForm (L _ (Atom (ASymbol k)))) : rest -> do
          ske <- getSkEnv
          case lookupMacro k ske of
           Just (Macro f)       -> f form >>= expand
           Just (SpecialForm f) -> f form >>= expand
           Nothing              -> do
             rest' <- mapM expand rest
             return (LForm (L l (constr (sym:rest'))))
        _ -> do
          forms' <- mapM expand forms
          return (LForm (L l (constr forms')))

expandInDo ::
   ([FastString], [Code]) -> Code -> Skc ([FastString], [Code])
expandInDo (bounded, xs) x = do
  let newbind =
        case x of
          LForm (L _ (List ((LForm (L _ (Atom (ASymbol sym)))):n:_)))
            | sym == "<-" -> boundedNameOne n
          _               -> []
  x' <- withShadowing bounded (expand x)
  return (newbind ++ bounded, (x' : xs))
{-# INLINE expandInDo #-}

-- | Expand given form once if the form is a macro form, otherwise
-- return the given form.
expand1 :: Code -> Skc Code
expand1 form =
  case unLForm form of
    L _l (List ((LForm (L _ (Atom (ASymbol k)))) : _)) -> do
      ske <- getSkEnv
      case lookupMacro k ske of
        Just (Macro f)       -> f form
        Just (SpecialForm f) -> f form
        Nothing              -> return form
    _ -> return form


-- ---------------------------------------------------------------------
--
-- Auxiliary
--
-- ---------------------------------------------------------------------

tSym :: SrcSpan -> FastString -> Code
tSym l s = LForm (L l (Atom (ASymbol s)))
{-# INLINE tSym #-}

tChar :: SrcSpan -> Char -> Code
tChar l c = LForm (L l (Atom (AChar c)))
{-# INLINE tChar #-}

tString :: SrcSpan -> String -> Code
tString l s = LForm (L l (Atom (AString s)))
{-# INLINE tString #-}

tInteger :: SrcSpan -> Integer -> Code
tInteger l n = LForm (L l (Atom (AInteger n)))
{-# INLINE tInteger #-}

tFractional :: SrcSpan -> FractionalLit -> Code
tFractional l n = LForm (L l (Atom (AFractional n)))
{-# INLINE tFractional #-}

tList :: SrcSpan -> [Code] -> Code
tList l forms = LForm (L l (List forms))
{-# INLINE tList #-}

tHsList :: SrcSpan -> [Code] -> Code
tHsList l forms = LForm (L l (HsList forms))
{-# INLINE tHsList #-}

mkQuoted :: SrcSpan -> Code -> Code
mkQuoted l form = tList l [tSym l "quoted", form]
{-# INLINE mkQuoted #-}

emptyForm :: Code
emptyForm = tList skSrcSpan [tSym skSrcSpan "begin"]
{-# INLINE emptyForm #-}

mkIIDecl :: FastString -> InteractiveImport
mkIIDecl = IIDecl . simpleImportDecl . mkModuleNameFS
{-# INLINE mkIIDecl #-}
