{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Module for macros.
module Language.SK.Macro
  ( expand
  , expands
  , setExpanderSettings
  , withExpanderSettings
  , specialForms
  , unquoteSplice
  ) where

-- base
import Control.Monad (foldM, when)
import Data.Char (isLower)
import Data.Maybe (catMaybes)
import Unsafe.Coerce (unsafeCoerce)

-- containers
import qualified Data.Map as Map

-- Internal
import Language.SK.Codish
import Language.SK.Eval
import Language.SK.Form
import Language.SK.GHC
import Language.SK.Syntax (evalBuilder, parseExpr, parseModule)
import Language.SK.SKC


-- ---------------------------------------------------------------------
--
-- Quote
--
-- ---------------------------------------------------------------------

quoteAtom :: SrcSpan -> Atom -> Code
quoteAtom l form =
  case form of
    ASymbol s -> atom [tSym l "aSymbol", tString l (unpackFS s)]
    AChar c -> atom [tSym l "AChar", tChar l c]
    AString s -> atom [tSym l "AString", tString l s]
    AInteger n -> atom [tSym l "AInteger", tInteger l n]
    AFractional n -> atom [tSym l "aFractional", tFractional l n]
    AUnit -> atom [tSym l "AUnit"]
    _ -> LForm (L l (Atom form))
  where
    atom vals = mkQuoted l (tList l [tSym l "Atom", tList l vals])

quote :: Code -> Code
quote orig@(LForm (L l form))  =
  case form of
    Atom atom -> quoteAtom l atom
    List xs ->
      mkQuoted l (tList l [tSym l "List", tHsList l (map quote xs)])
    HsList xs ->
      mkQuoted l (tList l [tSym l "HsList", tHsList l (map quote xs)])
    _ -> orig

-- Quasiquote is currently implemented in Haskell. Though it could be
-- implemented in SK code later. If done in SK code, lexer and reader
-- still need to handle the special case for backtick, comma, and
-- comma-at, because currently there's no way to define read macro.

quasiquote :: Code -> Code
quasiquote orig@(LForm (L l form)) =
  case form of
    List [LForm (L _ (Atom (ASymbol "unquote"))), x] ->
      tList l [tSym l "toCode", x]
    List forms'
       | any isUnquoteSplice forms' ->
          mkQuoted l (tList l [tSym l "List"
                              ,tList l [ tSym l "concat"
                                       , tHsList l (go [] forms')]])
       | otherwise ->
          mkQuoted l (tList l [tSym l "List"
                              ,tHsList l (map quasiquote forms')])
    HsList forms'
       | any isUnquoteSplice forms' ->
         mkQuoted l (tList l [ tSym l "HsList"
                              , tList l [ tSym l "concat"
                                        , tHsList l (go [] forms')]])
       | otherwise ->
         mkQuoted l (tList l [tSym l "HsList"
                             ,tHsList l (map quasiquote forms')])
    Atom atom -> quoteAtom l atom
    TEnd       -> orig
  where
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
    L _ (List ((LForm (L _ (Atom (ASymbol "unquote-splice"))):_)))
      -> True
    _ -> False

unquoteSplice :: Codish a => a -> [Code]
unquoteSplice form =
  case unLocLForm (toCode form) of
    List xs   -> xs
    HsList xs -> xs
    _         -> []

-- ---------------------------------------------------------------------
--
-- Macro
--
-- ---------------------------------------------------------------------

-- Macro expansion
-- ~~~~~~~~~~~~~~~
--
-- Consider how to support User defined macros. Need to load the form
-- transforming functions defined by the user. May restrict those
-- functions to imported module in current target file. Need to take
-- some kind of GHC environment value to expand macros.
--
-- Hy language separates the loading of modules. For runtime, it's done
-- with `import', for macro expansion time, done with `require'.

putMacro :: Code -> Code -> Code -> Code -> Skc [Code]
putMacro form self arg body = do
  let LForm (L l _) = form
      LForm (L _ (Atom (ASymbol name))) = self
  expanded <- expand body
  let expr = tList l [tSym l "\\", arg, expanded]
      expr' = tList l [tSym l "Macro", expr]
  case evalBuilder parseExpr [expr'] of
    Right hexpr -> do
      macro <- evalExpr hexpr
      let decls = [tList l [tSym l "::", self, tSym l "Macro"]
                  ,tList l [tSym l "=", self, expr']]
      insertMacro name (unsafeCoerce macro)
      return decls
    Left err -> skSrcError form err

mkIIDecl :: FastString -> InteractiveImport
mkIIDecl = IIDecl . simpleImportDecl . mkModuleNameFS

pTyThing :: DynFlags -> TyThing -> Skc ()
pTyThing dflags ty_thing@(AnId var) = do
  let str :: Outputable p => p -> String
      str = showPpr dflags
      typ = str (varType var)
  when (typ == "Macro")
       (do debugIO (putStrLn (";;; adding macro `" ++
                              str (varName var) ++
                              "' to current compiler session."))
           addImportedMacro ty_thing)
pTyThing dflags tt = pTyThing' dflags tt

-- Currently not in use during macro addition, just for printing out
-- information.
--
-- Using 'IfaceDecl' to detect the Language.SK.SKC.Macro type. This way is
-- more safe than comparing the Type of Var, since IfaceDecl contains
-- more detailed information.
--
pTyThing' :: DynFlags -> TyThing -> Skc ()
pTyThing' _ _ = return ()

-- pTyThing' dflags tt@(AnId _) = do
--   let ifd = tyThingToIfaceDecl tt
--       name = ifName ifd
--       typ = ifType ifd
--       prn = liftIO . putStrLn
--       str :: Outputable p => p -> String
--       str = showPpr dflags
--   prn (concat [";;; an id: name=", str name , " type=", str typ])
--   case typ of
--     -- Constructor name 'IfaceTyVar' changed since ghc 8.0.2 release.
--     IfaceTyVar _ -> prn "free ty var"
--     IfaceLitTy _ -> prn "lit ty"
--     IfaceAppTy _ _ -> prn "app ty"
--     IfaceFunTy _ _ -> prn "fun ty"
--     IfaceDFunTy _ _ -> prn "dfun ty"
--     IfaceForAllTy _ _ -> prn "for all ty"
--     IfaceTyConApp con arg -> do
--       let conName = str (ifaceTyConName con)
--       prn (concat [ ";;; ty con app,"
--                   , " tyConName=", conName
--                   , " tyConArg=", str arg])
--     IfaceCastTy _ _ -> prn "cast ty"
--     IfaceCoercionTy _ -> prn "coercion ty"
--     IfaceTupleTy {} -> prn "tuple ty"

-- pTyThing' dflags tt =
--   -- Arguments of `pprTyThing' changed since ghc-8.0.2 release.
--   liftIO (putStrLn (";;; " ++ (showSDocUnqual dflags (pprTyThing tt))))

addImportedMacro :: TyThing -> Skc ()
addImportedMacro ty_thing =
  case ty_thing of
    AnId var -> do
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
  decls <- add form
  return (tList l (tSym l "begin":decls))
  where
    add x =
      case unLocLForm x of
        List [_,self,arg,body] -> putMacro x self arg body
        _ -> skSrcError form ("define-macro: malformed args:\n" ++
                              show x)

-- XXX: When macros defined with `define-macro' have same name, old
-- macros will be overridden by `let-macro'. Need to update the SkEnv to
-- hold the list of macro name-function lookup table ...
m_letMacro :: Mfunc
m_letMacro form =
  case unLForm form of
    L l1 (List (_:LForm (L l2 (List forms)):rest)) -> do
      sk_env <- getSkEnv
      mapM_ addLetMacro forms
      expanded <- expands rest
      putSkEnv sk_env
      return (tList l1 (tSym l2 "begin":expanded))
    _ -> skSrcError form ("let-macro: malformed args:\n" ++ show form)
  where
    addLetMacro x =
      case unLocLForm x of
        List [self,arg,body] -> putMacro x self arg body
        _ -> skSrcError x ("malformed macro: " ++ show x)

m_require :: Mfunc
m_require form =
  -- The `require' is implemented as special form, to support dependency
  -- analysis during compilation of multiple modules with `--make'
  -- command.
  --
  -- The special form `require' modifies the HscEnv at the time of macro
  -- expansion, to update the context in compile time session.
  --
  -- XXX: Need to clean up the context after compiling each file, but
  -- not yet done. Reason to clean up the context is, if not cleaned,
  -- all files passed via "--make" command will use the same interactive
  -- context, which could be confusing, and may cause unwanted name
  -- conflicts and overridings.
  --
  case unLForm form of
    L _l1 (List [_,LForm (L _l2 (Atom (ASymbol mname)))]) -> do
      debugIO (putStrLn (";;; requiring " ++ unpackFS mname))
      contexts <- getContext
      setContext (mkIIDecl mname : contexts)
      mdl <- lookupModule (mkModuleNameFS mname) Nothing
      mb_minfo <- getModuleInfo mdl
      case mb_minfo of
        Just minfo ->
          do dflags <- getSessionDynFlags
             let names = modInfoExports minfo
             mb_tythings <- mapM lookupName names
             mapM_ (pTyThing dflags) (catMaybes mb_tythings)
             return emptyForm
        Nothing -> skSrcError form ("require: cannot find modinfo for "
                                 ++ unpackFS mname)
    _ -> skSrcError form "require: malformed syntax."

m_evalWhenCompile :: Mfunc
m_evalWhenCompile form =
  case unLForm form of
    L l (List (_ : body)) -> do
      expanded <- expands body
      case evalBuilder parseModule expanded of
        Right (HsModule {hsmodDecls = decls}) -> do
          _ <- evalDecls decls
          return emptyForm
        Left err -> skSrcError (LForm (L l (List body))) err
    _ -> skSrcError form ("eval-when-compile: malformed body: " ++
                          show form)

specialForms :: EnvMacros
specialForms =
  Map.fromList
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
                      , ghcLink = LinkInMemory
                      , optLevel = 0 }
      flags2 = gopt_unset flags1 Opt_Hpc
  _ <- setSessionDynFlags flags2
  contextModules <- envContextModules <$> getSkEnv
  setContext (map (mkIIDecl . fsLit) contextModules)

-- | Perform given action with DynFlags set for macroexpansion, used
-- this to preserve original DynFlags.
withExpanderSettings :: Skc a -> Skc a
withExpanderSettings act = do
  origFlags <- getSessionDynFlags
  setExpanderSettings
  ret <- act
  _ <- setSessionDynFlags origFlags
  return ret

-- | Returns a list of bounded names in let expression.
boundedNames :: Code -> [FastString]
boundedNames form =
  case unLocLForm form of
    List xs          -> concatMap boundedName xs
    Atom (ASymbol n) -> [n]
    _                -> []

boundedName :: Code -> [FastString]
boundedName form =
  case unLocLForm form of
    List ((LForm (L _ (Atom (ASymbol "=")))):n:_) -> boundedNameOne n
    _                                             -> []

boundedNameOne :: Code -> [FastString]
boundedNameOne form =
  case unLocLForm form of
    Atom (ASymbol n) -> [n]
    List ns          -> concatMap f ns
    HsList ns        -> concatMap f ns
    _                -> []
  where
    f x =
      case unLocLForm x of
        Atom (ASymbol n) | isLower (headFS n) -> [n]
        _                                     -> []

-- | Perform 'SKc' action with temporary shadowed macro environment.
withShadowing :: [FastString] -- ^ Names of macro to shadow.
              -> Skc a -- ^ Action to perform.
              -> Skc a
withShadowing toShadow skc = do
  macros <- getEnvMacros
  let macros' = Map.filterWithKey f macros
      f name _ | name `elem` toShadow = False
               | otherwise            = True
  putEnvMacros macros'
  result <- skc
  putEnvMacros macros
  return result

-- | Expands form, with taking care of @begin@ special form.
expands :: [Code] -> Skc [Code]
expands forms = do
    -- XXX: Get rid of unnecessary reverses.
    forms' <- mapM expand forms
    fmap reverse (foldM f [] forms')
  where
    f acc orig@(LForm (L _ form)) =
      case form of
        List (LForm (L _ (Atom (ASymbol "begin"))) : rest) -> do
          rest' <- expands rest
          return (reverse rest' ++ acc)
        _ -> return (orig : acc)

-- | Function to recursively expand the given 'Code'.
expand :: Code -> Skc Code
expand form =
  case unLForm form of
    L l (List forms) ->
      case forms of
        -- Expand `let' expression, `case' expression, function binding,
        -- and lambda, with shadowing the lexically bounded
        -- names. Expansion of other forms are done without name
        -- shadowing.
        kw@(LForm (L _ (Atom (ASymbol x)))):y:rest
          | x == "let"   -> expandLet l kw y rest
          | x == "case"  -> expandCase l kw y rest
          | x == "where" -> expandWhere l kw y rest
          | x == "=" ||
            x == "\\"    -> expandFunBind l kw (y:rest)
        _                -> expandList l List forms

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
          macros <- getEnvMacros
          case lookupMacro k macros of
           Just m -> case m of
              Macro f       -> f form >>= expand
              SpecialForm f -> f form >>= expand
           Nothing -> do
             rest' <- mapM expand rest
             return (LForm (L l (constr (sym:rest'))))
        _ -> do
          forms' <- mapM expand forms
          return (LForm (L l (constr forms')))


-- ---------------------------------------------------------------------
--
-- Auxiliary
--
-- ---------------------------------------------------------------------

tSym :: SrcSpan -> FastString -> Code
tSym l s = LForm (L l (Atom (ASymbol s)))

tChar :: SrcSpan -> Char -> Code
tChar l c = LForm (L l (Atom (AChar c)))

tString :: SrcSpan -> String -> Code
tString l s = LForm (L l (Atom (AString s)))

tInteger :: SrcSpan -> Integer -> Code
tInteger l n = LForm (L l (Atom (AInteger n)))

tFractional :: SrcSpan -> FractionalLit -> Code
tFractional l n = LForm (L l (Atom (AFractional n)))

tList :: SrcSpan -> [Code] -> Code
tList l forms = LForm (L l (List forms))

tHsList :: SrcSpan -> [Code] -> Code
tHsList l forms = LForm (L l (HsList forms))

mkQuoted :: SrcSpan -> Code -> Code
mkQuoted l form = tList l [tSym l "quoted", form]

emptyForm :: Code
emptyForm = tList skSrcSpan [tSym skSrcSpan "begin"]
