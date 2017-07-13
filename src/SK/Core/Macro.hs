{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Module for macros.
module SK.Core.Macro
  ( macroexpand
  , macroexpands
  , setExpanderSettings
  , withExpanderSettings
  , specialForms
  ) where

-- base
import Control.Monad (foldM, when)
import Unsafe.Coerce (unsafeCoerce)

-- Internal
import SK.Core.Form
import SK.Core.GHC
import SK.Core.Syntax (evalBuilder, parseExpr)
import SK.Core.SKC

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

tSym :: SrcSpan -> String -> LCode
tSym l s = L l (TAtom (ASymbol s))

tChar :: SrcSpan -> Char -> LCode
tChar l c = L l (TAtom (AChar c))

tString :: SrcSpan -> String -> LCode
tString l s = L l (TAtom (AString s))

tInteger :: SrcSpan -> Integer -> LCode
tInteger l n = L l (TAtom (AInteger n))

tFractional :: SrcSpan -> FractionalLit -> LCode
tFractional l n = L l (TAtom (AFractional n))

tList :: SrcSpan -> [LCode] -> LCode
tList l forms = L l (TList forms)

tHsList :: SrcSpan -> [LCode] -> LCode
tHsList l forms = L l (THsList forms)

quoteAtom :: SrcSpan -> Atom -> LCode
quoteAtom l form =
  case form of
    ASymbol s -> atom [tSym l "ASymbol", tString l s]
    AChar c -> atom [tSym l "AChar", tChar l c]
    AString s -> atom [tSym l "AString", tString l s]
    AInteger n -> atom [tSym l "AInteger", tInteger l n]
    AFractional n ->
      atom [tSym l "aFractional", tFractional l n]
    AUnit -> atom [tSym l "AUnit"]
    _ -> L l (TAtom form)
  where
    atom vals = tList l [tSym l "Atom", tList l vals]

quote :: LCode -> LCode
quote orig@(L l form) =
  case form of
    TAtom atom -> quoteAtom l atom
    TList xs -> tList l [tSym l "List", tHsList l (map quote xs)]
    THsList xs -> tList l [tSym l "HsList", tHsList l (map quote xs)]
    _ -> orig

isUnquoteSplice :: LCode -> Bool
isUnquoteSplice form =
  case form of
    L _ (TList (L _ (TAtom (ASymbol "unquote-splice")):_)) -> True
    _ -> False

quasiquote :: LCode -> LCode
quasiquote orig@(L l form) =
  case form of
    TList [L _ (TAtom (ASymbol "unquote")), rest] ->
      tList l [tSym l "toCode", rest]
    TList forms'
       | any isUnquoteSplice forms' ->
          tList l [ tSym l "List"
                  , tList l [tSym l "concat", tHsList l (go [] forms')]]
       | otherwise ->
          tList l [tSym l "List", tHsList l (map quasiquote forms')]
    THsList forms'
       | any isUnquoteSplice forms' ->
          tList l [ tSym l "HsList"
                  , tList l [tSym l "concat", tHsList l (go [] forms')]]
       | otherwise ->
          tList l [tSym l "HsList", tHsList l (map quasiquote forms')]
    TAtom atom -> quoteAtom l atom
    _       -> orig
  where
   go acc forms =
     let (pre, post) = break isUnquoteSplice forms
     in  case post of
           (L ls (TList (_:body)):post') ->
             go (acc ++ [tHsList l (map quasiquote pre)
                        ,tList ls [ tSym l "unquoteSplice"
                                  , tList l body]])
                     post'
           _ | null pre  -> acc
             | otherwise -> acc ++ [tHsList l (map quasiquote pre)]

-- Using `unsafeCoerce'.
compileMT :: LHsExpr RdrName -> Skc Macro
compileMT = fmap unsafeCoerce . compileParsedExpr
{-# INLINE compileMT #-}

putMacro :: LCode -> Skc ()
putMacro form =
  case form of
    L l (TList [(L _ (TAtom (ASymbol name))),arg,body]) -> do
      expanded <- macroexpand body
      let expr = tList l [ tSym l "::"
                         , tList l [tSym l "\\", arg, expanded]
                         , tSym l "Macro"]
      case evalBuilder parseExpr [expr] of
        Right hexpr -> do
          macro <- compileMT hexpr
          addMacro name macro
          return ()
        Left err -> skSrcError form err
    _ -> skSrcError form ("malformed macro: " ++
                      show (pForm (unLocForm form)))

mkIIDecl :: String -> InteractiveImport
mkIIDecl = IIDecl . simpleImportDecl . mkModuleName

emptyForm :: LCode
emptyForm = tList l0 [tSym l0 "begin"]
  where l0 = getLoc (noLoc ())

pTyThing :: DynFlags -> TyThing -> Skc ()
pTyThing dflags ty_thing@(AnId var) = do
  let str :: Outputable p => p -> String
      str = showPpr dflags
      prn = liftIO . putStrLn
      name = str (varName var)
      typ = str (varType var)
  when (typ == "Macro")
       (do prn (";;; adding macro `" ++
                 name ++
                 "' to current compiler session.")
           addImportedMacro ty_thing)
pTyThing dflags tt = pTyThing' dflags tt

-- Currently not in use during macro addition, just for printing out
-- information.
--
-- Using 'IfaceDecl' to detect the SK.Core.SKC.Macro type. This way is
-- more safe than comparing the Type of Var, since IfaceDecl contains
-- more detailed information.
pTyThing' :: DynFlags -> TyThing -> Skc ()
pTyThing' dflags tt@(AnId _) = do
  let ifd = tyThingToIfaceDecl tt
      name = ifName ifd
      typ = ifType ifd
      prn = liftIO . putStrLn
      str :: Outputable p => p -> String
      str = showPpr dflags
  prn (concat [";;; an id: name=", str name , " type=", str typ])
  case typ of
    -- Constructor name 'IfaceTyVar' changed sinde ghc 8.0.2 release.
    IfaceTyVar _ -> prn "free ty var"
    IfaceLitTy _ -> prn "lit ty"
    IfaceAppTy _ _ -> prn "app ty"
    IfaceFunTy _ _ -> prn "fun ty"
    IfaceDFunTy _ _ -> prn "dfun ty"
    IfaceForAllTy _ _ -> prn "for all ty"
    IfaceTyConApp con arg -> do
      let conName = str (ifaceTyConName con)
      prn (concat [ ";;; ty con app,"
                  , " tyConName=", conName
                  , " tyConArg=", str arg])
    IfaceCastTy _ _ -> prn "cast ty"
    IfaceCoercionTy _ -> prn "coercion ty"
    IfaceTupleTy _ _ _ -> prn "tuple ty"

pTyThing' dflags tt =
  -- Arguments of `pprTyThing' changed since ghc-8.0.2 release.
  liftIO (putStrLn (";;; " ++ (showSDocUnqual dflags (pprTyThing tt))))

addImportedMacro :: TyThing -> Skc ()
addImportedMacro ty_thing =
  case ty_thing of
    AnId var -> do
      hsc_env <- getSession
      let name = varName var
      fhv <- liftIO (getHValue hsc_env name)
      hv <- liftIO (withForeignRef fhv localRef)
      let macro = unsafeCoerce hv
      addMacro (showPpr (hsc_dflags hsc_env) name) macro
      return ()
    _ -> error "addImportedmacro"

m_quote :: Macro
m_quote form =
  case form of
    L _ (TList [_,body]) -> return (quote body)
    _ -> skSrcError form ("malformed quote at " ++ showLoc form)

m_quasiquote :: Macro
m_quasiquote form =
    case form of
      L _ (TList [_,body]) -> return (quasiquote body)
      _ -> skSrcError form ("malformed quasiquote at " ++ showLoc form)

m_defineMacro :: Macro
m_defineMacro form =
  case form of
    L l (TList [_,self@(L _ (TAtom (ASymbol name))),arg,body]) -> do
      body' <- macroexpand body
      let expr = tList l [ tSym l "::"
                         , tList l [tSym l "\\", arg, body']
                         , tSym l "Macro"]
      case evalBuilder parseExpr [expr] of
        Right hexpr -> do
          macro <- compileMT hexpr
          let decls = [tList l [tSym l "::", self, tSym l "Macro"]
                      ,tList l [tSym l "=", self, expr]]
          addMacro name macro
          return (tList l (tSym l "begin":decls))
        Left err -> skSrcError form err
    _ -> skSrcError form "define-macro: malformed body"

-- XXX: Does not preserve macros defined with `define-macro' inside
-- `let-macro' body. Need to update the SkEnv to hold the contents of
-- currently defined macros.
m_letMacro :: Macro
m_letMacro form =
  case form of
    L l1 (TList (_:L l2 (TList forms):rest)) -> do
      sk_env <- getSkEnv
      mapM_ putMacro forms
      expanded <- macroexpands rest
      putSkEnv sk_env
      return (tList l1 (tSym l2 "begin":expanded))
    _ -> skSrcError form ("let-macro: malformed args:\n" ++
                     show (pForm (unLocForm form)))

m_require :: Macro
m_require form =
  -- The `require' is implemented as special form, to support dependency
  -- resolution during compilation of multiple modules with `--make'
  -- command.
  --
  -- The special form `require' modifies the HscEnv at the time of macro
  -- expansion, to update the context in compile time session.
  --
  -- Need to clean up the context after compiling each file, but not yet
  -- done. Reason to clean up the context is, if not cleaned, all files
  -- passed via "--make" command will use the same interactive context,
  -- which could be confusing, and may cause unwanted name conflicts and
  -- overridings.
  --
  case form of
    L _l1 (TList [_,L _l2 (TAtom (ASymbol mname))]) -> do
      debugIO (putStrLn (";;; requiring " ++ mname))
      contexts <- getContext
      setContext (mkIIDecl mname : contexts)
      mdl <- lookupModule (mkModuleName mname) Nothing
      mb_minfo <- getModuleInfo mdl
      case mb_minfo of
        Just minfo ->
          do dflags <- getSessionDynFlags
             mapM_ (pTyThing dflags) (modInfoTyThings minfo)
             return emptyForm
        Nothing -> skSrcError form ("require: cannot find modinfo for "
                                 ++ mname)
    _ -> skSrcError form "require: malformed syntax."

m_varArgBinOp :: String -> Macro
m_varArgBinOp sym = \form ->
  case unLoc form of
    TList [op] -> return (L (getLoc op) (TList [mkOp op]))
    TList [op,arg] -> return (L (getLoc op) (TList [mkOp op, arg]))
    TList [op,arg1,arg2] -> return (L (getLoc op)
                                      (TList [mkOp op, arg1, arg2]))
    TList (_:rest) -> return (go rest)
    TList _        -> return form
    _              -> skSrcError form ("macroexpand error at " ++
                                   showLoc form ++ ", `" ++ sym ++ "'")
  where
    go [x,y] = combine x y
    go (x:xs) = combine x (go xs)
    go _ = error ("varArgBinOp: impossible happened with " ++ sym)
    mkOp x = L (getLoc x) (TAtom (ASymbol sym))
    combine x y = L (getLoc x) (TList [mkOp x, x, y])

specialForms :: [(String, Macro)]
specialForms =
  [("quote", m_quote)
  ,("quasiquote", m_quasiquote)
  ,("define-macro", m_defineMacro)
  ,("let-macro", m_letMacro)
  ,("require", m_require)

  -- XXX: Support for variable number of arguments for builtin binary
  -- operators may not a good idea, might remove. It will hide the types
  -- of operators, which could be confusing. Also, quoted '+' will
  -- always transformed '__+', which makes impossible to use the raw '+'
  -- symbol.
  --
  -- Binary operators defined in Prelude are hard coded,
  -- to support forms with variable number of arguments. In general,
  -- better not to expand with functions defined in Prelude when
  -- NoImplicitPrelude language extension was specified.
  --
  -- Also, when defining instance, qualified names are not allowed, need
  -- to cope such situations.
  --
  ,("__+", m_varArgBinOp "+")
  ,("__*", m_varArgBinOp "*")
  ]

-- | Add modules used during macro expansion to current context.
setExpanderSettings :: GhcMonad m => m ()
setExpanderSettings = do
  flags <- getSessionDynFlags
  _ <- setSessionDynFlags (flags { hscTarget = HscInterpreted
                                 , ghcLink = LinkInMemory
                                 , optLevel = 0 })
  setContext [mkIIDecl "Prelude", mkIIDecl "SK.Core"]

-- | Perform given action with DynFlags set for macroexpansion, used
-- this to preserve original DynFlags.
withExpanderSettings :: GhcMonad m => m a -> m a
withExpanderSettings act = do
  origFlags <- getSessionDynFlags
  setExpanderSettings
  ret <- act
  _ <- setSessionDynFlags origFlags
  return ret

-- | Expands form, with taking care of @begin@ special form.
macroexpands :: [LCode] -> Skc [LCode]
macroexpands forms = do
    -- XXX: Get rid of unnecessary reverses.
    forms' <- mapM macroexpand forms
    fmap reverse (foldM f [] forms')
  where
    f acc orig@(L _ form) =
      case form of
        TList (L _ (TAtom (ASymbol "begin")) : rest) -> do
          rest' <- macroexpands rest
          return (reverse rest' ++ acc)
        _ -> return (orig : acc)

-- This function recursively expand the result. Without recursively
-- calling macroexpand on the result, cannot expand macro-generating
-- macros.
macroexpand :: LCode -> Skc LCode
macroexpand form =
  -- liftIO (putStrLn ("expanding:\n" ++ show (pprForm (unLocForm form))))
  case form of
    -- Expand list of forms with preserving the constructor.
    L l (TList forms) -> expandList l TList forms
    L l (THsList forms) -> expandList l THsList forms

    -- The prefixes added to binary operators by tokenizer are removed
    -- at this point.
    L l (TAtom (ASymbol "__+")) -> return (L l (TAtom (ASymbol "+")))
    L l (TAtom (ASymbol "__*")) -> return (L l (TAtom (ASymbol "*")))

    -- Rest of the form are untouched.
    L _ _ -> return form
  where
    expandList l constr forms =
      case forms of
        sym@(L _ (TAtom (ASymbol k))) : rest -> do
          macros <- getMacroEnv
          case lookup k macros of
           Just f -> f form >>= macroexpand
           Nothing -> do
             rest' <- mapM macroexpand rest
             return (L l (constr (sym:rest')))
        _ -> do
          forms' <- mapM macroexpand forms
          return (L l (constr forms'))
