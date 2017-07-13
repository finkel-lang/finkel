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


-- ---------------------------------------------------------------------
--
-- Quote
--
-- ---------------------------------------------------------------------

quoteAtom :: SrcSpan -> Atom -> Code
quoteAtom l form =
  case form of
    ASymbol s -> atom [tSym l "ASymbol", tString l s]
    AChar c -> atom [tSym l "AChar", tChar l c]
    AString s -> atom [tSym l "AString", tString l s]
    AInteger n -> atom [tSym l "AInteger", tInteger l n]
    AFractional n -> atom [tSym l "aFractional", tFractional l n]
    AUnit -> atom [tSym l "AUnit"]
    _ -> L l (Atom form)
  where
    atom vals = mkQuoted l (tList l [tSym l "Atom", tList l vals])

quote :: Code -> Code
quote orig@(L l form) =
  case form of
    Atom atom -> quoteAtom l atom
    List xs ->
      mkQuoted l (tList l [tSym l "List", tHsList l (map quote xs)])
    HsList xs ->
      mkQuoted l (tList l [tSym l "HsList", tHsList l (map quote xs)])
    _ -> orig

-- Quasiquote is currently implemented in Haskell. Though it could be
-- implemented with SK code later. If done in SK code, lexer and reader
-- still need to handle the special case for backtick, comma, and
-- comma-at, because currently there's no way to define read macro.

isUnquoteSplice :: Code -> Bool
isUnquoteSplice form =
  case form of
    L _ (List (L _ (Atom (ASymbol "unquote-splice")):_)) -> True
    _ -> False

quasiquote :: Code -> Code
quasiquote orig@(L l form) =
  case form of
    List [L _ (Atom (ASymbol "unquote")), x] ->
      tList l [tSym l "toCode", x]
    List forms'
       | any isUnquoteSplice forms' ->
          mkQuoted l (tList l [tSym l "List"
                              ,tList l [tSym l "concat"
                                       ,tHsList l (go [] forms')]])
       | otherwise ->
          mkQuoted l (tList l [tSym l "List"
                              ,tHsList l (map quasiquote forms')])
    HsList forms'
       | any isUnquoteSplice forms' ->
         mkQuoted l (tList l [ tSym l "HsList"
                              , tList l [tSym l "concat"
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
           (L ls (List (_:body)):post') ->
             go (acc ++ [tHsList l (map quasiquote pre)
                        ,tList ls [ tSym l "unquoteSplice"
                                  , tList l body]])
                     post'
           _ | null pre  -> acc
             | otherwise -> acc ++ [tHsList l (map quasiquote pre)]


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

-- Using `unsafeCoerce'.
compileMT :: LHsExpr RdrName -> Skc Macro
compileMT = fmap unsafeCoerce . compileParsedExpr
{-# INLINE compileMT #-}

putMacro :: Code -> Skc ()
putMacro form =
  case form of
    L l (List [(L _ (Atom (ASymbol name))),arg,body]) -> do
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
                      show (pForm form))

mkIIDecl :: String -> InteractiveImport
mkIIDecl = IIDecl . simpleImportDecl . mkModuleName

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
    -- Constructor name 'IfaceTyVar' changed since ghc 8.0.2 release.
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


-- ---------------------------------------------------------------------
--
-- Special forms
--
-- ---------------------------------------------------------------------

mkVarArgBinOp :: String -> Macro
mkVarArgBinOp sym = \form ->
  case unLoc form of
    List [op] -> return (L (getLoc op) (List [mkOp op]))
    List [op,arg] -> return (L (getLoc op) (List [mkOp op, arg]))
    List [op,arg1,arg2] -> return (L (getLoc op)
                                      (List [mkOp op, arg1, arg2]))
    List (_:rest) -> return (go rest)
    List _        -> return form
    _              -> skSrcError form ("macroexpand error at " ++
                                   showLoc form ++ ", `" ++ sym ++ "'")
  where
    go [x,y] = combine x y
    go (x:xs) = combine x (go xs)
    go _ = error ("varArgBinOp: impossible happened with " ++ sym)
    mkOp x = L (getLoc x) (Atom (ASymbol sym))
    combine x y = L (getLoc x) (List [mkOp x, x, y])

m_quote :: Macro
m_quote form =
  case form of
    L l (List [_,body]) ->
      let (L _ body') = quote body
      in  return (L l body')
    _ -> skSrcError form ("malformed quote at " ++ showLoc form)

m_quasiquote :: Macro
m_quasiquote form =
    case form of
      L l (List [_,body]) ->
        let (L _ body') = quasiquote body
        in  return (L l body')
      _ -> skSrcError form ("malformed quasiquote at " ++ showLoc form)

m_defineMacro :: Macro
m_defineMacro form =
  case form of
    L l (List [_,self@(L _ (Atom (ASymbol name))),arg,body]) -> do
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

-- XXX: When macros defined with `define-macro' have same name, old
-- macros will be overridden by `let-macro'. Need to update the SkEnv to
-- hold the list of macro name-function lookup table ...
m_letMacro :: Macro
m_letMacro form =
  case form of
    L l1 (List (_:L l2 (List forms):rest)) -> do
      sk_env <- getSkEnv
      mapM_ putMacro forms
      expanded <- macroexpands rest
      putSkEnv sk_env
      return (tList l1 (tSym l2 "begin":expanded))
    _ -> skSrcError form ("let-macro: malformed args:\n" ++
                     show (pForm form))

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
    L _l1 (List [_,L _l2 (Atom (ASymbol mname))]) -> do
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
  ,("__+", mkVarArgBinOp "+")
  ,("__*", mkVarArgBinOp "*")]


-- ---------------------------------------------------------------------
--
-- Macro expander
--
-- ---------------------------------------------------------------------

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
macroexpands :: [Code] -> Skc [Code]
macroexpands forms = do
    -- XXX: Get rid of unnecessary reverses.
    forms' <- mapM macroexpand forms
    fmap reverse (foldM f [] forms')
  where
    f acc orig@(L _ form) =
      case form of
        List (L _ (Atom (ASymbol "begin")) : rest) -> do
          rest' <- macroexpands rest
          return (reverse rest' ++ acc)
        _ -> return (orig : acc)

-- This function recursively expand the result. Without recursively
-- calling macroexpand on the result, cannot expand macro-generating
-- macros.
macroexpand :: Code -> Skc Code
macroexpand form =
  -- liftIO (putStrLn ("expanding:\n" ++ show (pprForm (unLocForm form))))
  case form of
    -- Expand list of forms with preserving the constructor.
    L l (List forms) -> expandList l List forms
    L l (HsList forms) -> expandList l HsList forms

    -- The prefixes added to binary operators by tokenizer are removed
    -- at this point.
    L l (Atom (ASymbol "__+")) -> return (L l (Atom (ASymbol "+")))
    L l (Atom (ASymbol "__*")) -> return (L l (Atom (ASymbol "*")))

    -- Rest of the form are untouched.
    L _ _ -> return form
  where
    expandList l constr forms =
      case forms of
        sym@(L _ (Atom (ASymbol k))) : rest -> do
          macros <- getMacroEnv
          case lookup k macros of
           Just f -> f form >>= macroexpand
           Nothing -> do
             rest' <- mapM macroexpand rest
             return (L l (constr (sym:rest')))
        _ -> do
          forms' <- mapM macroexpand forms
          return (L l (constr forms'))


-- ---------------------------------------------------------------------
--
-- Auxiliary
--
-- ---------------------------------------------------------------------

tSym :: SrcSpan -> String -> Code
tSym l s = L l (Atom (ASymbol s))

tChar :: SrcSpan -> Char -> Code
tChar l c = L l (Atom (AChar c))

tString :: SrcSpan -> String -> Code
tString l s = L l (Atom (AString s))

tInteger :: SrcSpan -> Integer -> Code
tInteger l n = L l (Atom (AInteger n))

tFractional :: SrcSpan -> FractionalLit -> Code
tFractional l n = L l (Atom (AFractional n))

tList :: SrcSpan -> [Code] -> Code
tList l forms = L l (List forms)

tHsList :: SrcSpan -> [Code] -> Code
tHsList l forms = L l (HsList forms)

mkQuoted :: SrcSpan -> Code -> Code
mkQuoted l form = tList l [tSym l "quoted", form]

emptyForm :: Code
emptyForm = tList skSrcSpan [tSym skSrcSpan "begin"]
