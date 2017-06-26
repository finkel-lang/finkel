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
import Control.Monad (foldM)
import Unsafe.Coerce

-- ghc-paths
import GHC.Paths (libdir)

-- Internal
import SK.Core.Form
import SK.Core.GHC
import SK.Core.FormParser (evalBuilder, p_expr, showLoc)
import SK.Core.SKC

-- Macro expansion
-- ~~~~~~~~~~~~~~~
--
-- Consider how to support User defined macros. Need to load the form
-- transforming functions defined by the user. May restrict those
-- functions to imported module in current target file. Need to take
-- some kind of GHC environment value to expand macros.
--
-- Hy separates the loading of modules for runtime with `import',
-- between the loading of modules for macro expansion with `require'.

tSym :: SrcSpan -> String -> LTForm Atom
tSym l s = L l (TAtom (ASymbol s))

tChar :: SrcSpan -> Char -> LTForm Atom
tChar l c = L l (TAtom (AChar c))

tString :: SrcSpan -> String -> LTForm Atom
tString l s = L l (TAtom (AString s))

tInteger :: SrcSpan -> Integer -> LTForm Atom
tInteger l n = L l (TAtom (AInteger n))

tFractional :: SrcSpan -> FractionalLit -> LTForm Atom
tFractional l n = L l (TAtom (AFractional n))

tList :: SrcSpan -> [LTForm Atom] -> LTForm Atom
tList l forms = L l (TList forms)

tHsList :: SrcSpan -> [LTForm Atom] -> LTForm Atom
tHsList l forms = L l (THsList forms)

quoteAtom :: LTForm Atom -> LTForm Atom
quoteAtom orig@(L l form) =
  case form of
    TAtom (ASymbol s) -> atom [tSym l "ASymbol", tString l s]
    TAtom (AChar c) -> atom [tSym l "AChar", tChar l c]
    TAtom (AString s) -> atom [tSym l "AString", tString l s]
    TAtom (AInteger n) -> atom [tSym l "AInteger", tInteger l n]
    TAtom (AFractional n) ->
      atom [tSym l "aFractional", tFractional l n]
    TAtom AUnit -> atom [tSym l "AUnit"]
    _ -> orig
  where
    atom vals = tList l [tSym l "Atom", tList l vals]

quote :: LTForm Atom -> LTForm Atom
quote orig@(L l form) =
  case form of
    TAtom _ -> quoteAtom orig
    TList xs ->
      tList l [tSym l "List", (tHsList l (map quote xs)) ]
    _ -> orig

isUnquoteSplice :: LTForm Atom -> Bool
isUnquoteSplice form =
  case form of
    (L _ (TList (L _ (TAtom (ASymbol "unquote-splice")):_))) -> True
    _ -> False

quasiquote :: LTForm Atom -> LTForm Atom
quasiquote orig@(L l form) =
  case form of
   TList [L _ (TAtom (ASymbol "unquote")), rest] ->
     -- rest
     tList l [ tSym l "toForm", rest ]
   TList forms' | any isUnquoteSplice forms' ->
     tList l [ tSym l "List"
             , tList l [tSym l "concat", tHsList l (go [] forms')]]
   TList forms' ->
     tList l [ tSym l "List"
             , tHsList l (map quasiquote forms')]
   TAtom _ -> quoteAtom orig
   _       -> orig
  where
   go acc forms =
     let (pre, post) = span (not . isUnquoteSplice) forms
     in  case post of
           (L ls (TList (_:body)):post') ->
             go (acc ++ [tHsList l (map quasiquote pre)
                        ,tList ls [tSym l "splice", tList l body]])
                     post'
           _ | null pre  -> acc
             | otherwise -> acc ++ [tHsList l (map quasiquote pre)]

-- Using `unsafeCoerce'.
compileMT :: LHsExpr RdrName -> Skc (Form Atom -> Skc (Form Atom))
compileMT = fmap unsafeCoerce . compileParsedExpr
{-# INLINE compileMT #-}

putMacro :: LTForm Atom -> Skc [LTForm Atom]
putMacro form =
  case form of
    L l (TList [self@(L _ (TAtom (ASymbol name))),args,body]) -> do
      expanded <- macroexpand body
      let tsig = tList l [tSym l "::", self, tSym l "Macro"]
          self' = tList l [ tSym l "=", self
                          , wrapArgs name args expanded]
          expr = tList l [ tSym l "let", tList l [tsig, self']
                         , self]
      case evalBuilder p_expr (Just ("macro:"++ name)) [expr] of
        Right hexpr -> do
          macro <- compileMT hexpr
          let wrap f form = fmap nlForm (f (cdr (lTFormToForm form)))
          addMacro name (wrap macro)
          return [tsig, self']
        Left err -> failS err
    _ -> failS ("malformed macro: " ++ show (pForm (lTFormToForm form)))

wrapArgs :: String -> LTForm Atom -> LTForm Atom -> LTForm Atom
wrapArgs name args@(L l1 _) body0=
  let sym = tSym l1
      list = tList l1
      form = sym "form"
      message =
        list [ sym "failS"
             , list [ sym "++"
                    , tString l1
                              (concat [ "macroexpand error with `"
                                      , name, "' at "
                                      , showLoc form, "\n"
                                      , "arg mismatch: "])
                    , list [sym "show", list [sym "pForm", form]]]]
      body1 =  list [ sym "case", form
                    , mkPat args, body0
                    , sym "_", message ]
      mkPat (L l x) =
        case x of
          TList xs -> list [sym "List", tHsList l (map mkPat xs)]
          s@(TAtom (ASymbol y)) -> L l s
  in  list [sym  "\\", list [form], body1]

m_defmacro :: LMacro
m_defmacro form =
  case form of
    L l (TList (_:self@(L _ (TAtom (ASymbol name))):rest)) -> do
      decls <- putMacro (L l (TList (self:rest)))
      return (tList l (tSym l "begin":decls))
    _ -> failS "defmacro: malformed macro"

m_quote :: LMacro
m_quote form =
  case form of
    L _ (TList [_,body]) -> return (quote body)
    _ -> failS ("malformed quote at " ++ showLoc form)

m_quasiquote :: LMacro
m_quasiquote form =
    case form of
      L _ (TList [_,body]) -> return (quasiquote body)
      _ -> failS ("malformed quasiquote at " ++ showLoc form)

-- | Alias for @=@.
m_defn :: LMacro
m_defn form =
  case form of
    L l0 (TList [L l1 _, lhs, body]) ->
      return (L l0 (TList [ L l1 (TAtom (ASymbol "=")), lhs, body ]))
    _ -> failS ("macroexpand error at " ++ showLoc form ++ ", `defn'")

m_varArgBinOp :: String -> LMacro
m_varArgBinOp sym = \form ->
  case unLoc form of
    TList [op] -> return (L (getLoc op) (TList [mkOp op]))
    TList [op,arg] -> return (L (getLoc op) (TList [mkOp op, arg]))
    TList [op,arg1,arg2] -> return (L (getLoc op)
                                      (TList [mkOp op, arg1, arg2]))
    TList (_:rest) -> return (go rest)
    TList _        -> return form
    _              -> failS ("macroexpand error at " ++
                             showLoc form ++ ", `" ++ sym ++ "'")
  where
    go [x,y] = combine x y
    go (x:xs) = combine x (go xs)
    go _ = error ("varArgBinOp: impossible happened with " ++ sym)
    mkOp x = L (getLoc x) (TAtom (ASymbol sym))
    combine x y = L (getLoc x) (TList [mkOp x, x, y])

-- XXX: Does not preserve macros defined with `defmacro' inside
-- `macrolet' body. Need to update the SkEnv to hold the contents of
-- currently defined macros.
m_macrolet :: LMacro
m_macrolet form =
  case form of
    L l1 (TList (_:L l2 (TList forms):rest)) -> do
      sk_env <- getSkEnv
      mapM_ putMacro forms
      expanded <- macroexpands rest
      putSkEnv sk_env
      return (tList l1 (tSym l2 "begin":expanded))
    _ -> failS ("macrolet: malformed macro: " ++
               show (pForm (lTFormToForm form)))

specialForms :: [(String, LMacro)]
specialForms =
  [("quote", m_quote)
  ,("quasiquote", m_quasiquote)
  ,("macrolet", m_macrolet)
  ,("defn", m_defn)
  ,("defmacro", m_defmacro)

  -- Binary operators defined in Prelude are hard coded, to support
  -- forms with variable number of arguments. In general, better not to
  -- expand with functions defined in Prelude when NoImplicitPrelude
  -- language extension was specified.
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
  let decl = IIDecl . simpleImportDecl . mkModuleName
  setContext [decl "Prelude", decl "SK.Core"]

-- | Perform given action with DynFlags set for macroexpansion, used
-- this to preserve original DynFlags.
withExpanderSettings :: GhcMonad m => m a -> m a
withExpanderSettings act = do
  origFlags <- getSessionDynFlags
  setExpanderSettings
  ret <- act
  setSessionDynFlags origFlags
  return ret

-- | Expands form, with taking care of @begin@ special form.
macroexpands :: [LTForm Atom] -> Skc [LTForm Atom]
macroexpands forms = do
    -- XXX: Get rid of unnecessary reverses.
    forms' <- mapM macroexpand forms
    fmap reverse (foldM f [] forms')
  where
    f acc orig@(L l form) =
      case form of
        TList (L _ (TAtom (ASymbol "begin")) : rest) -> do
          rest' <- macroexpands rest
          return (reverse rest' ++ acc)
        _ -> return (orig : acc)

-- This function recursively expand the result. Without recursively
-- calling macroexpand on the result, cannot expand macro-generating
-- macros.
macroexpand :: LTForm Atom -> Skc (LTForm Atom)
macroexpand form = do
  -- liftIO (putStrLn ("expanding:\n" ++ show (pprForm (lTFormToForm form))))
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
