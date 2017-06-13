{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Module for macros.
module SK.Core.Macro
  ( macroexpand
  , evalExpanded
  , returnE
  ) where

import Control.Monad.IO.Class
import Unsafe.Coerce

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State

-- ghc-paths
import GHC.Paths (libdir)
-- import qualified Outputable

import qualified GHC.LanguageExtensions as LangExt

-- Internal
import SK.Core.Form
import SK.Core.GHC
import SK.Core.FormParser (evalBuilder, p_expr, showLoc)

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

newtype Expanded a = Expanded {
  unExpanded :: StateT [(String, LMacro)] (ExceptT String Ghc) a
} deriving (Functor, Applicative, Monad, MonadIO)

runExpanded :: Expanded a -> [(String, LMacro)]
            -> Ghc (Either String (a, [(String, LMacro)]))
runExpanded m st = runExceptT (runStateT (unExpanded m) st)

evalExpanded :: Expanded a -> IO (Either String a)
evalExpanded m =
  defaultErrorHandler
    defaultFatalMessager
    defaultFlushOut
    (runGhc
       (Just libdir)
       (do origFlags <- getSessionDynFlags
           _ <- setSessionDynFlags
                  (origFlags { hscTarget = HscInterpreted
                             , ghcLink = LinkInMemory })
           let decl = IIDecl . simpleImportDecl . mkModuleName
           setContext
             (map decl ["Prelude", "SK.Core.Form", "SK.Core.Macro"])

           -- Run the macroexpansion computation.
           ret <- runExpanded m builtinMacros

           case ret of
             Right (a, _) -> do
               _ <- setSessionDynFlags origFlags
               return (Right a)
             Left err     -> return (Left err)))

returnE :: a -> Expanded a
returnE = return

failE :: String -> Expanded a
failE msg = Expanded (lift (throwE msg))

getMacroEnv :: Expanded [(String, LMacro)]
getMacroEnv = Expanded get

extendMacroEnv :: String -> LMacro -> Expanded ()
extendMacroEnv name mac = Expanded go
  where
    go = do
      env <- get
      put ((name, mac) : env)

instance ExceptionMonad Expanded where
  gcatch m h =
    Expanded
      (StateT
         (\st ->
            (ExceptT
               (runExpanded m st `gcatch` \e -> runExpanded (h e) st))))
  gmask f =
    Expanded
      (StateT
         (\st ->
            (ExceptT
               (gmask
                  (\r ->
                     let g m =
                           Expanded
                             (StateT
                                (\st' ->
                                   ExceptT (r (runExpanded m st'))))
                     in  runExpanded (f g) st)))))

instance HasDynFlags Expanded where
   getDynFlags = Expanded (lift getDynFlags)

instance GhcMonad Expanded where
   getSession = Expanded (lift (lift getSession))
   setSession s = Expanded (lift (lift (setSession s)))

-- | Macro transformer with location information preserved.
type LMacro = LTForm Atom -> Expanded (LTForm Atom)

tSym :: SrcSpan -> String -> LTForm Atom
tSym l s = L l (TAtom (ASymbol s))

tString :: SrcSpan -> String -> LTForm Atom
tString l s = L l (TAtom (AString s))

tInteger :: SrcSpan -> Integer -> LTForm Atom
tInteger l n = L l (TAtom (AInteger n))

tList :: SrcSpan -> [LTForm Atom] -> LTForm Atom
tList l forms = L l (TList forms)

tHsList :: SrcSpan -> [LTForm Atom] -> LTForm Atom
tHsList l forms = L l (THsList forms)

quoteAtom :: LTForm Atom -> LTForm Atom
quoteAtom orig@(L l form) =
  case form of
    TAtom (ASymbol s) -> atom [tSym l "ASymbol", tString l s]
    TAtom (AString s) -> atom [tSym l "AString", tString l s]
    TAtom (AInteger n) -> atom [tSym l "AInteger", tInteger l n]
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
     rest
     -- tList l [ tSym l "toForm", rest ]
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
compileMT :: LHsExpr RdrName -> Expanded LMacro
compileMT = fmap unsafeCoerce . compileParsedExpr

m_defMacroTransformer :: LTForm Atom -> Expanded (LTForm Atom)
m_defMacroTransformer form =
  case form of
    L l (TList [_,L _ (TAtom (ASymbol name)),body]) -> do
      expanded <- macroexpand body
      -- liftIO (print (pForm (lTFormToForm expanded)))
      case evalBuilder p_expr [expanded] of
        Right hexpr -> do
          -- flags <- getSessionDynFlags
          -- liftIO (putStrLn
          --           (Outputable.showSDoc flags (Outputable.ppr hexpr)))
          newMacro <- compileMT hexpr
          extendMacroEnv name newMacro
          return (tList l [tSym l "=", tSym l name, body])
        Left err -> failE err
    _ -> failE "malformed macro"

m_quote :: LMacro
m_quote form =
  case form of
    L _ (TList [_,body]) -> return (quote body)
    _ -> failE ("malformed quote at " ++ showLoc form)

m_quasiquote :: LMacro
m_quasiquote form =
    case form of
      L _ (TList [_,body]) -> return (quasiquote body)
      _ -> failE ("malformed quasiquote at " ++ showLoc form)

-- | Alias for @=@.
m_defn :: LMacro
m_defn form =
  case form of
    L l0 (TList [L l1 _, lhs, body]) ->
      return (L l0 (TList [ L l1 (TAtom (ASymbol "=")), lhs, body ]))
    _ -> failE ("macroexpand error at " ++ showLoc form ++ ", `defn'")

m_varArgBinOp :: String -> LMacro
m_varArgBinOp sym = \form ->
  case unLoc form of
    TList [op] -> return (L (getLoc op) (TList [mkOp op]))
    TList [op,arg] -> return (L (getLoc op) (TList [mkOp op, arg]))
    TList [op,arg1,arg2] -> return (L (getLoc op)
                                      (TList [mkOp op, arg1, arg2]))
    TList (_:rest) -> return (go rest)
    TList _        -> return form
    _              -> failE ("macroexpand error at " ++
                                showLoc form ++ ", `" ++ sym ++ "'")
  where
    go [x,y] = combine x y
    go (x:xs) = combine x (go xs)
    go _ = error ("varArgBinOp: impossible happened with " ++ sym)
    mkOp x = L (getLoc x) (TAtom (ASymbol sym))
    combine x y = L (getLoc x) (TList [mkOp x, x, y])

-- m_defmacro :: LMacro
-- m_defmacro _ = failE "defmacro not yet supported."

builtinMacros :: [(String, LMacro)]
builtinMacros =
  [("quote", m_quote)
  ,("quasiquote", m_quasiquote)
  ,("defn", m_defn)
  ,("defmacro-transformer", m_defMacroTransformer)

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

-- This function recursively expand the result. Without recursively
-- calling macroexpand on the result, cannot expand macro-generating
-- macros.
macroexpand :: LTForm Atom -> Expanded (LTForm Atom)
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
