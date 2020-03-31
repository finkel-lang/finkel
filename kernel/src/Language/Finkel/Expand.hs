{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
-- | Module for macro expansion.
module Language.Finkel.Expand
  ( expand
  , expand1
  , expands
  , withExpanderSettings
  , specialForms
  , unquoteSplice
  ) where

#include "Syntax.h"

-- base
import           Control.Exception               (throw)
import           Control.Monad                   (foldM, when)
import           Control.Monad.IO.Class          (MonadIO (..))
import           Data.Char                       (isLower)
import           Data.Maybe                      (catMaybes)
import           GHC.Exts                        (unsafeCoerce#)

-- containers
import qualified Data.Map                        as Map

-- ghc
import           BasicTypes                      (FractionalLit (..),
                                                  SourceText (..))
import           DynFlags                        (DynFlags (..),
                                                  GeneralFlag (..),
                                                  GhcLink (..), HscTarget (..),
                                                  getDynFlags, gopt, gopt_unset,
                                                  updOptLevel, xopt_unset)
import           ErrUtils                        (compilationProgressMsg)
import           Exception                       (gbracket)
import           FastString                      (FastString, appendFS, fsLit,
                                                  headFS, unpackFS)
import           Finder                          (findImportedModule)
import           GHC                             (ModuleInfo, getModuleInfo,
                                                  lookupModule, lookupName,
                                                  modInfoExports, setContext)
import           GHC_Hs                          (HsModule (..))
import           GHC_Hs_ImpExp                   (ImportDecl (..), ieName)
import           GhcMonad                        (GhcMonad (..))
import           HscMain                         (Messager,
                                                  hscTcRnLookupRdrName,
                                                  showModuleIndex)
import           HscTypes                        (FindResult (..), HscEnv (..),
                                                  InteractiveImport (..),
                                                  showModMsg)
import           InteractiveEval                 (getContext)
import           MkIface                         (RecompileRequired (..),
                                                  recompileRequired)
import           Module                          (moduleNameString)
import           Name                            (nameOccName)
import           Outputable                      (showPpr)
import           RdrName                         (rdrNameOcc)
import           SrcLoc                          (GenLocated (..), SrcSpan,
                                                  unLoc)
import           TyCoRep                         (TyThing (..))
import           Var                             (varName)

-- ghc-boot
import qualified GHC.LanguageExtensions          as LangExt

-- Internal
import           Language.Finkel.Builder         (HImportDecl, evalBuilder,
                                                  syntaxErrMsg)
import           Language.Finkel.Eval
import           Language.Finkel.Fnk
import           Language.Finkel.Form
import           Language.Finkel.Homoiconic
import           Language.Finkel.Syntax          (parseDecls, parseExpr,
                                                  parseLImport, parseModule)
import           Language.Finkel.Syntax.SynUtils (cL, dL)


-- ---------------------------------------------------------------------
--
-- Quote
--
-- ---------------------------------------------------------------------

quote :: Bool -> Code -> Code
quote qual orig@(LForm (L l form)) =
  case form of
    Atom atom -> quoteAtom qual l atom
    List xs   -> quoteList (qListS qual)  xs
    HsList xs -> quoteList (qHsListS qual) xs
    _         -> orig
    where
      quoteList tag xs =
        tList l [tSym l tag, tHsList l (map (quote qual) xs)]

quoteAtom :: Bool -> SrcSpan -> Atom -> Code
quoteAtom qual l form =
  case form of
    ASymbol s     -> li [tSym l (qSymbolS qual), tString l NoSourceText s]
    AChar st c    -> li [tSym l (qCharS qual), tChar l st c]
    AString st s  -> li [tSym l (qStringS qual), tString l st s]
    AInteger il   -> li [tSym l (qIntegerS qual), tInteger l il]
    AFractional n -> li [tSym l (qFractionalS qual), tFractional l n]
    AUnit         -> tSym l (qUnitS qual)
  where
    li = tList l
{-# INLINE quoteAtom #-}

-- Quasiquote is implemented as special form in Haskell. Though it could
-- be implemented in Finkel code later. If done in Finkel code, lexer and reader
-- still need to handle the special case for backtick, comma, and
-- comma-at, because currently there's no way to define read macro.

quasiquote :: Bool -> Code -> Code
quasiquote qual orig@(LForm (L l form)) =
  case form of
    List [LForm (L _ (Atom (ASymbol ":unquote"))), x]
      | isUnquoteSplice x -> x
      | otherwise         -> tList l [tSym l (toCodeS qual), x]
    List forms'
      | [q, body] <- forms'
      , q == tSym l ":quasiquote"   -> qq (qq body)
      | any isUnquoteSplice forms' -> splicedList qListS forms'
      | otherwise                  -> nonSplicedList qListS forms'
    HsList forms'
      | any isUnquoteSplice forms' -> splicedList qHsListS forms'
      | otherwise                  -> nonSplicedList qHsListS forms'
    Atom atom                      -> quoteAtom qual l atom
    TEnd                           -> orig
  where
   splicedList tag forms =
     tList l [ tSym l (tag qual)
             , tList l [ tSym l (concatS qual)
                       , tHsList l (go [] forms)]]
   nonSplicedList tag forms =
     tList l [ tSym l (tag qual)
             , tHsList l (map qq forms)]
   go acc forms =
     let (pre, post) = break isUnquoteSplice forms
     in  case post of
           LForm (L ls (List (_:body))):post' ->
             go (acc ++ [tHsList l (map qq pre)
                        ,tList ls [ tSym l (unquoteSpliceS qual)
                                  , tList l body]])
                     post'
           _ | null pre  -> acc
             | otherwise -> acc ++ [tHsList l (map qq pre)]
   qq = quasiquote qual

isUnquoteSplice :: Code -> Bool
isUnquoteSplice (LForm form) =
  case form of
    L _ (List (LForm (L _ (Atom (ASymbol ":unquote-splice"))):_))
      -> True
    _ -> False
{-# INLINE isUnquoteSplice #-}

-- | Internally used by macro expander for @unquote-splice@ special
-- form.
unquoteSplice :: ToCode a => a -> [Code]
unquoteSplice form =
  case unCode (toCode form) of
    List xs             -> xs
    HsList xs           -> xs
    Atom AUnit          -> []
    Atom (AString _ xs) -> map toCode (unpackFS xs)
    _                   -> throw (FinkelException
                                    ("unquote splice: got " ++
                                     show (toCode form)))


-- ---------------------------------------------------------------------
--
-- Macro
--
-- ---------------------------------------------------------------------

coerceMacro :: Code -> Fnk Macro
coerceMacro name
  | LForm (L _ (Atom (ASymbol _))) <- name =
    do dflags <- getDynFlags
       case evalBuilder dflags parseExpr [name] of
         Right hexpr -> fmap unsafeCoerce# (evalExpr hexpr)
         Left err    -> failS (syntaxErrMsg err)
  | otherwise = failS ("coerceMacro: expecting name")

getTyThingsFromIDecl :: HImportDecl -> ModuleInfo -> Fnk [TyThing]
getTyThingsFromIDecl (L _ idecl) minfo = do
  -- 'toImportList' borrowed from local definition in
  -- 'TcRnDriver.tcPreludeClashWarn'.
  let exportedNames = modInfoExports minfo
      ieName' (dL->L l ie) = cL l (ieName ie)
      toImportList (h, dL->L _ loc) = (h, map ieName' loc)
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

addImportedMacro :: TyThing -> Fnk ()
addImportedMacro thing = when (isMacro thing) go
  where
    go = do
      dflags <- getDynFlags
      case thing of
        AnId var -> do
          debugFnk (";;; adding macro `" ++
                    showPpr dflags (varName var) ++
                    "' to current compiler session.")
          hsc_env <- getSession
          let name_str = showPpr (hsc_dflags hsc_env) (varName var)
              name_sym = toCode (aSymbol name_str)
          macro <- coerceMacro name_sym
          insertMacro (fsLit name_str) macro
        _ -> error "addImportedmacro"


-- ---------------------------------------------------------------------
--
-- Special forms
--
-- ---------------------------------------------------------------------

m_quote :: MacroFunction
m_quote form =
  case unLForm form of
    L l (List [_,body]) -> do
      qualify <- fmap envQualifyQuotePrimitives getFnkEnv
      let LForm (L _ body') = quote qualify body
      return (LForm (L l body'))
    _ -> finkelSrcError form ("malformed quote at " ++ showLoc form)

m_quasiquote :: MacroFunction
m_quasiquote form =
    case unLForm form of
      L l (List [_,body]) -> do
        qualify <- fmap envQualifyQuotePrimitives getFnkEnv
        let LForm (L _ body') = quasiquote qualify body
        return (LForm (L l body'))
      _ -> finkelSrcError form ("malformed quasiquote at " ++ showLoc form)

m_withMacro :: MacroFunction
m_withMacro form =
  case unLForm form of
    L l1 (List (_:LForm (L _ (List forms)):rest)) -> do
      fnkc_env0 <- getFnkEnv

      -- Expand body of `let-macro' with temporary macros.
      macros <- Map.fromList <$> mapM evalMacroDef forms
      let tmpMacros0 = envTmpMacros fnkc_env0
      putFnkEnv (fnkc_env0 {envTmpMacros = macros : tmpMacros0})
      expanded <- expands rest

      -- Getting 'FnkEnv' again, so that persistent macros defined inside
      -- the `let-macro' body could be used hereafter. Restoring
      -- tmporary macros to preserved value.
      fnkc_env1 <- getFnkEnv
      putFnkEnv (fnkc_env1 {envTmpMacros = tmpMacros0})

      case expanded of
        [x] -> return x
        _   -> return (tList l1 (tSym l1 ":begin" : expanded))
    _ -> finkelSrcError form ("with-macro: malformed args:\n" ++ show form)
  where
    evalMacroDef decl = do
      expanded <- expand decl
      case unCode expanded of
        List (_ : fname@(LForm (L _ (Atom (ASymbol name)))) : _) ->
          do dflags <- getDynFlags
             case evalBuilder dflags parseDecls [expanded] of
               Right hdecls -> do
                 (tythings, _ic) <- evalDecls hdecls
                 case tythings of
                   tything : _ | isMacro tything -> do
                     macro <- coerceMacro fname
                     return (name, macro)
                   _ -> finkelSrcError fname "with-macro: not a macro"
               Left err -> failS (syntaxErrMsg err)
        _ -> finkelSrcError decl "with-macro: malformed args"

m_require :: MacroFunction
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
      do dflags0 <- getDynFlags
         case evalBuilder dflags0 parseLImport code of
           Right lidecl@(L loc idecl) -> do
             hsc_env <- getSession
             fnkc_env <- getFnkEnv

             let dflags = hsc_dflags hsc_env
                 recomp = gopt Opt_ForceRecomp dflags
                 mname = unLoc (ideclName idecl)
                 mname' = moduleNameString mname
                 lmname' = L loc mname'

             debugFnk (";;; require: " ++ showPpr dflags idecl)

             -- Try finding the required module. Delegate the work to
             -- 'envMake' function stored in FnkEnv when the file is found
             -- in import paths.
             --
             -- N.B. 'findImportedModule' does not know ".fnk" file
             -- extension, so it will not return Finkel source files for
             -- home package modules.

             fresult <- liftIO (findImportedModule hsc_env mname Nothing)
             compiled <-
               case fresult of
                 Found {} -> return []
                 _        ->
                   case envMake fnkc_env of
                     Just mk -> withRequiredSettings (mk recomp lmname')
                     Nothing -> failS "require: no make function"

             -- Add the module to current compilation context.
             contexts <- getContext
             setContext (IIDecl idecl : contexts)

             -- Update required module names and compiled home modules to
             -- FnkEnv. These are used by the callee module (i.e. the module
             -- containing this 'require' form).
             let reqs = lmname':envRequiredModuleNames fnkc_env
                 fnkc_env' = fnkc_env {envRequiredModuleNames = reqs
                                      ,envCompiledInRequire = compiled}
             putFnkEnv fnkc_env'

             -- Look up Macros in parsed module, add to FnkEnv when found.
             mdl <- lookupModule mname Nothing
             mb_minfo <- getModuleInfo mdl
             case mb_minfo of
               Just minfo -> do
                 things <- getTyThingsFromIDecl lidecl minfo
                 mapM_ addImportedMacro things
                 return emptyForm
               Nothing ->
                 finkelSrcError form ("require: module " ++ mname' ++
                                  " not found.")
           Left err -> finkelSrcError form ("require: " ++ syntaxErrMsg err)
    _ -> finkelSrcError form "require: malformed body"

m_evalWhenCompile :: MacroFunction
m_evalWhenCompile form =
  case unLForm form of
    L l (List (_ : body)) -> do
      expanded <- expands body
      dflags <- getDynFlags
      case evalBuilder dflags parseModule expanded of
        Right (HsModule {hsmodDecls = decls}) -> do
          (tythings, _ic) <- evalDecls decls
          mapM_ addImportedMacro tythings
          return emptyForm
        Left err -> finkelSrcError (LForm (L l (List body)))
                               (syntaxErrMsg err)
    _ -> finkelSrcError form ("eval-when-compile: malformed body: " ++
                          show form)

-- | The special forms.  The macros listed in 'specialForms' are used
-- in default 'FnkEnv'.
specialForms :: EnvMacros
specialForms =
  makeEnvMacros
    [(":eval-when-compile", SpecialForm m_evalWhenCompile)
    ,(":with-macro", SpecialForm m_withMacro)
    ,(":quote", SpecialForm m_quote)
    ,(":quasiquote", SpecialForm m_quasiquote)
    ,(":require", SpecialForm m_require)]


-- ---------------------------------------------------------------------
--
-- Macro expander
--
-- ---------------------------------------------------------------------

-- | Set state for macro expansion.
--
-- Add modules used during macro expansion to current context, and set
-- some DynFlags fields.
setExpanderSettings :: Fnk ()
setExpanderSettings = do
  flags0 <- getDynFlags

  -- Setup DynFlags for interactive evaluation.
  let flags1 = flags0 { hscTarget = HscInterpreted }
      flags2 = gopt_unset flags1 Opt_Hpc
      flags3 = xopt_unset flags2 LangExt.MonomorphismRestriction
      flags4 = updOptLevel 0 flags3

  -- Save the original DynFlags for compiling required modules when not
  -- yet saved.
  fnkc_env <- getFnkEnv
  case envMakeDynFlags fnkc_env of
    Nothing -> putFnkEnv (fnkc_env {envMakeDynFlags = Just flags0})
    Just _  -> return ()

  setDynFlags flags4

-- | Perform given action with 'DynFlags' updated to perform
-- macroexpansion with interactive evaluation, then reset to preserved
-- original DynFlags.
withExpanderSettings :: Fnk a -> Fnk a
withExpanderSettings act =
  gbracket getDynFlags
           setDynFlags
           (const (setExpanderSettings >> act))

setRequiredSettings :: Fnk ()
setRequiredSettings = do
  fnkc_env <- getFnkEnv
  case envMakeDynFlags fnkc_env of
    Just dflags -> setDynFlags dflags {ghcLink = LinkInMemory}
    Nothing     -> failS "setRequiredSettings: missing DynFlags"
  putFnkEnv fnkc_env {envMessager = requiredMessager}

withRequiredSettings :: Fnk a -> Fnk a
withRequiredSettings act =
  gbracket
    (do dflags <- getDynFlags
        fnkc_env <- getFnkEnv
        return (dflags, fnkc_env))
    (\(dflags, fnkc_env) ->
       do setDynFlags dflags
          putFnkEnv fnkc_env)
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

-- | Perform 'Fnk' action with temporary shadowed macro environment.
withShadowing :: [FastString] -- ^ Names of macro to shadow.
              -> Fnk a -- ^ Action to perform.
              -> Fnk a
withShadowing toShadow fnkc = do
  fnkc_env <- getFnkEnv
  let emacros = envMacros fnkc_env
      tmacros = envTmpMacros fnkc_env
      f name _ | name `elem` toShadow = False
               | otherwise            = True
  putFnkEnv (fnkc_env { envMacros = Map.filterWithKey f emacros
                      , envTmpMacros = map (Map.filterWithKey f) tmacros })
  result <- fnkc
  putFnkEnv fnkc_env
  return result

-- | Expand forms, with taking care of @begin@ special form.
expands :: [Code] -> Fnk [Code]
expands forms = fmap concat (mapM expand' forms)

-- | Expand form to list of 'Code', supports special form /begin/.
expand' :: Code -> Fnk [Code]
expand' form = do
  form' <- expand form
  case unCode form' of
    List (LForm (L _ (Atom (ASymbol ":begin"))):rest) ->
      case rest of
        [] -> return []
        _  -> expands rest
    _ -> return [form']
{-# INLINE expand' #-}

-- | Recursively expands the given 'Code'.
expand :: Code -> Fnk Code
expand form =
  case unLForm form of
    L l (List forms) ->
      case forms of
        -- Expand `let' expression, `do' expression, `case' expression,
        -- lambda expression and function binding with shadowing the
        -- lexically bounded names. Expansion of other forms are done
        -- without name shadowing.
        kw@(LForm (L _ (Atom (ASymbol x)))):y:rest
          | x == "let"            -> expandLet l kw y rest
          | x == "do"             -> expandDo l kw (y:rest)
          | x == "case"           -> expandCase l kw y rest
          | x == "where"          -> expandWhere l kw y rest
          | x == "=" || x == "\\" -> expandFunBind l kw (y:rest)
        _                         -> expandList l forms

    L l (HsList forms) ->
      -- Without recursively calling 'expand' on the result, cannot
      -- expand macro-generating macros.
      LForm . L l . HsList <$> expands forms

    -- Non-list forms are untouched.
    _ -> return form
  where
    expandLet l kw binds body = do
      binds' <- expand binds
      let bounded = boundedNames binds'
      body' <- withShadowing bounded (expands body)
      return (LForm (L l (List (kw:binds':body'))))

    expandDo l kw body = do
      (_, body') <- foldM expandInDo ([], []) body
      return (LForm (L l (List (kw:reverse body'))))

    expandFunBind l kw rest = do
      let args = init rest
          body = last rest
          bounded = concatMap boundedNameOne args
      args' <- expands args
      body' <- withShadowing bounded (expand body)
      return (LForm (L l (List (kw:args'++[body']))))

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
      return (LForm (L l (List (kw:expr':reverse rest'))))

    expandWhere l kw expr rest = do
      rest' <- expands rest
      let bounded = concatMap boundedName rest'
      expr' <- withShadowing bounded (expand expr)
      return (LForm (L l (List (kw:expr':rest'))))

    expandList l forms =
      case forms of
        sym@(LForm (L _ (Atom (ASymbol k)))) : rest -> do
          fnkc_env <- getFnkEnv
          case lookupMacro k fnkc_env of
            Just (Macro f)       -> f form >>= expand
            Just (SpecialForm f) -> f form >>= expand
            Nothing              -> do
              rest' <- expands rest
              return (LForm (L l (List (sym:rest'))))
        _ -> do
          forms' <- expands forms
          return (LForm (L l (List forms')))

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


-- ---------------------------------------------------------------------
--
-- Auxiliary
--
-- ---------------------------------------------------------------------

tSym :: SrcSpan -> FastString -> Code
tSym l s = LForm (L l (Atom (ASymbol s)))
{-# INLINE tSym #-}

tChar :: SrcSpan -> SourceText -> Char -> Code
tChar l st c = LForm (L l (Atom (AChar st c)))
{-# INLINE tChar #-}

tString :: SrcSpan -> SourceText -> FastString -> Code
tString l st s = LForm (L l (Atom (AString st s)))
{-# INLINE tString #-}

tInteger :: SrcSpan -> IntegralLit -> Code
tInteger l il = LForm (L l (Atom (AInteger il)))
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

emptyForm :: Code
emptyForm =
  LForm (genSrc (List [LForm (genSrc (Atom (ASymbol ":begin")))]))
{-# INLINE emptyForm #-}

-- Note: Qualified names for quoting functions
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Quoting functions can use qualified name after expansion, to
-- support quote in REPL without importing the "Language.Finkel"
-- module.  See how "Opt_ImplicitImportQualified" flag is set in
-- initialization code of Finkel REPL in "finkel-tool" package.

type Quote = Bool -> FastString

quoteWith :: FastString -> Quote
quoteWith name qualify =
  if qualify
     then appendFS "Language.Finkel."  name
     else name
{-# INLINE quoteWith #-}

qListS :: Quote
qListS = quoteWith "qList"
{-# INLINE qListS #-}

qHsListS :: Quote
qHsListS = quoteWith "qHsList"
{-# INLINE qHsListS #-}

qSymbolS :: Quote
qSymbolS = quoteWith "qSymbol"
{-# INLINE qSymbolS #-}

qCharS :: Quote
qCharS = quoteWith "qChar"
{-# INLINE qCharS #-}

qStringS :: Quote
qStringS = quoteWith "qString"
{-# INLINE qStringS #-}

qIntegerS :: Quote
qIntegerS = quoteWith "qInteger"
{-# INLINE qIntegerS #-}

qFractionalS :: Quote
qFractionalS = quoteWith "qFractional"
{-# INLINE qFractionalS #-}

qUnitS :: Quote
qUnitS = quoteWith "qUnit"
{-# INLINE qUnitS #-}

toCodeS :: Quote
toCodeS = quoteWith "toCode"
{-# INLINE toCodeS #-}

unquoteSpliceS :: Quote
unquoteSpliceS = quoteWith "unquoteSplice"
{-# INLINE unquoteSpliceS #-}

concatS :: Quote
concatS qual =
  if qual
     then "Data.Foldable.concat"
     else "concat"
{-# INLINE concatS #-}
