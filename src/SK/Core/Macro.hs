{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
-- | Module for macros.
module SK.Core.Macro
  ( macroexpand
  , macroexpands
  , setExpanderSettings
  , withExpanderSettings
  , compileDecls
  , specialForms
  ) where

-- base
import Control.Monad (foldM, when)
import Unsafe.Coerce (unsafeCoerce)

-- ghc
import ByteCodeGen (byteCodeGen)
import ConLike (ConLike(..))
import CorePrep (corePrepPgm)
import CoreSyn (bindersOfBinds)
import Desugar (deSugar)
import ErrUtils (Messages)
import HscMain (hscSimplify)
import HscTypes ( CgGuts(..), ModDetails(..), ModGuts(..)
                , InteractiveContext(..), extendInteractiveContext )
import Id (idName, isDFunId, isImplicitId)
import InteractiveEval (compileParsedExprRemote)
import Linker (linkDecls)
import Name (isExternalName)
import SrcLoc (srcLocSpan)
import TcRnDriver (tcRnDeclsi)
import TcRnTypes (TcGblEnv(..))
import TidyPgm (tidyProgram)
import TyCon (isDataTyCon, isImplicitTyCon)
import Util (filterOut)

-- Internal
import SK.Core.Builder (HExpr, HDecl)
import SK.Core.Form
import SK.Core.GHC
import SK.Core.Syntax (evalBuilder, parseExpr, parseModule)
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

isUnquoteSplice :: Code -> Bool
isUnquoteSplice (LForm form) =
  case form of
    L _ (List ((LForm (L _ (Atom (ASymbol "unquote-splice"))):_)))
      -> True
    _ -> False

quasiquote :: Code -> Code
quasiquote orig@(LForm (L l form)) =
  case form of
    List [LForm (L _ (Atom (ASymbol "unquote"))), x] ->
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
           LForm (L ls (List (_:body))):post' ->
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

-- This function uses `unsafeCoerce'.
compileMT :: HExpr -> Skc Macro
compileMT expr = do
  fhv <- compileParsedExprRemote expr
  hv <- liftIO (withForeignRef fhv localRef)
  return (unsafeCoerce hv)
{-# INLINE compileMT #-}

putMacro :: Code -> Skc ()
putMacro form =
  case unLForm form of
    L l (List [LForm (L _ (Atom (ASymbol name))),arg,body]) -> do
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
    _ -> skSrcError form ("malformed macro: " ++ show (pForm form))

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
    IfaceTupleTy {} -> prn "tuple ty"

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
-- eval-when-compile
--
-- ---------------------------------------------------------------------

compileDecls :: [HDecl] -> Skc ([TyThing], InteractiveContext)
compileDecls decls = do
  -- Mostly doing similar works done in `HscMain.hscDeclsWithLocation',
  -- but this function is wrapped with 'Skc' instead of 'Hsc'. Also,
  -- 'hscDeclsWithlocation' is not exported, and takes 'String' of
  -- declarations codes as argument.
  hsc_env <- getSession
  tc_gblenv <- ioMsgMaybe (tcRnDeclsi hsc_env decls)
  let defaults = tcg_default tc_gblenv
      interactive_loc =
         ModLocation { ml_hs_file = Nothing
                     , ml_hi_file = error "ewc:ml_hi_file"
                     , ml_obj_file = error "ewc:ml_obj_file"}
  ds_result <- skcDesugar' interactive_loc tc_gblenv
  simpl_mg <- liftIO (hscSimplify hsc_env ds_result)
  (tidy_cg, mod_details) <- liftIO (tidyProgram hsc_env simpl_mg)
  let !CgGuts { cg_module = this_mod
              , cg_binds = core_binds
              , cg_tycons = tycons
              , cg_modBreaks = mod_breaks } = tidy_cg
      !ModDetails { md_insts = cls_insts
                  , md_fam_insts = fam_insts } = mod_details
      data_tycons = filter isDataTyCon tycons
  prepd_binds <-
    liftIO (corePrepPgm hsc_env this_mod interactive_loc core_binds
                        data_tycons)
  cbc <- liftIO (byteCodeGen hsc_env this_mod prepd_binds
                             data_tycons mod_breaks)
  let src_span = srcLocSpan compileDeclsSrcLoc
  liftIO (linkDecls hsc_env src_span cbc)

  -- Not done in ghc-8.0.2.
  -- liftIO (hscAddSptEntries hsc_env (cg_spt_entries tidy_cg))

  let tcs = filterOut isImplicitTyCon (mg_tcs simpl_mg)
      patsyns = mg_patsyns simpl_mg
      ext_ids = [ id' | id' <- bindersOfBinds core_binds
                      , isExternalName (idName id')
                      , not (isDFunId id' || isImplicitId id')]
      new_tythings = map AnId ext_ids ++ map ATyCon tcs ++
                     map (AConLike . PatSynCon) patsyns
      ictxt = hsc_IC hsc_env
      fix_env = tcg_fix_env tc_gblenv
      new_ictxt = extendInteractiveContext ictxt new_tythings
                                           cls_insts fam_insts
                                           defaults fix_env
  setSession (hsc_env {hsc_IC = new_ictxt})
  return (new_tythings, new_ictxt)

-- GHC head exports this from HscMain, but 8.0.2 doesn't.
ioMsgMaybe :: IO (Messages, Maybe a) -> Skc a
ioMsgMaybe ioA = do
  -- XXX: Show warning messages with DynFlags settings.
  ((_warns, errs), mb_r) <- liftIO ioA
  debugIO (putStrLn "ioMsgMaybe")
  case mb_r of
    Nothing -> liftIO (throwIO (mkSrcErr errs))
    Just r  -> return r

-- Like 'HscMain.hscDesugar'', but for 'SKC'.
skcDesugar' :: ModLocation -> TcGblEnv -> Skc ModGuts
skcDesugar' mod_location tc_result = do
  hsc_env <- getSession
  r <- ioMsgMaybe (deSugar hsc_env mod_location tc_result)

  -- In `Hsc', `handleWarning' is called at this point. But currently
  -- Skc does not keep tracks of warning messages, so does nothing ...
  --
  -- handleWarnings

  return r

compileDeclsSrcLoc :: SrcLoc
compileDeclsSrcLoc = UnhelpfulLoc (fsLit "<SK.Macro.compileDecls>")


-- ---------------------------------------------------------------------
--
-- Special forms
--
-- ---------------------------------------------------------------------

m_quote :: Macro
m_quote form =
  case unLForm form of
    L l (List [_,body]) ->
      let LForm (L _ body') = quote body
      in  return (LForm (L l body'))
    _ -> skSrcError form ("malformed quote at " ++ showLoc form)

m_quasiquote :: Macro
m_quasiquote form =
    case unLForm form of
      L l (List [_,body]) ->
        let LForm (L _ body') = quasiquote body
        in  return (LForm (L l body'))
      _ -> skSrcError form ("malformed quasiquote at " ++ showLoc form)

m_defineMacro :: Macro
m_defineMacro form =
  case unLForm form of
    L l (List [_,self@(LForm (L _ (Atom (ASymbol name)))),arg,body]) -> do
      body' <- macroexpand body
      let expr = tList l [tSym l "\\", arg, body']
          expr' = tList l [ tSym l "::" , expr, tSym l "Macro"]
      case evalBuilder parseExpr [expr'] of
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
  case unLForm form of
    L l1 (List (_:LForm (L l2 (List forms)):rest)) -> do
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
  -- XXX: Need to clean up the context after compiling each file, but
  -- not yet done. Reason to clean up the context is, if not cleaned,
  -- all files passed via "--make" command will use the same interactive
  -- context, which could be confusing, and may cause unwanted name
  -- conflicts and overridings.
  --
  case unLForm form of
    L _l1 (List [_,LForm (L _l2 (Atom (ASymbol mname)))]) -> do
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

m_evalWhenCompile :: Macro
m_evalWhenCompile form =
  case unLForm form of
    L l (List (_ : body)) -> do
      expanded <- macroexpands body
      case evalBuilder parseModule expanded of
        Right (HsModule {hsmodDecls = decls}) -> do
          _ <- compileDecls decls
          return emptyForm
        Left err -> skSrcError (LForm (L l (List body))) err
    _ -> skSrcError form ("eval-when-compile: malformed body: " ++
                          show form)

specialForms :: [(String, Macro)]
specialForms =
  [("quote", m_quote)
  ,("quasiquote", m_quasiquote)
  ,("define-macro", m_defineMacro)
  ,("let-macro", m_letMacro)
  ,("require", m_require)
  ,("eval-when-compile", m_evalWhenCompile)]


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
    f acc orig@(LForm (L _ form)) =
      case form of
        List (LForm (L _ (Atom (ASymbol "begin"))) : rest) -> do
          rest' <- macroexpands rest
          return (reverse rest' ++ acc)
        _ -> return (orig : acc)

-- This function recursively expand the result. Without recursively
-- calling macroexpand on the result, cannot expand macro-generating
-- macros.
macroexpand :: Code -> Skc Code
macroexpand form =
  -- liftIO (putStrLn ("expanding:\n" ++ show (pprForm (unLocForm form))))
  case unLForm form of
    -- Expand list of forms with preserving the constructor.
    L l (List forms) -> expandList l List forms
    L l (HsList forms) -> expandList l HsList forms

    -- Rest of the form are untouched.
    L _ _ -> return form
  where
    expandList l constr forms =
      case forms of
        sym@(LForm (L _ (Atom (ASymbol k)))) : rest -> do
          macros <- getMacroEnv
          case lookup k macros of
           Just f -> f form >>= macroexpand
           Nothing -> do
             rest' <- mapM macroexpand rest
             return (LForm (L l (constr (sym:rest'))))
        _ -> do
          forms' <- mapM macroexpand forms
          return (LForm (L l (constr forms')))


-- ---------------------------------------------------------------------
--
-- Auxiliary
--
-- ---------------------------------------------------------------------

tSym :: SrcSpan -> String -> Code
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
