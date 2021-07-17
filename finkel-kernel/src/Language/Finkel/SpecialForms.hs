{-# LANGUAGE CPP               #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
-- | Special forms.
module Language.Finkel.SpecialForms
  ( specialForms
  , unquoteSplice
  , defaultFnkEnv
  ) where

#include "ghc_modules.h"

-- base
import Control.Exception               (throw)
import Control.Monad                   (foldM, unless, when)
import Control.Monad.IO.Class          (MonadIO (..))
import Data.Maybe                      (catMaybes)
import GHC.Exts                        (unsafeCoerce#)

#if MIN_VERSION_ghc(9,2,0)
import GHC.Utils.Outputable            ((<>))
import Prelude                         hiding ((<>))
#endif

-- containers
import Data.Map                        (fromList)

-- exceptions
import Control.Monad.Catch             (bracket)

-- ghc
import GHC                             (ModuleInfo, getModuleInfo, lookupModule,
                                        lookupName, modInfoExports, setContext)
import GHC_Data_FastString             (FastString, fsLit, unpackFS)
import GHC_Driver_Env_Types            (HscEnv (..))
import GHC_Driver_Main                 (Messager, hscTcRnLookupRdrName,
                                        showModuleIndex)
import GHC_Driver_Monad                (GhcMonad (..), modifySession)
import GHC_Driver_Ppr                  (showPpr)
import GHC_Driver_Session              (DynFlags (..), GeneralFlag (..),
                                        HasDynFlags (..), getDynFlags,
                                        unSetGeneralFlag')
import GHC_Hs                          (HsModule (..))
import GHC_Hs_ImpExp                   (ImportDecl (..), ieName)
import GHC_Iface_Recomp                (RecompileRequired (..),
                                        recompileRequired)
import GHC_Runtime_Context             (InteractiveImport (..))
import GHC_Runtime_Eval                (getContext)
import GHC_Types_Name                  (nameOccName, occName)
import GHC_Types_Name_Occurrence       (occNameFS)
import GHC_Types_Name_Reader           (rdrNameOcc)
import GHC_Types_SourceText            (SourceText (..))
import GHC_Types_SrcLoc                (GenLocated (..), SrcSpan (..), unLoc)
import GHC_Types_TyThing               (TyThing (..))
import GHC_Types_Var                   (varName)
import GHC_Unit_Finder                 (FindResult (..), findImportedModule)
import GHC_Unit_Home_ModInfo           (lookupHpt)
import GHC_Unit_Module                 (Module, moduleNameString)
import GHC_Unit_Module_Graph           (ModuleGraph, showModMsg)
import GHC_Unit_Module_ModSummary      (ModSummary (..))
import GHC_Utils_Error                 (compilationProgressMsg)
import GHC_Utils_Outputable            (SDoc, hcat, ppr)

#if MIN_VERSION_ghc(9,2,0)
import GHC_Utils_Outputable            (text)
#endif

#if MIN_VERSION_ghc(9,0,0)
import GHC_Types_SrcLoc                (UnhelpfulSpanReason (..))
#endif

#if MIN_VERSION_ghc(8,4,0)
import GHC_Unit_Module_Graph           (mgLookupModule)
#endif

-- Internal
import Language.Finkel.Builder
import Language.Finkel.Eval
import Language.Finkel.Exception
import Language.Finkel.Expand          (bcoDynFlags, expand, expands')
import Language.Finkel.Fnk
import Language.Finkel.Form
import Language.Finkel.Homoiconic
import Language.Finkel.Make            (findTargetModuleNameMaybe,
                                        makeFromRequire)
import Language.Finkel.Syntax          (parseExpr, parseLImport, parseModuleNoHeader)
import Language.Finkel.Syntax.SynUtils

-- ---------------------------------------------------------------------
--
-- Quasiquote
--
-- ---------------------------------------------------------------------

-- Quasiquote is implemented as special form in Haskell. Though it could be
-- implemented in Finkel code later. If done in Finkel code, lexer and reader
-- still need to handle the special case for backtick, comma, and comma-at,
-- because currently there's no way to define read macro.

quasiquote :: Bool -> Code -> Code
quasiquote qual orig@(LForm (L l form)) =
  case form of
    List [LForm (L _ (Atom (ASymbol ":unquote"))), x]
      | isUnquoteSplice x -> x
      | otherwise         -> tList l [tSym l (toCodeS qual), x]
    List forms'
      | [q, body] <- forms'
      , q == tSym l ":quasiquote"  -> qq (qq body)
      | any isUnquoteSplice forms' -> spliced qListS forms'
      | otherwise                  -> nonSpliced qListS forms'
    HsList forms'
      | any isUnquoteSplice forms' -> spliced qHsListS forms'
      | otherwise                  -> nonSpliced qHsListS forms'
    Atom _                         -> tList l [tSym l ":quote", orig]
    TEnd                           -> orig
  where
   spliced tag forms =
     tList l [ tSym l (tag qual)
             , tList l [ tSym l (concatS qual)
                       , tHsList l (go [] forms) ]
             , fname, sl, sc, el, ec ]
   nonSpliced tag forms =
     tList l [ tSym l (tag qual)
             , tHsList l (map qq forms)
             , fname, sl, sc, el, ec ]
   (fname, sl, sc, el, ec) = withLocInfo l (tString qq_l) (tInt qq_l)
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
#if MIN_VERSION_ghc(9,0,0)
   qq_l = UnhelpfulSpan (UnhelpfulOther (fsLit "<quasiquote>"))
#else
   qq_l = UnhelpfulSpan (fsLit "<quasiquote>")
#endif

isUnquoteSplice :: Code -> Bool
isUnquoteSplice (LForm form) =
  case form of
    L _ (List (LForm (L _ (Atom (ASymbol ":unquote-splice"))):_))
      -> True
    _ -> False
{-# INLINABLE isUnquoteSplice #-}

-- | Internally used by macro expander for @:unquote-splice@ special form.
--
-- This functions throw 'InvalidUnquoteSplice' when the given argument could not
-- be unquote spliced.
unquoteSplice :: Homoiconic a => a -> [Code]
unquoteSplice form =
  case unCode c of
    List xs             -> xs
    HsList xs           -> xs
    Atom AUnit          -> []
    Atom (AString _ xs) -> map toCode (unpackFS xs)
    _                   -> throw (InvalidUnquoteSplice c)
  where
    c = toCode form


-- ---------------------------------------------------------------------
--
-- Macro
--
-- ---------------------------------------------------------------------

coerceMacro :: DynFlags -> Code -> Fnk Macro
coerceMacro dflags name =
  case unCode name of
    Atom (ASymbol _) -> go
    _                -> failFnk "coerceMacro: expecting name symbol"
  where
    go = do
      qualify <- envQualifyQuotePrimitives <$> getFnkEnv
      case evalBuilder dflags qualify parseExpr [name] of
        Right hexpr -> unsafeCoerce# <$> evalExpr hexpr
        Left err    -> failFnk (syntaxErrMsg err)
{-# INLINABLE coerceMacro #-}

getTyThingsFromIDecl :: GhcMonad m => HImportDecl -> ModuleInfo -> m [TyThing]
getTyThingsFromIDecl (L _ idecl) minfo = do
  -- 'toImportList' borrowed from local definition in
  -- 'TcRnDriver.tcPreludeClashWarn'.
  let exportedNames = modInfoExports minfo
      ieName' (dL->L l ie) = la2la (cL l (ieName ie))
      toImportList (h, dL->L _ loc) = (h, map ieName' loc)
      getNames =
        case fmap toImportList (ideclHiding idecl) of
          -- Import with `hiding' entities. Comparing 'Name' and 'RdrName' via
          -- OccName'.
          Just (True, ns)  -> do
            let f n acc = if nameOccName n `elem` ns'
                             then acc
                             else n : acc
                ns' = map (rdrNameOcc . unLoc) ns
            return (foldr f [] exportedNames)

          -- Import with explicit entities.
          Just (False, ns) -> do
            hsc_env <- getSession
            concat <$> mapM (liftIO . hscTcRnLookupRdrName hsc_env) ns

          -- Import whole module.
          Nothing          -> return exportedNames

  catMaybes <$> (getNames >>= mapM lookupName)

addImportedMacro :: FnkEnv -> HscEnv -> TyThing -> Fnk ()
addImportedMacro fnk_env hsc_env thing = when (isMacro hsc_env thing) go
  where
    go =
      case thing of
        AnId var -> do
          debug fnk_env
                "addImportedMacro"
                [hcat [ "Adding macro `"
                      , ppr (varName var)
                      , "' to current compiler session" ]]
          let name_str = showPpr dflags (varName var)
              name_sym = toCode (aSymbol name_str)
          coerceMacro dflags name_sym >>= insertMacro (fsLit name_str)
        _ -> failFnk "addImportedmacro"
    dflags = hsc_dflags hsc_env

-- Note [Bytecode and object code for require and :eval_when_compile import]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Use of object codes are not working well for importing home package modules
-- when optimization option were enabled.  Conservatively using bytecode by
-- delegating further works to 'makeFromRequire' via 'withRequiredSettings' for
-- such cases.

setRequiredSettings :: Fnk ()
setRequiredSettings = do
  -- 'DynFlags' in the current session might be updated by the file local
  -- pragmas.  Using the 'DynFlags' from 'envDefaultDynFlags', which is
  -- initialized when entering the 'make' function in 'initSessionForMake'.
  fnk_env <- getFnkEnv
  let no_force_recomp = unSetGeneralFlag' Opt_ForceRecomp
      update = setDynFlags . no_force_recomp . bcoDynFlags
  mapM_ update (envDefaultDynFlags fnk_env)
  putFnkEnv fnk_env {envMessager = requiredMessager}

withRequiredSettings :: Fnk a -> Fnk a
withRequiredSettings act =
  bracket
    (do dflags <- getDynFlags
        fnk_env <- getFnkEnv
        return (dflags, fnk_env))
    (\(dflags, fnk_env) ->
       do setDynFlags dflags
          putFnkEnv fnk_env)
    (const (setRequiredSettings >> act))

requiredMessager :: Messager
requiredMessager hsc_env mod_index recomp node =
  -- See: GHC.Driver.Main.batchMsg
  case recomp of
    MustCompile       -> showMsg "Compiling " " [required]"
    UpToDate          -> when (verbosity dflags >= 2) (showMsg "Skipping " "")
    RecompBecause why ->
#if MIN_VERSION_ghc(9,2,0)
      showMsg "Compiling " (" [required, " <> text why <> "]")
#else
      showMsg "Compiling " (" [required, " ++ why ++ "]")
#endif
  where
    dflags = hsc_dflags hsc_env
    showMsg msg reason =
#if MIN_VERSION_ghc(9,2,0)
      compilationProgressMsg (hsc_logger hsc_env) dflags
        (showModuleIndex mod_index <> msg <>
         showModMsg dflags (recompileRequired recomp) node <>
         reason)
#else
      compilationProgressMsg dflags
        (showModuleIndex mod_index ++ msg ++
         showModMsg dflags (hscTarget dflags)
                    (recompileRequired recomp)
                    node ++
         reason)
#endif

makeMissingHomeMod :: HImportDecl -> Fnk ()
makeMissingHomeMod (L _ idecl) = do
  -- Try finding the required module. Delegate the work to 'makeFromRequire'
  -- function when the file is found in import paths.
  -- Look up module with "findTargetModuleNameMaybe" before "findImportedModule"
  -- is to avoid loading modules from own package when generating documentation
  -- with haddock. Always checking up-to-date ness via "makeFromRequire".
  --
  -- N.B. 'findImportedModule' does not know ".fnk" file extension, so it will
  -- not return Finkel source files for home package modules.
  --
  hsc_env <- getSession

  let mname = unLoc lmname
      lmname = ideclName idecl
      smpl_mk = withRequiredSettings (makeFromRequire lmname)
      dflags = hsc_dflags hsc_env

  case lookupHpt (hsc_HPT hsc_env) mname of
    Just _ -> smpl_mk
    _ -> do
      mb_ts <- findTargetModuleNameMaybe dflags lmname
      case mb_ts of
        Just _ -> smpl_mk
        Nothing -> do
          fresult <- liftIO (findImportedModule hsc_env mname Nothing)
          case fresult of
            Found {} -> return ()
            _        -> smpl_mk

-- ---------------------------------------------------------------------
--
-- Special forms
--
-- ---------------------------------------------------------------------

m_quasiquote :: MacroFunction
m_quasiquote form =
    case unLForm form of
      L l (List [_,body]) -> do
        qualify <- fmap envQualifyQuotePrimitives getFnkEnv
        let LForm (L _ body') = quasiquote qualify body
        return (LForm (L l body'))
      _ -> finkelSrcError form "malformed quasiquote"

m_withMacro :: MacroFunction
m_withMacro form =
  case unLForm form of
    L l1 (List (_:LForm (L _ (List forms)):rest)) -> do
      fnkc_env0 <- getFnkEnv
      hsc_env <- getSession

      -- Expand body of `with-macro' with temporary macros.
      macros <- fromList <$> evalMacroDefs hsc_env forms
      let tmpMacros0 = envTmpMacros fnkc_env0
      putFnkEnv (fnkc_env0 {envTmpMacros = macros : tmpMacros0})
      expanded <- expands' rest

      -- Getting 'FnkEnv' again, so that the persistent macros defined inside
      -- the `with-macro' body could be used from here. Then restoring tmporary
      -- macros to preserved value.
      fnkc_env1 <- getFnkEnv
      putFnkEnv (fnkc_env1 {envTmpMacros = tmpMacros0})

      case expanded of
        [x] -> return x
        _   -> return (tList l1 (tSym l1 ":begin" : expanded))
    _ -> finkelSrcError form ("with-macro: malformed args:\n" ++ show form)
  where
    evalMacroDefs hsc_env forms = do
      forms' <- mapM expand forms
      qualify <- envQualifyQuotePrimitives <$> getFnkEnv
      case evalBuilder (hsc_dflags hsc_env) qualify parseModuleNoHeader forms' of
        Right HsModule {hsmodDecls=decls} -> do
          (tythings, ic) <- evalDecls decls
          modifySession (\he -> he {hsc_IC=ic})
          foldM (asMacro hsc_env) [] tythings
        Left err -> finkelSrcError form (syntaxErrMsg err)
    asMacro hsc_env acc tything =
      case tything of
        AnId var | isMacro hsc_env tything ->
          do let name_fs = occNameFS (occName (varName var))
                 name_sym = toCode (ASymbol name_fs)
             macro <- coerceMacro (hsc_dflags hsc_env) name_sym
             return ((MacroName name_fs, macro):acc)
        _ -> return acc

m_require :: MacroFunction
m_require form =
  -- The special form `require' modifies the HscEnv at the time of macro
  -- expansion, to update the context in compile time session.  The `require' is
  -- implemented as special form, to support dependency analysis during
  -- compilation of multiple modules with `--make' command.
  --
  -- Note that the form body of `require' is parsed twice, once in Reader, and
  -- again in this module. Parsing twice because the first parse is done before
  -- expanding macro, to analyse the module dependency graph of home package
  -- module.
  --
  case form of
    LForm (L _ (List (_:code))) ->
      do dflags <- getDynFlags
         qualify <- envQualifyQuotePrimitives <$> getFnkEnv
         case evalBuilder dflags qualify parseLImport code of
           Right lidecl@(L _ idecl) -> do
             fnk_env <- getFnkEnv
             let tr = debug fnk_env "m_require"
                 mname = unLoc (ideclName idecl)
             tr [ppr idecl]

             -- Handle home modules.
             makeMissingHomeMod lidecl
             context <- getContext
             setContext (IIDecl idecl : context)
             mgraph <- hsc_mod_graph <$> getSession
             mdl <- lookupModule mname Nothing

             -- Update required module names and compiled home modules in
             -- FnkEnv. These are used by the callee module (i.e. the module
             -- containing this 'require' form).
             let reqs0 = envRequiredHomeModules fnk_env
                 reqs1 = case mgLookupModule' mgraph mdl of
                           Just m -> m:reqs0
                           _      -> reqs0
             modifyFnkEnv (\e -> e {envRequiredHomeModules = reqs1})

             -- Look up Macros in parsed module, add to FnkEnv when found.
             mb_minfo <- getModuleInfo mdl
             case mb_minfo of
               Just minfo -> do
                 things <- getTyThingsFromIDecl lidecl minfo
                 hsc_env <- getSession
                 mapM_ (addImportedMacro fnk_env hsc_env) things
                 return emptyForm
               Nothing ->
                 finkelSrcError form
                                ("require: module " ++
                                 moduleNameString mname ++ " not found.")
           Left err -> finkelSrcError form ("require: " ++ syntaxErrMsg err)
    _ -> finkelSrcError form "require: malformed body"

m_evalWhenCompile :: MacroFunction
m_evalWhenCompile form =
  case unLForm form of
    L l (List (_ : body)) -> do
      expanded <- expands' body
      dflags <- getDynFlags
      qualify <- envQualifyQuotePrimitives <$> getFnkEnv
      case evalBuilder dflags qualify parseModuleNoHeader expanded of
        Right HsModule { hsmodDecls = decls
                       , hsmodImports = limps } -> do

          -- If module imports were given, add to current interactive context.
          -- Compile home modules if not found.
          unless (null limps) $ do
             mapM_ makeMissingHomeMod limps
             context <- getContext
             setContext (map (IIDecl . unLoc) limps ++ context)

          -- Then evaluate the declarations and set the interactive context with
          -- the update `tythings'. If the compiled decls contain macros, add
          -- to current Finkel environment.
          unless (null decls) $ do
            (tythings, ic) <- evalDecls decls
            modifySession (\hsc_env -> hsc_env {hsc_IC=ic})
            fnk_env <- getFnkEnv
            hsc_env <- getSession
            mapM_ (addImportedMacro fnk_env hsc_env) tythings

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
    ,(":quasiquote", SpecialForm m_quasiquote)
    ,(":require", SpecialForm m_require)]

-- | Default 'FnkEnv'.
defaultFnkEnv :: FnkEnv
defaultFnkEnv = emptyFnkEnv
  { envMacros         = specialForms
  , envDefaultMacros  = specialForms
  }


-- ---------------------------------------------------------------------
--
-- Auxiliary
--
-- ---------------------------------------------------------------------

tSym :: SrcSpan -> FastString -> Code
tSym l s = LForm (L l (Atom (ASymbol s)))
{-# INLINABLE tSym #-}

tString :: SrcSpan -> FastString -> Code
tString l s = LForm (L l (Atom (AString (SourceText (show s)) s)))
{-# INLINABLE tString #-}

tInt :: SrcSpan -> Int -> Code
tInt l i = LForm (L l (Atom (AInteger (mkIntegralLit i))))
{-# INLINABLE tInt #-}

tList :: SrcSpan -> [Code] -> Code
tList l forms = LForm (L l (List forms))
{-# INLINABLE tList #-}

tHsList :: SrcSpan -> [Code] -> Code
tHsList l forms = LForm (L l (HsList forms))
{-# INLINABLE tHsList #-}

emptyForm :: Code
emptyForm =
  LForm (genSrc (List [LForm (genSrc (Atom (ASymbol ":begin")))]))
{-# INLINABLE emptyForm #-}

toCodeS :: Quote
toCodeS = quoteWith "toCode"
{-# INLINABLE toCodeS #-}

unquoteSpliceS :: Quote
unquoteSpliceS = quoteWith "unquoteSplice"
{-# INLINABLE unquoteSpliceS #-}

concatS :: Quote
concatS qual =
  if qual
     then "Data.Foldable.concat"
     else "concat"
{-# INLINABLE concatS #-}

-- | Debug function for this module
debug :: (MonadIO m, HasDynFlags m) => FnkEnv -> SDoc -> [SDoc] -> m ()
debug fnk_env fn msgs =
  debugWhen fnk_env
            Fnk_trace_spf
            (hcat [";;; [Language.Finkel.SpecialForms.", fn, "]:"] : msgs)

mgLookupModule' :: ModuleGraph -> Module -> Maybe ModSummary
#if MIN_VERSION_ghc (8,4,0)
mgLookupModule' = mgLookupModule
#else
mgLookupModule' mg mdl = go mg
  where
    go []       = Nothing
    go (ms:mss) = if ms_mod ms == mdl then Just ms else go mss
#endif
