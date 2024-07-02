;;; -*- mode: finkel -*-
;;;; Macros used in REPL.

;;; This module contains macros accessible only from REPL. Main purpose
;;; of using macros for REPL is to access runtime value of
;;; `FnkEnv'. Macro body can contain codes accessing `FnkEnv', and then
;;; the code could be invoked from REPL via evaluating the typed in
;;; forms.

(defmodule Finkel.Tool.Internal.Macro.Repl
  (export repl-macro compile-and-import)
  (require
   ;; Internal
   (Finkel.Tool.Internal.Macro.Ghc))
  (import-when [:compile]
    ;; finkel-core
    (Finkel.Prelude))
  (import
   ;; base
   (Prelude hiding [<>])
   (Control.Exception [(Exception ..) (SomeException ..) try])
   (Control.Monad [filterM unless void when])
   (Control.Monad.IO.Class [(MonadIO ..)])
   (Data.Foldable [toList])
   (Data.Function [on])
   (Data.List [find intercalate intersperse isPrefixOf partition sortBy])
   (Data.Maybe [catMaybes])
   (System.Console.GetOpt [(ArgOrder ..) getOpt])
   (Text.Printf [printf])
   (Text.Read [readMaybe])

   ;; directory
   (System.Directory
    [getCurrentDirectory getHomeDirectory setCurrentDirectory])

   ;; filepath
   (System.FilePath [normalise])

   ;; exceptions
   (Control.Monad.Catch [catch bracket])

   ;; process
   (System.Process [callProcess])

   ;; finkel-kernel
   (Language.Finkel)
   (Language.Finkel.Expand [discardInteractiveContext])
   (Language.Finkel.Eval [evalExprType evalTypeKind])
   (Language.Finkel.Form [mkLocatedForm])
   (Language.Finkel.Make [buildHsSyn simpleMake])
   (Language.Finkel.Fnk [(FnkEnv ..) getFnkEnv macroNames modifyFnkEnv putFnkEnv
                         setDynFlags setFnkVerbosity updateDynFlags])
   (Language.Finkel.Options [fnkEnvOptions partitionFnkEnvOptions])
   (Language.Finkel.Syntax [parseExpr parseType])
   (Language.Finkel.Make [asModuleName])

   ;; finkel-core
   (Finkel.Core.Functions)

   ;; Internal
   (Finkel.Tool.Internal.Compat)))

;;; ghc

(imports-from-ghc
 (GHC
  [(ModuleInfo) findModule getBindings getModSummary getModuleGraph
   getModuleInfo getTargets isLoaded lookupName lookupModule modInfoExports
   setSessionDynFlags setTargets workingDirectoryChanged])

 (GHC.Core.FamInstEnv [FamInst pprFamInst])
 (GHC.Core.InstEnv [ClsInst pprInstance])

 (GHC.Data.FastString [FastString fsLit unpackFS])

 (GHC.Driver.Env [(HscEnv ..)])
 (GHC.Driver.Make [(LoadHowMuch ..)])
 (GHC.Driver.Monad [(GhcMonad ..) getSessionDynFlags withTempSession])
 (GHC.Driver.Ppr [showSDoc showSDocForUser showPpr])
 (GHC.Driver.Session
  [(DynFlags ..) (GhcMode ..) (HasDynFlags ..) (Language ..)
   (PackageFlag ..) (GeneralFlag ..)  defaultDynFlags
   fFlags flagSpecFlag flagSpecName gopt lang_set
   parseDynamicFlagsCmdLine settings xFlags xopt wopt wWarningFlags])

 (GHC.Iface.Syntax [showToHeader])

 (GHC.Runtime.Context [(InteractiveImport ..)])
 (GHC.Runtime.Debugger [pprTypeAndContents])
 (GHC.Runtime.Eval
  [abandonAll getContext getInfo moduleIsInterpreted parseName setContext
   showModule])

 (GHC.Types.Basic [(SuccessFlag ..)])
 (GHC.Types.Fixity [Fixity defaultFixity])
 (GHC.Types.Name
  [Name getName nameModule nameOccName nameSrcSpan pprInfixName])
 (GHC.Types.Name.Set [elemNameSet mkNameSet])
 (GHC.Types.SourceError [srcErrorMessages])
 (GHC.Types.SrcLoc [getLoc isGoodSrcSpan mkGeneralLocated unLoc])
 (GHC.Types.Target [(Target ..) (TargetId ..) pprTarget])
 (GHC.Types.TyThing [(TyThing ..) tyThingParent_maybe])
 (GHC.Types.TyThing.Ppr [pprTyThing pprTyThingInContextLoc])

 (GHC.Unit.Finder (flushFinderCaches uncacheModule))
 (GHC.Unit.Home.ModInfo [pprHPT])
 (GHC.Unit.Module [ModuleName mkModuleName mkModuleNameFS moduleNameString])
 (GHC.Unit.Module.Graph [mgModSummaries])
 (GHC.Unit.Module.ModSummary [(ModSummary ..) ms-mod-name])

 (GHC.Utils.Misc [looksLikeModuleName])
 (GHC.Utils.Outputable
  [SDoc $$ <+> <> empty dcolon hsep nest ppr sep text vcat]))


;;; Extra imports

(import GHC.Hs.ImpExp ((ImportDecl ..) simpleImportDecl))

(cond-expand
  [(<= 904 :ghc)
   (:begin
     (import qualified GHC)
     (import GHC.Core.TyCo.Ppr (pprSigmaType))
     (import GHC.Driver.Env (hsc-HPT hsc-home-unit hscActiveUnitId))
     (import GHC.Driver.Make ((ModIfaceCache ..))))]
  [otherwise
   (imports-from-ghc
    (GHC (Type))
    (GHC.Types.SourceText [(StringLiteral ..)])
    (GHC.Types.TyThing.Ppr [pprTypeForUser]))])

(cond-expand
  [(<= 906 :ghc)
   (import GHC.Driver.Backend (backendCanReuseLoadedCode))]
  [(<= 902 :ghc)
   (import GHC.Driver.Backend (backendProducesObject))]
  [otherwise
   (imports-from-ghc
    (GHC.Driver.Session [isObjectTarget]))])

(cond-expand
  [(<= 902 :ghc)
   (:begin
     (import GHC.Driver.Env (hsc-units))
     (import GHC.Linker.Loader (initLoaderState showLoaderState)))]
  [otherwise
   (imports-from-ghc
    (GHC.Runtime.Linker [initDynLinker showLinkerState]))])

(cond-expand
  [(<= 902 :ghc)
   (import GHC.Runtime.Interpreter ((Message ..) interpCmd))]
  [otherwise
   (imports-from-ghc
    (GHC.Runtime.Interpreter [(Message ..) iservCmd]))])

(cond-expand
  [(<= 900 :ghc)
   (:begin
     (import GHC.Types.SrcLoc (leftmost-smallest))
     (import GHC.Unit.Module ((Module) moduleName))
     (import qualified GHC.Driver.Make as GhcMake))]
  [otherwise
   (:begin
     (imports-from-ghc
      (GHC.Unit.Module [(Module ..)]))
     (import qualified GhcMake))])


;;; Types

(type ReplAction (-> [Code] (Fnk Code)))

(data ReplCmd
  (ReplCmd {(:: rc-name String)
            (:: rc-args [String])
            (:: rc-action ReplAction)
            (:: rc-help String)}))


;;; Auxiliary functions

(cond-expand
  [(<= 904 :ghc)
   (:begin)]
  [otherwise
   (defn (:: pprSigmaType (-> Type SDoc)) pprTypeForUser)])

(defn (:: showUnitId (-> DynFlags String))
  [dflags]
  (cond-expand
    [(<= 902 :ghc)
     (showPpr dflags (homeUnitId_ dflags))]
    [(<= 900 :ghc)
     (showPpr dflags (homeUnitId dflags))]
    [otherwise
     (showPpr dflags (thisInstalledUnitId dflags))]))

(defn (:: gen-default-dflags (-> DynFlags DynFlags))
  [flg]
  (cond-expand
    [(<= 906 :ghc)
     (defaultDynFlags (settings flg))]
    [otherwise
     (defaultDynFlags (settings flg) (llvmConfig flg))]))

(defn (:: show-linker-state (-> HscEnv (IO ())))
  [hsc-env]
  (cond-expand
    [(<= 902 :ghc)
     (case (hsc-interp hsc-env)
       (Just interp) (do (<- sdoc (showLoaderState interp))
                         (putStrLn (showPpr (hsc-dflags hsc-env) sdoc)))
       _ (pure ()))]
    [(<= 900 :ghc)
     (do (<- sdoc (showLinkerState (hsc-dynLinker hsc-env)))
         (putStrLn (showPpr (hsc-dflags hsc-env) sdoc)))]
    [otherwise
     (showLinkerState (hsc-dynLinker hsc-env) (hsc-dflags hsc-env))]))

(defn (:: rts-revert-cafs (Fnk ()))
  (cond-expand
    [(<= 902 :ghc)
     (case-do (fmap hsc-interp getSession)
       (Just interp) (liftIO (interpCmd interp RtsRevertCAFs))
       _ (pure ()))]
    [otherwise
     (do (<- hsc-env getSession)
         (liftIO (iservCmd hsc-env RtsRevertCAFs)))]))

(defn (:: is-interpreting (-> DynFlags Bool))
  (cond-expand
    [(<= 906 :ghc)
     (. backendCanReuseLoadedCode backend)]
    [(<= 902 :ghc)
     (. not backendProducesObject backend)]
    [otherwise
     (. not isObjectTarget hscTarget)]))

(defn (:: mk-ii (-> ModuleName InteractiveImport))
  (. IIDecl simpleImportDecl))

(defn (:: mk-ii-fs (-> FastString InteractiveImport))
  (. mk-ii mkModuleNameFS))

(defn (:: mk-ii-str (-> String InteractiveImport))
  (. mk-ii-fs fsLit))

(defn (:: code-to-mb-string (-> Code (Maybe String)))
  (. (fmap unpackFS) code-to-mb-fs))

(defn (:: code-to-mb-fs (-> Code (Maybe FastString)))
  [code]
  (case (unCode code)
    (Atom (ASymbol s)) (Just s)
    (Atom (AString _ s)) (Just s)
    _ Nothing))

(defn (:: located-list (-> [Code] Code))
  [xs]
  (case xs
    [] nil
    _  (LForm (L (getLoc (mkLocatedForm xs)) (List xs)))))

(defn (:: show-sdoc-for-user-m (-> SDoc (Fnk String)))
  [sdoc]
  (do (<- hsc-env getSession)
      (<- unqual get-name-ppr-ctx)
      (lept [dflags (hsc-dflags hsc-env)
             str (cond-expand
                   [(<= 902 :ghc)
                    (showSDocForUser dflags (hsc-units hsc-env) unqual sdoc)]
                   [otherwise
                    (showSDocForUser dflags unqual sdoc)])])

      (pure str)))

(defn (:: invalid-form (-> String [Code] (Fnk a)))
  [label forms]
  (lept [form (car (located-list forms))
         msg (concat [label ": invalid form `" (show form) "'"])]
    (finkelSrcError form msg)))

(defn (:: onTheREPL (-> a (Located a)))
  (mkGeneralLocated "on the REPL"))

(defn (:: compile-module (-> [(Located String)] (Fnk SuccessFlag)))
  [lstrs]
  (where (bracket acquire cleanup work)
    (defn acquire
      (do (<- dflags getDynFlags)
          (<- fnk-env getFnkEnv)
          (return (, dflags fnk-env))))
    (defn cleanup [(, dflags fnk-env)]
      (do (setDynFlags dflags)
          (modifyFnkEnv (\e (e {(= envQualifyQuotePrimitives
                                  (envQualifyQuotePrimitives fnk-env))})))))
    (defn work [(, dflags fnk-env)]
      (do (putFnkEnv (fnk-env {(= envQualifyQuotePrimitives False)}))
          (lept [force-recomp (gopt Opt-ForceRecomp dflags)])
          (<- success-flag
            (simpleMake (zip lstrs (repeat Nothing)) force-recomp Nothing))

          ;; As done in `GHCi.UI', reverting CAFs on load.
          rts-revert-cafs

          (return success-flag)))))

(defn (:: compile-and-import (-> [(Located FilePath)] (Fnk SuccessFlag)))
  [lpaths]
  (do (<- fnk-env getFnkEnv)
      (lefn [(imps0 (map mk-ii-str (envContextModules fnk-env)))
             (:: adjust-module (-> (Located String) (Fnk InteractiveImport)))
             (adjust-module [lpath]
               (lept [name1 (asModuleName (unLoc lpath))
                      name2 (if (null name1) "Main" name1)
                      name3 (mkModuleNameFS (fsLit name2))]
                 (do (<- mdl (getModSummary name3))
                     (<- is-interp (moduleIsInterpreted (ms-mod mdl)))
                     (return (if is-interp
                               (IIModule name3)
                               (mk-ii name3))))))])

      ;; As done in ghci, adding `IIModule' if the module is interpreted as
      ;; bytecode, adding `IIDecl' otherwise. Safe Haskell setting in DynFlags
      ;; is ignored at the moment.
      (<- success-flag (compile-module lpaths))
      (case success-flag
        Succeeded (do (<- mdls (mapM adjust-module lpaths))
                      (setContext (++ mdls imps0)))
        Failed (setContext imps0))
      (return success-flag)))

(defn (:: adjust-current-target
        (-> FilePath [InteractiveImport] (Fnk [InteractiveImport])))
  "Adjust current IIModule target to IIDecl if current HscTarget is
object code."
  [path imports]
  (do (<- dflags getDynFlags)
      (lefn [(current-module-name
               (mkModuleName (asModuleName path)))
             (iimodule-to-iidecl [ii]
               (case ii
                 (IIModule mname) (| ((== mname current-module-name)
                                      (mk-ii mname)))
                 _ ii))
             (iidecl-to-iimodule [ii]
               (case ii
                 (IIDecl idecl) (| ((== (unLoc (ideclName idecl))
                                        current-module-name)
                                    (IIModule current-module-name)))
                 _              ii))
             (ii-fn
               (if (not (is-interpreting dflags))
                 iimodule-to-iidecl
                 iidecl-to-iimodule))])
      (return (map ii-fn imports))))

(defn (:: env-context-on-exception (-> (Fnk Code) (Fnk Code)))
  [action]
  (catch action
    (\e
      (do (lefn [(print-se [se]
                   (do (<- dflags getSessionDynFlags)
                       (liftIO
                        (putStr (unlines
                                 (map (showSDoc dflags) (msgs se)))))))
                 (msgs [se]
                   (ppr-wrapped-msg-bag-with-loc (srcErrorMessages se)))])
          (maybe (liftIO (print e)) print-se (fromException e))
          (<- mods (fmap envContextModules getFnkEnv))
          (setContext (map mk-ii-str mods))
          (return '(:begin))))))

(defn (:: sort-by-name-src-span (-> [Name] [Name]))
  (cond-expand
    [(<= 900 :ghc)
     (sortBy (on leftmost-smallest nameSrcSpan))]
    [otherwise
     (sortBy (on compare nameSrcSpan))]))

(defn (:: browse-module (-> Module ModuleInfo (Fnk Code)))
  "Simplified version of `GHCi.UI.browseModule'."
  [mdl mod-info]
  (lefn [(loc-sort [ns]
           (| ((<- (: n _) ns) (isGoodSrcSpan (nameSrcSpan n))
               (sort-by-name-src-span ns))
              (otherwise (occ-sort ns))))
         (occ-sort
           (sortBy (on compare nameOccName)))]
    (lept [names (modInfoExports mod-info)
           (, local external) (partition (. (== mdl) nameModule) names)
           sorted-names (++ (loc-sort local) (occ-sort external))
           pretty (pprTyThing showToHeader)]
      (do (<- mb-things (mapM lookupName sorted-names))
          (lept [things (catMaybes mb-things)
                 prettyThings (map pretty things)])
          (<- str (show-sdoc-for-user-m (vcat prettyThings)))
          (return `(System.IO.putStrLn ,str))))))

(defn (:: expand-with (-> String (-> Code (Fnk Code)) ReplAction))
  [label f forms]
  (case forms
    [] (return '(:begin))
    [expr] (>>= (f expr) (\x (return `(System.IO.print ',x))))
    _ (invalid-form label forms)))

;;; Mostly translated from `GHCi.UI.infoThing'.
(defn (:: info-name (-> Code (Fnk Code)))
  [thing]
  (do (<- sdoc (info-thing True (show thing)))
      (<- str (show-sdoc-for-user-m sdoc))
      (return `(System.IO.putStrLn ,str))))

(defn (:: info-thing (-> Bool String (Fnk SDoc)))
  [all-info str]
  (where (do (<- names (parseName str))
             (<- mb_stuffs (mapM (getInfo all-info) names))
             (lept [filtered
                    (filter-out-children child-filter
                                         (catMaybes (toList mb_stuffs)))])
             (return
              (vcat (intersperse (text "") (map ppr-info filtered)))))
    (defn child-filter [(, a _ _ _ _)] a)
    (defn ppr-info [(, thing fixity cls fam _)]
      (__ppr-info thing fixity cls fam))))

(defn (:: __ppr-info (-> TyThing Fixity [ClsInst] [FamInst] SDoc))
  [thing fixity cls fam]
  (lept [show-fixity (if (== fixity defaultFixity)
                       empty
                       (<+> (ppr fixity) (pprInfixName (getName thing))))]
    ($$ (pprTyThingInContextLoc thing)
        show-fixity
        (vcat (map pprInstance cls))
        (vcat (map pprFamInst fam)))))

(defn (:: filter-out-children (-> (-> a TyThing) [a] [a]))
  [get-thing xs]
  (lefn [(all-names
           (mkNameSet (map (. getName get-thing) xs)))
         (has-parent [x]
           (case (tyThingParent-maybe (get-thing x))
             (Just p) (elemNameSet (getName p) all-names)
             _ False))]
    (filter (. not has-parent) xs)))

(defn (:: clear-all-targets (=> (GhcMonad m) (m ())))
  (do (setTargets [])
      (void (GhcMake.load LoadAllTargets))))

(defn (:: clear-caches (Fnk ()))
  (cond-expand
    [(<= 904 :ghc)
     (do (<- fnk-env getFnkEnv)
         (lept [clear (. void liftIO iface-clearCache)])
         (mapM- clear (envInterpModIfaceCache fnk-env)))]
    [otherwise
     (pure ())]))


;;; Functions for show command

(defn (:: show-bindings (Fnk Code))
  (where (do (<- bs getBindings)
             (<- docs (mapM make-doc (reverse bs)))
             (<- str (show-sdoc-for-user-m (vcat docs)))
             (return `(System.IO.putStrLn ,str)))
    (defn (:: make-doc (-> TyThing (Fnk SDoc))) [tt]
      (case tt
        (AnId i) (pprTypeAndContents i)
        _ (do (<- mb-stuff (getInfo False (getName tt)))
              (return (maybe (text "") ppr-tt mb-stuff)))))
    (defn ppr-tt [(, thing _ _ _ _)]
      (pprTyThing showToHeader thing))))

(defn (:: show-context (Fnk Code))
  (where (do (<- context getContext)
             (<- dflags getSessionDynFlags)
             (return `(System.IO.putStr ,(result dflags context))))
    (defn result [dflags context]
      (unlines (: "; context" (map (context-string dflags) context))))
    (defn context-string [dflags ctx]
      (case ctx
        (IIDecl d) (++ ";  IIDecl: " (showSDoc dflags (ppr d)))
        (IIModule m) (++ ";  IIModule: " (moduleNameString m))))))

(defn (:: show-backend (-> DynFlags String))
  (cond-expand
    [(<= 902 :ghc)
     (. show backend)]
    [otherwise
     (. show hscTarget)]))

(defn (:: show-dflags (Fnk Code))
  (lefn [(ss [dflags]
           ["; dflags:"
            (++ ";  ghcLink: " (show (ghcLink dflags)))
            (++ ";  ghcMode: " (showGhcMode (ghcMode dflags)))
            (++ ";  backend: " (show-backend dflags))
            (++ ";  objectDir: " (show (objectDir dflags)))
            (++ ";  homeUnitId: " (showUnitId dflags))
            (++ ";  forceRecomp: "
                (show (gopt Opt-ForceRecomp dflags)))])
         (showGhcMode [m]
           (case m
             CompManager "CompManager"
             OneShot     "OneShot"
             MkDepend    "MkDepend"))]
    (do (<- dflags getDynFlags)
        (return `(System.IO.putStr ,($ unlines ss dflags))))))

(defn (:: show-hpt (Fnk Code))
  "Show home package table."
  (do (<- hsc-env getSession)
      (<- str0 (show-sdoc-for-user-m (pprHPT (hsc-HPT hsc-env))))
      (lept [str1 (if (null str0)
                    "show: no home package table found"
                    str0)])
      (return `(System.IO.putStrLn ,str1))))

;;; Mostly taken from `GHCi.UI.showLanguages''.
(defn (:: show-language (-> Bool (Fnk Code)))
  [show-all]
  (do (<- dflags getDynFlags)
      (lefn [(setting [test flag]
               (where (| (quiet     empty)
                         (is-on     (<> (text "-X") (text name)))
                         (otherwise (<> (text "-XNo") (text name))))
                 (= name (flagSpecName flag))
                 (= f (flagSpecFlag flag))
                 (= is-on (test f dflags))
                 (= quiet (&& (not show-all)
                              (== (test f default-dflags) is-on)))))

             (default-lang
               (cond-expand
                 [(<= 902 :ghc) GHC2021]
                 [otherwise Haskell2010]))

             (default-dflags
               (lang-set (gen-default-dflags dflags)
                         (case (language dflags)
                           Nothing (Just default-lang)
                           other   other)))])
      (<- str
        (show-sdoc-for-user-m
         (vcat [(<> (text "base language is: ")
                    (case (language dflags)
                      Nothing (text (show default-lang))
                      (Just lang) (ppr lang)))
                ($$ (if show-all
                      (text "all active language options:")
                      (text "with the following modifiers:"))
                    (nest 2 (vcat (map (setting xopt) xFlags))))])))
      (return `(System.IO.putStrLn ,str))))

(defn (:: show-loader-state (-> HscEnv (IO ())))
  [hsc-env]
  (cond-expand
    [(<= 902 :ghc)
     (case (hsc-interp hsc-env)
       (Just interp) (do (initLoaderState interp hsc-env)
                         (show-linker-state hsc-env))
       _ (pure ()))]
    [otherwise
     ;; XXX: `Linker.showLinkerState' reads from `v_PersistentLinkerState',
     ;; which is not exposed from the module its defined ... not sure how
     ;; to get resulting output as `String' other than redirecting output
     ;; to stdout.
     (>> (initDynLinker hsc-env)
         (show-linker-state hsc-env))]))

(defn (:: show-linker (Fnk Code))
  (do (<- hsc-env getSession)
      (liftIO (show-loader-state hsc-env))
      (return '(:begin))))

(defn (:: show-macros (Fnk Code))
  (do (<- macros (fmap envMacros getFnkEnv))
      (lept [macro-strings (unlines
                            (: "; macros: "
                               (map (++ ";  ") (macroNames macros))))])
      (return `(System.IO.putStr ,macro-strings))))

(defn (:: show-modules (Fnk Code))
  (do (<- graph0 getModuleGraph)
      (lept [graph1 (mgModSummaries graph0)])
      (<- graph2 (filterM (. isLoaded ms_mod_name) graph1))
      (<- mods (mapM showModule graph2))
      (return `(System.IO.putStr ,(unlines mods)))))

(defn (:: show-options (-> Bool (Fnk Code)))
  [show-all]
  (do (<- dflags getDynFlags)
      (lefn [(setting [prefix no-prefix test flag]
               (where (| (quiet     empty)
                         (is-on     (<> (text prefix) (text name)))
                         (otherwise (<> (text no-prefix) (text name))))
                 (= name (flagSpecName flag))
                 (= f (flagSpecFlag flag))
                 (= is-on (test f dflags))
                 (= quiet (&& (not show-all)
                              (== (test f default-dflags) is-on)))))
             (default-dflags
               (gen-default-dflags dflags))
             ((, ghciFlags others)
               (partition (\f (elem (flagSpecFlag f) flgs)) fFlags))
             (flgs
               [Opt_PrintExplicitForalls
                Opt_PrintExplicitKinds
                Opt_PrintBindResult
                Opt_BreakOnException
                Opt_BreakOnError
                Opt_PrintEvldWithShow])
             (sdocs
               [($$ (text "REPL specific dynamic flag settings:")
                    (nest 2 (vcat (map (setting "-f" "-fno-" gopt)
                                       ghciFlags))))
                ($$ (text "other dynamic, non-language, flag settings:")
                    (nest 2 (vcat (map (setting "-f" "-fno-" gopt)
                                       others))))
                ($$ (text "warning settings:")
                    (nest 2 (vcat (map (setting "-W" "-Wno-" wopt)
                                       wWarningFlags))))])
             (printOthers
               `(Data.Foldable.mapM_
                 System.IO.putStrLn
                 ,(map (showSDoc dflags) sdocs)))])
      (<- printLang (show-language show-all))
      (return `(>> ,printLang ,printOthers))))

(defn (:: show-packages (Fnk Code))
  (do (<- dflags getDynFlags)
      (lefn [(pr (++ ";   "))
             (pr-flag [flag]
               (case flag
                 (ExposePackage n _ _) (pr n)
                 (HidePackage n) (pr (++ "hiding " n))))
             (pkgs
               (: "; packages" (map pr-flag (packageFlags dflags))))])
      (return `(System.IO.putStr ,(unlines pkgs)))))

(defn (:: show-paths (Fnk Code))
  (do (<- dflags getDynFlags)
      (<- cwd (liftIO getCurrentDirectory))
      (lept [ipaths (importPaths dflags)
             result (unlines (concat
                              [["; current working directory:"
                                (++ ";   " cwd)
                                "; module import search paths:"]
                               (if (null ipaths)
                                 [";    none"]
                                 (map (++ ";    ") ipaths))]))])
      (return `(System.IO.putStr ,result))))

(defn (:: show-targets (Fnk Code))
  (do (<- hsc-env getSession)
      (<- strs (mapM (. show-sdoc-for-user-m pprTarget)
                     (hsc-targets hsc-env)))
      (return `(System.IO.putStrLn (++ "; targets: "
                                       ,(if (null strs)
                                          "none"
                                          (unwords strs)))))))


;;; REPL commands

(defn (:: help-cmd ReplAction)
  [_form]
  (return
   `(System.IO.putStrLn ,(++ "DESCRIPTION: \n\
\\n\
\  REPL meta macro, ARGS varies per COMMAND.\n\
\\n\
\COMMANDS:\n\
\\n" (unlines
      (map (\rc
             (lept [pre (unwords (: (rc-name rc) (rc-args rc)))]
               (concat
                ["  ," (printf "%-14s" pre) " - " (rc-help rc)])))
           commands))))))

(defn (:: system-cmd ReplAction)
  "Invoke system command."
  [forms]
  ;; Using `callProces' which uses `System.Process.RawCommand' instead of
  ;; `System.Process.Shell', to support invoking commands without shell.
  (do (case (map show forms)
        [] (return ())
        (: cmd rest) (liftIO (callProcess cmd rest)))
      (return '(:begin))))

;;; Mostly taken from `GHCi.UI.guessCurrentModule'.
(defn (:: guess-current-module (-> Code (Fnk Module)))
  [form]
  (case-do getContext
    (: (IIModule m) _) (findModule m Nothing)
    (: (IIDecl d) _)   (cond-expand
                         [(<= 904 :ghc)
                          (do (<- pq (GHC.renameRawPkgQualM (unLoc (ideclName d))
                                                            (ideclPkgQual d)))
                              (GHC.findQualifiedModule pq (unLoc (ideclName d))))]
                         [otherwise
                          (findModule (unLoc (ideclName d))
                                      (fmap sl-fs (ideclPkgQual d)))])
    _ (finkelSrcError form "browse: no current module")))

;;; Mostly taken from `GHCi.UI.browseCmd'.
(defn (:: browse-cmd ReplAction)
  [forms]
  (lefn [(go [mb-name]
            (case mb-name
              (Just name) (| ((looksLikeModuleName name)
                              (>>= (lookupModule (mkModuleName name) Nothing)
                                   go')))
              _ (>>= (guess-current-module (located-list forms)) go')))
         (go' [mdl]
           (case-do (getModuleInfo mdl)
             (Just mod-info) (browse-module mdl mod-info)
             Nothing (lept [mname (moduleName mdl)
                            str (moduleNameString mname)
                            msg (++ "unknown module: " str)]
                       (return `(System.IO.putStrLn ,msg)))))]
    (case (map unCode forms)
      [(Atom (ASymbol sym))] (go (Just (unpackFS sym)))
      [] (go Nothing)
      _ (invalid-form "browse" forms))))

(defn (:: expand-path (=> (MonadIO m) (-> FilePath (m FilePath))))
  (lefn [(:: try-getHomeDirectory (IO (Either SomeException FilePath)))
         (try-getHomeDirectory (try getHomeDirectory))
         (go [path]
           (case path
             (: #'~ rest) (case-do try-getHomeDirectory
                            (Right home) (pure (normalise
                                                (++ home (: #'/ rest))))
                            (Left _) (pure path))
             _ (return path)))]
    (. liftIO go)))

;;; From `GHCi.UI.changeDirectory'.
(defn (:: cd-cmd ReplAction)
  "Function to change current directory."
  [forms]
  (lefn [(work [dir0]
           (do (<- graph getModuleGraph)
               (<- mods (fmap envContextModules getFnkEnv))
               (when ($ not null mgModSummaries graph)
                 (liftIO (putStrLn warn-unloading)))
               clear-all-targets
               clear-caches
               (setContext (map mk-ii-str mods))
               workingDirectoryChanged
               (liftIO (do (<- dir1 (expand-path dir0))
                           (setCurrentDirectory dir1)))
               (return '(:begin))))
         (warn-unloading
           (++ "Warning: "
               "changing directory causes all loaded modules to be unloaded\n"
               "because the search path has changed."))]
    (case forms
      [] (>>= (expand-path "~") work)
      [arg1] (| ((<- (Just path) (code-to-mb-string arg1))
                 (work path)))
      _ (invalid-form "cd" forms))))

(defn (:: expand-cmd ReplAction)
  "Expand given form for one layer."
  (expand-with "expand" expand1))

(defn (:: expand-full-cmd ReplAction)
  "Fully expand given form."
  (expand-with "expand!" expand))

(defn (:: info-cmd ReplAction)
  [forms]
  (case (map unCode forms)
    [(@ form (Atom (ASymbol _)))] (info-name (toCode form))
    [(Atom AUnit)] (info-name (make-symbol "()"))
    [(HsList [])] (info-name (make-symbol "[]"))
    _ (invalid-form "info" forms)))

;; From `GHCi.UI.kindOfType'
(defn (:: kind-cmd ReplAction)
  [forms]
  (case forms
    [form] (do (<- ty0 (buildHsSyn parseType forms))
               (<- (, _ kind) (evalTypeKind ty0))
               (lept [sdoc (hsep [(text (show form))
                                  dcolon
                                  (pprSigmaType kind)])])
               (<- str (show-sdoc-for-user-m sdoc))
               (return `(System.IO.putStrLn ,str)))
    _ (invalid-form "kind" forms)))

(defn (:: load-cmd ReplAction)
  "Load a module source code file. Handles absolute paths and relative
paths from import directories."
  [forms]
  (lefn [(clear-all
           ;; Clear the main session, then clear the macro expander session.
           (do do-clear-all
               (case-do (fmap envSessionForExpand getFnkEnv)
                 (Just mex-env-1) (do-clear-mex mex-env-1)
                 _ (pure ()))))
         (do-clear-mex [hsc-env-1]
           (withTempSession (const hsc-env-1)
                            (do do-clear-all
                                (<- hsc-env-2 getSession)
                                (modifyFnkEnv
                                 (\e (e {(= envSessionForExpand
                                           (Just hsc-env-2))}))))))
         (do-clear-all []
           ;; See `loadModule'' in "ghc/GHCi/UI.hs". Clearing various states:
           ;; finder cache, targets, interactive context ... etc.
           (do (<- graph0 getModuleGraph)
               (<- _ abandonAll)
               clear-all-targets
               clear-caches
               (<- hsc-env getSession)
               (lept [graph1 (mgModSummaries graph0)
                      uncache (cond-expand
                                [(<= 904 :ghc)
                                 (\ ms (uncacheModule (hsc-FC hsc-env)
                                                      (hsc-home-unit hsc-env)
                                                      (ms-mod-name ms)))]
                                [otherwise
                                 (. (uncacheModule hsc-env) ms_mod_name)])])
               (liftIO (do (mapM_ uncache graph1)
                           (cond-expand
                             [(<= 904 :ghc)
                              (flushFinderCaches (hsc-FC hsc-env)
                                                 (hsc-unit-env hsc-env))]
                             [otherwise
                              (flushFinderCaches hsc-env)])))
               (setSession (discardInteractiveContext hsc-env))))
         (make-target [path]
           (do (<- hsc-env getSession)
               (lept [allow-obj (cond-expand
                                  [(<= 904 :ghc)
                                   ;; Deciding from target name, since the
                                   ;; "-fforce-recomp" option is always turned
                                   ;; ON when object code was not allowed.
                                   (not (isPrefixOf "*" path))]
                                  [otherwise
                                   ($ not is-interpreting hsc-dflags hsc-env)])
                      tfile (TargetFile path Nothing)])
               (pure
                (cond-expand
                  [(<= 904 :ghc)
                   (Target tfile allow-obj (hscActiveUnitId hsc-env) Nothing)]
                  [otherwise
                   (Target tfile allow-obj Nothing)]))))]

    (case forms
      [form] (maybe
              (finkelSrcError form (++ "load: not a FilePath: "
                                       (show form)))
              (\path
                ;; Clear current state first. Then find the source file
                ;; path and compile, load, and link.
                (env-context-on-exception
                 (do clear-all
                     (<- target (make-target path))
                     (<- _ (setTargets [target]))
                     (<- success-flag (compile-and-import [(onTheREPL path)]))
                     (case success-flag
                       Succeeded (liftIO (putStrLn (++ "; loaded " path)))
                       _ (return ()))
                     (return '(:begin)))))
              (code-to-mb-string form))
      _ (invalid-form "load" forms))))

(defn (:: pwd-cmd ReplAction)
  "Function to show current directory."
  [_forms]
  (do (<- dir (liftIO getCurrentDirectory))
      (return `,dir)))

(defn (:: reload-cmd ReplAction)
  "Function to reload previously loaded module."
  [_forms]
  (case-do getTargets
    (cond-expand
      [(<= 904 :ghc)
       (: (Target target-id _ _ _) _)]
      [otherwise
       (: (Target target-id _ _) _)])
    (env-context-on-exception
     (lept [tstr (case target-id
                   (TargetFile path _) path
                   (TargetModule mdl) (moduleNameString mdl))]
       (case-do (compile-and-import [(onTheREPL tstr)])
         Succeeded (do (<- ctx0 getContext)
                       (<- ctx1 (adjust-current-target tstr ctx0))
                       (setContext ctx1)
                       (return `(System.IO.putStrLn
                                 ,(++ "; reloaded " tstr))))
         Failed (return '(:begin)))))
    _ (return '(System.IO.putStrLn "; reload: invalid target"))))

(defn (:: set-cmd ReplAction)
  "Set command line flags, see `GHCi.UI.newDynFlags'."
  [forms]
  (case forms
    (: _ _)
    ;; Always using `setSessionDynFlags' for `set' REPL command to support
    ;; `-package' flag.
    (do (lefn [(all-flags
                 (foldr (\form acc
                          (case (mb-symbol-name form)
                            (Just name) (: name acc)
                            _ acc))
                        [] forms))
               ((, fnk-flags hs-flags)
                 (partitionFnkEnvOptions all-flags))
               (update-fnk-opts [opts fnk-env]
                 (foldl (flip id) fnk-env opts))])
        (when (not (null fnk-flags))
          (case (getOpt Permute fnkEnvOptions fnk-flags)
            (, o _ []) (modifyFnkEnv (update-fnk-opts o))
            (, _ _ es) (liftIO (print es))))
        (<- hsc-env getSession)
        (lept [dflags0 (hsc-dflags hsc-env)])
        (<- (, dflags1 leftovers warns)
          (parseDynamicFlagsCmdLine dflags0 (map onTheREPL hs-flags)))
        (liftIO
         (do (print-or-throw-diagnostics hsc-env dflags1 warns)
             (unless (null leftovers)
               (putStrLn
                (++ "Some flags have not been recognized: "
                    (intercalate ", " (map unLoc leftovers)))))))
        (<- _ (setSessionDynFlags dflags1))
        (<- dflags2 getDynFlags)
        (setDynFlags dflags2)
        ;; Updating two more `DynFlags', one is the default `DynFlags' used to
        ;; import modules during macro expansion, and another is the `DynFlags'
        ;; in the `HscEnv' used by the macro expander.
        (modifyFnkEnv
         (\e (e {(= envDefaultDynFlags (Just dflags2))
                 (= envSessionForExpand (fmap (updateDynFlags dflags2)
                                              (envSessionForExpand e)))})))
        (return '(:begin)))
    _ (finkelSrcError nil "set: empty form")))

(defn (:: show-cmd ReplAction)
  [forms]
  (where go
    (defn go
      (case forms
        [form] (| ((<- (Just name) (mb-symbol-name form))
                   (<- (Just act) (lookup name things))
                   act))
        _ (finkelSrcError nil (++ "show: expecting one of:\n"
                              (intercalate ", " (map fst things))))))
    (defn things
      [(, "bindings" show-bindings)
       (, "context" show-context)
       (, "dflags" show-dflags)
       (, "hpt" show-hpt)
       (, "language" (show-language False))
       (, "linker" show-linker)
       (, "macros" show-macros)
       (, "modules" show-modules)
       (, "options" (show-options False))
       (, "options!" (show-options True))
       (, "packages" show-packages)
       (, "paths" show-paths)
       (, "targets" show-targets)])))

;; From `GHCi.UI.typeOfExpr'.
(defn (:: type-cmd ReplAction)
  [forms]
  (case forms
    [form] (do (<- expanded (expand form))
               (<- expr (buildHsSyn parseExpr [expanded]))
               (<- ty (evalExprType expr))
               (lept [sdoc (sep [(text (show form))
                                 (nest 2 (<+> dcolon (pprSigmaType ty)))])])
               (<- str (show-sdoc-for-user-m sdoc))
               (return `(System.IO.putStrLn ,str)))
    _ (invalid-form "type" forms)))

(defn (:: verbose-cmd ReplAction)
  "Modify verbosity settings in REPL."
  [forms]
  (case forms
    [] (do (<- lvl (fmap envVerbosity getFnkEnv))
           (return
            `(System.IO.putStrLn ,(++ "Verbosity level is " (show lvl)))))
    [form] (| ((<- (Just n) (readMaybe (show form)))
               (do (modifyFnkEnv (setFnkVerbosity n))
                   (lept [msg (++ "Verbosity level set to " (show n))])
                   (return `(System.IO.putStrLn ,msg)))))
    _ (invalid-form "verbose" forms)))


;;; REPL command macro

(defn (:: commands [ReplCmd])
  (lept [c ReplCmd]
    [(c "!" ["CMD" "ARGS" "..."] system-cmd
        "run system CMD with ARGS")
     (c "?" [] help-cmd
        "show this help")
     (c "browse" ["MODULE"] browse-cmd
        "browse contents of MODULE")
     (c "cd" ["DIR"] cd-cmd
        "change working directory to DIR")
     (c "expand" ["FORM"] expand-cmd
        "show expanded result of FORM")
     (c "expand!" ["FORM"] expand-full-cmd
        "show fully expanded result of FORM")
     (c "info" ["NAME"] info-cmd
        "show info of NAME")
     (c "kind" ["TYPE"] kind-cmd
        "show kind of TYPE")
     (c "load" ["FILE"] load-cmd
        "compile and load FILE")
     (c "pwd" [] pwd-cmd
        "show working directory")
     (c "reload" [] reload-cmd
        "reload previous module")
     (c "set" ["FLAGS" "..."] set-cmd
        "parse and set FLAGS")
     (c "show" ["ARG"] show-cmd
        "show information of ARG")
     (c "type" ["EXPR"] type-cmd
        "show type of EXPR")
     (c "verbose" ["INT"] verbose-cmd
        "set finkel verbosity to INT")]))

(defmacroM repl-macro form
  (case (unCode form)
    (List (: name args)) (case (do (<- name' (code-to-mb-string name))
                                   (find (. (isPrefixOf name') rc-name)
                                         commands))
                           (Just rc) (rc-action rc args)
                           _ (help-cmd []))
    _ (finkelSrcError form (++ "invalid args: " (show form)))))
