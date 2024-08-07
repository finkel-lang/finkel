;;; -*- mode: finkel -*-

;;; Eval loop in REPL.

(defmodule Finkel.Tool.Internal.Eval
  (export eval-loop eval-once fork-eval-loop)
  (require
   ;; Internal
   (Finkel.Tool.Internal.Macro.Ghc))
  (import-when [:compile]
   ;; finkel-core
   (Finkel.Prelude))
  (import
   ;; base
   (Control.Concurrent [MVar ThreadId forkIOWithUnmask putMVar takeMVar])
   (Control.Exception
    [(AsyncException ..) (Exception ..) SomeException
     fromException throwIO throwTo])
   (Control.Monad (unless))
   (Control.Monad.IO.Class [(MonadIO ..)])
   (Data.Foldable [toList])
   (Data.List [intercalate])
   (GHC.Conc [myThreadId])
   (System.IO [Handle])

   ;; exceptions
   (Control.Monad.Catch [catch])

   ;; ghc-boot
   (GHC.LanguageExtensions [(Extension ..)])

   ;; ghci
   (GHCi.Message [(EvalExpr ..)])
   (GHCi.RemoteTypes (ForeignHValue))

   ;; finkel-kernel
   (Language.Finkel)
   (Language.Finkel.Builder [HDecl HImportDecl HStmt
                             syntaxErrCode syntaxErrMsg evalBuilder])
   (Language.Finkel.Error [mkWrappedMsg])
   (Language.Finkel.Eval [evalDecls])
   (Language.Finkel.Exception [finkelExceptionLoc])
   (Language.Finkel.Make [initSessionForMake isFnkFile isHsFile])
   (Language.Finkel.Fnk
    [(FnkEnv ..) failFnk modifyFnkEnv prepareInterpreter setDynFlags
     useInterpreter withTmpDynFlags])
   (Language.Finkel.Syntax [parseExpr parseImports parseStmt parseTopDecls])

   (Language.Finkel.Plugin [setFinkelPluginWithArgs])
   (Finkel.Core.Plugin [plugin])

   ;; internal
   (Finkel.Tool.Internal.Compat)
   (Finkel.Tool.Internal.Exception)
   (Finkel.Tool.Internal.IO)
   (Finkel.Tool.Internal.Macro.Repl)
   (Finkel.Tool.Internal.Types)))

(imports-from-ghc
 (GHC
  [(Target ..) (TargetId ..) parseDynamicFlags setTargets])

 (GHC.Data.Bag [unitBag])
 (GHC.Data.OrdList [toOL])

 (GHC.Driver.Env [(HscEnv ..)])
 (GHC.Driver.Monad [(GhcMonad ..) modifySession withTempSession])
 (GHC.Driver.Ppr [showSDoc showPpr])
 (GHC.Driver.Session
  [(DynFlags ..) (GeneralFlag ..) (HasDynFlags ..) (Option ..)
   gopt-set xopt-unset])

 (GHC.Parser.PostProcess [cvTopDecls])

 (GHC.Runtime.Context [(InteractiveImport ..) setInteractivePrintName])
 (GHC.Runtime.Eval [(ExecOptions ..) (ExecResult ..) compileParsedExprRemote
                    getContext parseName setContext execStmt' execOptions])

 (GHC.Types.Basic [(SuccessFlag ..)])

 (GHC.Types.Name [(Name) getName nameOccName occNameString])
 (GHC.Types.SourceError [SourceError srcErrorMessages])
 (GHC.Types.SrcLoc [mkGeneralLocated unLoc])
 (GHC.Types.TyThing [(TyThing ..)])
 (GHC.Types.Var [Var varType])

 (GHC.Unit.Module [mkModuleName])

 (GHC.Utils.Misc [looksLikeModuleName])
 (GHC.Utils.Outputable
  [SDoc ppr mkErrStyle setStyleColoured text vcat]))


;;; Extra imports

(import GHC.Hs.ImpExp ((ImportDecl ..) isImportDeclQualified))

(cond-expand
  [(<= 906 :ghc)
   (import Language.Haskell.Syntax.ImpExp ((ImportListInterpretation ..)))]
  [otherwise
   (:begin)])

(cond-expand
  [(<= 904 :ghc)
   (:begin
     (import GHC.Driver.Env (hscActiveUnitId))
     (import GHC.Types.Error (mkMessages)))]
  [otherwise
   (:begin)])

(cond-expand
  [(<= 902 :ghc)
   (import GHC.Utils.Outputable (renderWithContext))]
  [otherwise
   (imports-from-ghc
    (GHC.Utils.Outputable [renderWithStyle]))])

(cond-expand
  [(<= 902 :ghc)
   (import GHC.Driver.Session (initSDocContext))]
  [(<= 900 :ghc)
   (import GHC.Utils.Outputable (initSDocContext))]
  [otherwise
   (:begin)])

;;; Version compatibility function

(defn (:: optional-dynflags [GeneralFlag])
  "Optional 'GeneralFlag' set for REPL.

See \"GHCi.UI\", \"GHCi.UI.Monad\", and \"ghc/Main.hs\"."
  (cond-expand
    [(<= 906 :ghc)
     [Opt-ImplicitImportQualified
      Opt-IgnoreOptimChanges
      Opt-IgnoreHpcChanges
      Opt-UseBytecodeRatherThanObjects]]
    [otherwise
     [Opt-ImplicitImportQualified
      Opt-IgnoreOptimChanges
      Opt-IgnoreHpcChanges]]))

(defn (:: parse-dynamic-flags
        (=> (MonadIO m)
            (-> HscEnv [Located String]
                (m (, DynFlags [Located String] WARNINGs)))))
  [hsc-env]
  (cond-expand
    [(<= 902 :ghc)
     (parseDynamicFlags (hsc-logger hsc-env) (hsc-dflags hsc-env))]
    [otherwise
     (parseDynamicFlags (hsc-dflags hsc-env))]))

(defn (:: render-with-err-style (-> DynFlags NamePprCtx SDoc String))
  [dflags unqual sdoc]
  (lept [style0 (cond-expand
                  [(<= 900 :ghc) (mkErrStyle unqual)]
                  [otherwise (mkErrStyle dflags unqual)])
         style1 (setStyleColoured True style0)]
    (cond-expand
      [(<= 902 :ghc)
       (renderWithContext (initSDocContext dflags style1) sdoc)]
      [(<= 900 :ghc)
       (renderWithStyle (initSDocContext dflags style1) sdoc)]
      [otherwise
       (renderWithStyle dflags sdoc style1)])))


;;; Eval loop

(defn (:: fork-eval-loop
        (-> [String] Handle (MVar Input) FnkEnv (IO ThreadId)))
  [ghc-args hdl in-mv fnk-env]
  (do (<- me myThreadId)
      (forkIOWithUnmask
       (\unmask
         (catch (unmask (runFnk (eval-loop ghc-args hdl in-mv) fnk-env))
           (\e (throwTo me (:: e SomeException))))))))

(defn (:: init-eval-loop (-> [String] [String] (Fnk ForeignHValue)))
  "Initialization works for evaluation loop."
  [eval-wrapper-opts ghc-opts]
  (do prepareInterpreter

      ;; Parse the ghc options from argument, assuming that the arguments are
      ;; passed from the command line.
      (<- hsc-env0 getSession)
      (lept [on-the-commandline (mkGeneralLocated "on the commandline")
             ghc-opts2 (<> ["-F" "-pgmF" "fnkpp" "-optF" "--no-warn-interp"]
                           ghc-opts)
             ;; XXX: Get plugin options from command line
             plugin-args []
             lghc-opts (map on-the-commandline ghc-opts2)])
      (<- (, dflags0 fileish warns) (parse-dynamic-flags hsc-env0 lghc-opts))
      (liftIO (print-or-throw-diagnostics hsc-env0 dflags0 warns))

      ;; As done in the Main.hs in "ghc-bin" package, updating the `ldInputs'
      ;; field o the `DynFlags' with `FileOption', to support linking object
      ;; files.
      (lept [dflags1 (foldl gopt-set dflags0 optional-dynflags)
             (, srcs objs) (partition-args fileish)
             dflags2 (dflags1 {(= ldInputs
                                 (++ (map (. (FileOption "") unLoc) objs)
                                     (ldInputs dflags1)))})])

      ;; Initializing plugins with dflags from updated session.
      (setDynFlags dflags2)
      initSessionForMake

      ;; Registring the finkel plugin.
      ;;
      ;; XXX: It is possible to pass "-fplugin" option and dynamically load the
      ;; plugin module. Reconsider after rewriting other commands with
      ;; plugin. If so, modify the `ghc-opts' to take the options for plugin.
      (setFinkelPluginWithArgs plugin plugin-args)

      ;; Setting the default `DynFlags' for macro expansion.
      (<- dflags3 getDynFlags)
      (modifyFnkEnv (\e (e {(= envDefaultDynFlags (Just dflags3))})))

      ;; Load modules specified from command line, when given.
      (lept [err (=<< (. liftIO throwIO FinkelToolException))])
      (unless (null srcs)
        (catch (do (setTargets (map (guessFnkTarget hsc-env0) srcs))
                   (<- sflag (compile-and-import srcs))
                   (case sflag
                     Failed ($ err pure (++ "Failed loading: ")
                               (intercalate ", ") (map unLoc) fileish)
                     Succeeded (pure ())))
          (\e
            (cond
              [(<- (Just se) (fromException e))
               ($ err make-src-err-message se)]
              [(<- (Just fe) (fromException e))
               ($ err make-finkel-exception-message fe)]
              [otherwise
               ($ err pure displayException e)]))))

      ;; XXX: Currently the printer function and the arguments returned from
      ;; "System.Environment.getArgs" are defined here and cannot be changed.
      (set-print-name "System.IO.print")

      ;; Pass the argument to evaluation wrapper, to set the value of `argv'
      ;; returned from `System.Environment.getArgs'.
      (make-eval-wrapper eval-wrapper-opts)))

(defn (:: eval-loop (-> [String] Handle (MVar Input) (Fnk ())))
  "Loop to evaluate expressions."
  (eval-loop-or-once False []))

(defn (:: eval-once (-> [String] [String] Handle (MVar Input) (Fnk ())))
  "Evalute the form once and return."
  (eval-loop-or-once True))

(defn (:: eval-loop-or-once
        (-> Bool [String] [String] Handle (MVar Input) (Fnk ())))
  "Evaluate expressions, and loop or return."
  [once-only wrapper-args ghc-opts hdl in-mvar]
  (lefn [(with-async-handler [wrapper act]
           (catch act
             (\e
               (case (fromException e)
                 (Just UserInterrupt) (loop wrapper)
                 (Just ThreadKilled) (return ())
                 _ (liftIO (throwIO e))))))
         (:: throw-async-io (-> AsyncException (Fnk a)))
         (throw-async-io (. liftIO throwIO))
         (withErrorHandler [act]
           (catch act
            (\e
              (cond
                [(<- (Just se) (fromException e))
                 (fmap Left (make-src-err-message se))]
                [(<- (Just ae) (fromException e))
                 (throw-async-io ae)]
                [(<- (Just fe) (fromException e))
                 (fmap Left (make-finkel-exception-message fe))]
                [otherwise
                 ($ pure Left show e)]))))
         (eval-one [wrapper]
           (do (<- (Input itype form out-mv) (liftIO (takeMVar in-mvar)))
               (<- ret (withErrorHandler
                        (do (<- expanded (expands [form]))
                            (<- dflags getDynFlags)
                            (eval-form hdl dflags wrapper itype expanded))))
               (liftIO (putMVar out-mv ret))))
         (loop [wrapper]
            (with-async-handler wrapper
              (>> (eval-one wrapper)
                  (loop wrapper))))]
    (>>= (init-eval-loop wrapper-args ghc-opts)
         (if once-only eval-one loop))))

(defn (:: set-print-name (-> String (Fnk ())))
  "Set the name of function used for printing values in interactive
context."
  [name]
  (case-do (fmap toList (parseName name))
    (: f _) (modifySession
             (\he (he {(= hsc-IC (setInteractivePrintName (hsc-IC he) f))})))
    _ (failFnk "set-print-name: parse error")))

(defn (:: eval-form
        (-> Handle DynFlags ForeignHValue InSource [Code] (Fnk Result)))
  [hdl dflags wrapper itype forms]
  (| ((null forms)
      (return (Right "")))
     ((<- (Right stmt) (evalBuilder dflags True parseStmt forms))
      (eval-statement hdl wrapper itype stmt))
     ((<- (Right decls) (evalBuilder dflags True parseTopDecls forms))
      (eval-decls decls))
     (otherwise
      (case (evalBuilder dflags True parseImports forms)
        (Right idecl) (eval-imports dflags idecl)
        (Left se) (finkelSrcError (syntaxErrCode se) (syntaxErrMsg se))))))

(defn (:: eval-statement
        (-> Handle ForeignHValue InSource HStmt (Fnk Result)))
  [hdl wrapper itype stmt]
  (lept [wrap (case itype
                Prompt (fmap (\r (, r "")))
                Connection (with-io-redirect hdl))
         err (. pure Left (++ "*** Exception: ") show)
         ok (. pure Right)
         opts (execOptions {(= execWrap
                              (\fhv (EvalApp (EvalThis wrapper)
                                             (EvalThis fhv))))})]
    (case-do (wrap (execStmt' stmt "stmt-text" opts))
      (, (ExecComplete (Right _ns) _) r) (ok r)
      (, (ExecComplete (Left e) _) _r) (err e)
      (, (ExecBreak {}) r) (pure (Left (++ "break: " r))))))

(defn (:: eval-imports (-> DynFlags [HImportDecl] (Fnk Result)))
  [dflags imports]
  (lefn [(mkIIDecl [(L _ idecl)]
           (IIDecl idecl))
         (imps (map (. (showSDoc dflags) ppr) imports))
         (mdls (++ "; " (intercalate ", " imps)))
         (add-imports [ctx]
           (foldr (\mdl (add-gt-ii (mkIIDecl mdl))) ctx imports))]
    (do (<- ctx0 getContext)
        (setContext (add-imports ctx0))
        (return (Right mdls)))))

(defn (:: eval-decls (-> [HDecl] (Fnk Result)))
  [decls]
  (do (<- hsc-env getSession)
      (lefn [(decls' (cvTopDecls (toOL decls)))
             (dflags (hsc-dflags hsc-env))
             (pr [tt]
               (case tt
                 (AnId var) (var-name-and-type dflags (getName var) var)
                 _ (++ "; " (showSDoc dflags (ppr tt)))))
             (show-tything [tt acc]
               (lefn [(nstr (showSDoc dflags (ppr (getName tt))))]
                 (if (== "$trModule" nstr)
                   acc
                   (: (pr tt) acc))))
             (tystr [tt]
               (intercalate "\n" (foldr show-tything [] tt)))])

      ;; In "ghc/GHCi/UI.hs", the `runStmt' function is wrapping declarations
      ;; with `let' expression and passing to `execStmt'' as a work around for
      ;; supporting top level declaration. However, this approach seems like
      ;; not working well when multiple declarations were entered at once.
      ;;
      ;; In finkel REPL, instead of wrapping with `let', always using
      ;; `HscInterpreted' as target when evaluating declarations, to support
      ;; declaring functions and values when the REPL is using `-fobject-code'.
      (<- (, tythings ic)
        (withTmpDynFlags (useInterpreter dflags) (evalDecls decls')))

      (setSession (hsc-env {(= hsc-IC ic)}))
      (return (Right (tystr tythings)))))


;;; Auxiliary

(defn (:: guessFnkTarget (-> HscEnv (Located String) Target))
  "Simple function to do similar work done in `GHC.guessTarget', to support
source code file paths with @.fnk@ extension."
  [_hsc_env lsrc]
  (lept [src (unLoc lsrc)
         tid (if (looksLikeModuleName src)
               (TargetModule (mkModuleName src))
               (TargetFile src Nothing))]
    (cond-expand
      [(<= 904 :ghc)
       (Target tid True (hscActiveUnitId _hsc_env) Nothing)]
      [otherwise
       (Target tid True Nothing)])))

(defn (:: partition-args
        (-> [(Located String)] (, [(Located String)] [(Located String)])))
  "Simplified version of the function with same name defined in @ghc/Main.hs@,
to separate object files from source code files."
  (lefn [(f [(L l arg) (, srcs objs)]
           (if (|| (isFnkFile arg)
                   (isHsFile arg)
                   (looksLikeModuleName arg))
             (, (: (L l arg) srcs) objs)
             (, srcs (: (L l arg) objs))))]
    (foldr f (, [] []))))

(defn (:: make-src-err-message (-> SourceError (Fnk String)))
  [src-err]
  (lept [emsgs (srcErrorMessages src-err)
         sdoc (vcat (ppr-wrapped-msg-bag-with-loc emsgs))]
    (do (<- dflags getDynFlags)
        (<- unqual get-name-ppr-ctx)
        (return (render-with-err-style dflags unqual sdoc)))))

(defn (:: make-finkel-exception-message (-> FinkelException (Fnk String)))
  [fe]
  (lept [msg (displayException fe)]
    (do (<- dflags getDynFlags)
        (<- unqual get-name-ppr-ctx)
        (lefn [(lmsg [l]
                 (lept [wmsg (mkWrappedMsg dflags l unqual (text msg))
                        emsgs (cond-expand
                                [(<= 904 :ghc) (mkMessages (unitBag wmsg))]
                                [otherwise (unitBag wmsg)])
                        sdoc (vcat (ppr-wrapped-msg-bag-with-loc emsgs))]
                   (render-with-err-style dflags unqual sdoc)))])
        (case (finkelExceptionLoc fe)
          (Just l) ($ pure lmsg l)
          _ (pure msg)))))

(defn (:: make-eval-wrapper (-> [String] (Fnk ForeignHValue)))
  [args]
  (lept [form `(\m (do (<- r (System.Environment.withArgs ,args m))
                       (System.IO.hFlush System.IO.stdout)
                       (System.IO.hFlush System.IO.stderr)
                       (Control.Monad.return r)))
         no-rb-hsc (\hsc-env
                     (hsc-env {(= hsc-dflags (xopt-unset (hsc-dflags hsc-env)
                                                         RebindableSyntax))}))]
    (do (<- dflags getDynFlags)
        (case (evalBuilder dflags True parseExpr [form])
          (Right expr) (withTempSession no-rb-hsc
                                        (compileParsedExprRemote expr))
          (Left err) (finkelSrcError (syntaxErrCode err)
                                     (syntaxErrMsg err))))))

(defn (:: var-name-and-type (-> DynFlags Name Var String))
  [dflags name var]
  (lept [nstr (occNameString (nameOccName name))
         typ (showPpr dflags (varType var))]
    (if (== nstr "it")
      ""
      (intercalate "\n"
                   (map (++ "; ")
                        (lines (++ nstr (++ " :: " typ))))))))

(defn (:: add-gt-ii
        (-> InteractiveImport [InteractiveImport] [InteractiveImport]))
  [mdl acc]
  (if (any (subsume-ii mdl) acc)
    acc
    (: mdl acc)))

(defn (:: subsume-ii (-> InteractiveImport InteractiveImport Bool))
  ;; See `GHCi.UI.iiSubsumes'.
  [(IIModule x) (IIModule y)] (== x y)
  [(IIDecl x) (IIDecl y)]
  (where (&& (== (unLoc (ideclName x)) (unLoc (ideclName y)))
             (== (ideclAs x) (ideclAs y))
             (|| (not (isImportDeclQualified (ideclQualified x)))
                 (isImportDeclQualified (ideclQualified y)))
             (cond-expand
               [(<= 906 :ghc)
                (hiding-subsumes (ideclImportList x) (ideclImportList y))]
               [otherwise
                (hiding-subsumes (ideclHiding x) (ideclHiding y))]))
    (cond-expand
      [(<= 906 :ghc)
       (defn hiding-subsumes
         [_ (Just (, Exactly (L _ [])))] True
         [(Just (, Exactly (L _ xs))) (Just (, Exactly (L _ ys)))]
         (all (flip elem xs) ys)
         [a b] (== a b))]
      [otherwise
       (defn hiding-subsumes
         [_ (Just (, False (L _ [])))] True
         [(Just (, False (L _ xs))) (Just (, False (L _ ys)))]
         (all (flip elem xs) ys)
         [a b] (== a b))]))

  [_ _] False)
