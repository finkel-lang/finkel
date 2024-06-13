;;; -*- mode: finkel -*-

%p(LANGUAGE TypeApplications
            TypeFamilies)

;;;; Some commonly used version compatibility type and functions

(:require Finkel.Core)

(defmodule Finkel.Tool.Internal.Compat
  (export WARNINGs NamePprCtx print-or-throw-diagnostics
          ppr-wrapped-msg-bag-with-loc get-name-ppr-ctx)
  (require
   (Finkel.Tool.Internal.Macro.Ghc))
  (import-when [:compile]
    ;; finkel-core
    (Finkel.Prelude))
  (import
   ;; finkel-kernel
   (Language.Finkel.Error [WARNINGs printOrThrowDiagnostics'])))

;;; ghc

(imports-from-ghc
 (GHC.Driver.Env [(HscEnv ..)])
 (GHC.Driver.Monad [(GhcMonad ..)])
 (GHC.Driver.Session [(DynFlags ..)])
 (GHC.Utils.Outputable [SDoc]))

(cond-expand
  [(<= 906 :ghc)
   (import GHC (NamePprCtx getNamePprCtx))]
  [otherwise
   (import GHC (PrintUnqualified getPrintUnqual))])

(cond-expand
  [(<= 906 :ghc)
   (:begin
     (import GHC.Driver.Errors.Types ((GhcMessage ..) GhcMessageOpts))
     (import GHC.Types.Error ((Messages ..) (Diagnostic ..)
                              defaultDiagnosticOpts)))]
  [(<= 904 :ghc)
   (import GHC.Types.Error (Diagnostic Messages getMessages))]
  [otherwise
   (:begin
     (import Language.Finkel.Error (WrappedMsg))
     (imports-from-ghc
      (GHC.Data.Bag [Bag])))])

(cond-expand
  [(<= 902 :ghc)
   (import GHC.Utils.Error (pprMsgEnvelopeBagWithLoc))]
  [otherwise
   (imports-from-ghc
    (GHC.Utils.Error [pprErrMsgBagWithLoc]))])

;;; Functions

(:: get-name-ppr-ctx (=> (GhcMonad m) (m NamePprCtx)))

(cond-expand
  [(<= 906 :ghc)
   (defn get-name-ppr-ctx getNamePprCtx)]
  [otherwise
   (:begin
     (type NamePprCtx PrintUnqualified)
     (defn get-name-ppr-ctx getPrintUnqual))])

(defn (:: print-or-throw-diagnostics (-> HscEnv DynFlags WARNINGs (IO ())))
  [_hsc-env dflags warns]
  (cond-expand
    [(<= 902 :ghc)
     (printOrThrowDiagnostics' (hsc-logger _hsc-env) dflags warns)]
    [otherwise
     (printOrThrowDiagnostics' (error "no logger") dflags warns)]))

(cond-expand
  [(<= 906 :ghc)
   (defn (:: ppr-wrapped-msg-bag-with-loc
           (=> (Diagnostic e) (~ (DiagnosticOpts e) GhcMessageOpts)
               (-> (Messages e) [SDoc])))
     [msg]
     (pprMsgEnvelopeBagWithLoc (defaultDiagnosticOpts @GhcMessage)
                               (getMessages msg)))]
  [(<= 904 :ghc)
   (defn (:: ppr-wrapped-msg-bag-with-loc
           (=> (Diagnostic e) (-> (Messages e) [SDoc])))
     (. pprMsgEnvelopeBagWithLoc getMessages))]
  [otherwise
   (defn (:: ppr-wrapped-msg-bag-with-loc (-> (Bag WrappedMsg) [SDoc]))
     (cond-expand
       [(<= 902 :ghc) pprMsgEnvelopeBagWithLoc]
       [otherwise pprErrMsgBagWithLoc]))])
