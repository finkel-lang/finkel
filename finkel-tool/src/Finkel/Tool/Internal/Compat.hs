;;; -*- mode: finkel -*-

;;;; Some commonly used version compatibility type and functions

(:require Finkel.Core)

(defmodule Finkel.Tool.Internal.Compat
  (export WARN handle-flag-warnings ppr-wrapped-msg-bag-with-loc)
  (require
   (Finkel.Tool.Internal.Macro.Ghc))
  (import-when [:compile]
    ;; finkel-core
    (Finkel.Prelude)))

;;; ghc

(imports-from-ghc
 (GHC.Driver.Env [(HscEnv ..)])
 (GHC.Driver.Session [(DynFlags ..)])
 (GHC.Driver.Errors [handleFlagWarnings])
 (GHC.Utils.Outputable [SDoc]))

(cond-expand
  [(<= 904 :ghc)
   (:begin
     (import GHC.Driver.Config.Diagnostic (initDiagOpts))
     (import GHC.Types.Error (Diagnostic Messages getMessages)))]
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

(cond-expand
  [(< 802 :ghc)
   (imports-from-ghc
    (GHC.Driver.CmdLine (Warn)))]
  [otherwise
   (import SrcLoc (Located))])

;;; Types

(type WARN
  (cond-expand
    [(< 802 :ghc) Warn]
    [otherwise (Located String)]))

;;; Functions

(defn (:: handle-flag-warnings (-> HscEnv DynFlags [WARN] (IO ())))
  [_hsc-env dflags warns]
  (cond-expand
    [(<= 904 :ghc)
     (handleFlagWarnings (hsc-logger _hsc-env) (initDiagOpts dflags) warns)]
    [(<= 902 :ghc)
     (handleFlagWarnings (hsc-logger _hsc-env) dflags warns)]
    [otherwise
     (handleFlagWarnings dflags warns)]))

(cond-expand
  [(<= 904 :ghc)
   (defn (:: ppr-wrapped-msg-bag-with-loc
           (=> (Diagnostic e) (-> (Messages e) [SDoc])))
     (. pprMsgEnvelopeBagWithLoc getMessages))]
  [otherwise
   (defn (:: ppr-wrapped-msg-bag-with-loc (-> (Bag WrappedMsg) [SDoc]))
     (cond-expand
       [(<= 902 :ghc) pprMsgEnvelopeBagWithLoc]
       [otherwise pprErrMsgBagWithLoc]))])
