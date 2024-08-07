;;; -*- mode: finkel -*-
;;; Module containing ghc version compatibililty macro.

(defmodule Finkel.Tool.Internal.Macro.Ghc
  (export imports-from-ghc)
  (import
   ;; base
   (Control.Exception [throw])

   ;; finkel-core
   (Finkel.Prelude)
   (Finkel.Core.Internal [__glasgow_haskell__])))

;;; Module name aliases

(data GhcModuleName
  (Pre902 Code Code)
  (Pre900 Code))

(defn (:: hscTypes GhcModuleName)
  (Pre902 'GHC.Driver.Types 'HscTypes))

(defn (:: basicTypes GhcModuleName)
  (Pre902 'GHC.Types.Basic 'BasicTypes))

(defn (:: ghc-module-name-aliases [(, Code GhcModuleName)])
  "List of `Code' and `GhcModuleName' pairs, to relate module names in latest ghc
with module names in older versions."
  [(, 'GHC (Pre900 'GHC))

   (, 'GHC.Core.FamInstEnv (Pre900 'FamInstEnv))
   (, 'GHC.Core.InstEnv (Pre900 'InstEnv))
   (, 'GHC.Core.TyCo.Rep (Pre900 'TyCoRep))

   (, 'GHC.Data.Bag (Pre900 'Bag))
   (, 'GHC.Data.FastString (Pre900 'FastString))
   (, 'GHC.Data.OrdList (Pre900 'OrdList))
   (, 'GHC.Data.StringBuffer (Pre900 'StringBuffer))

   (, 'GHC.Driver.CmdLine (Pre900 'CmdLineParser))
   (, 'GHC.Driver.Main (Pre900 'HscMain))
   (, 'GHC.Driver.Make (Pre900 'GhcMake))
   (, 'GHC.Driver.Monad (Pre900 'GhcMonad))
   (, 'GHC.Driver.Ppr (Pre902 'GHC.Utils.Outputable 'Outputable))
   (, 'GHC.Driver.Session (Pre900 'DynFlags))
   (, 'GHC.Driver.Env hscTypes)
   (, 'GHC.Driver.Errors hscTypes)

   (, 'GHC.Iface.Syntax (Pre900 'IfaceSyn))

   (, 'GHC.Parser (Pre900 'Parser))
   (, 'GHC.Parser.Lexer (Pre900 'Lexer))
   (, 'GHC.Parser.PostProcess (Pre900 'RdrHsSyn))

   (, 'GHC.Runtime.Context hscTypes)
   (, 'GHC.Runtime.Debugger (Pre900 'Debugger))
   (, 'GHC.Runtime.Eval (Pre900 'InteractiveEval))
   (, 'GHC.Runtime.Interpreter (Pre900 'GHCi))
   (, 'GHC.Runtime.Linker (Pre900 'Linker))

   (, 'GHC.Settings.Config (Pre900 'Config))

   (, 'GHC.Types.Basic (Pre900 'BasicTypes))
   (, 'GHC.Types.Fixity basicTypes)
   (, 'GHC.Types.Fixity.Env hscTypes)
   (, 'GHC.Types.Name (Pre900 'Name))
   (, 'GHC.Types.Name.Set (Pre900 'NameSet))
   (, 'GHC.Types.SrcLoc (Pre900 'SrcLoc))
   (, 'GHC.Types.SourceError hscTypes)
   (, 'GHC.Types.SourceText basicTypes)
   (, 'GHC.Types.Target hscTypes)
   (, 'GHC.Types.TyThing hscTypes)
   (, 'GHC.Types.TyThing.Ppr (Pre902 'GHC.Core.Ppr.TyThing 'PprTyThing))
   (, 'GHC.Types.Var (Pre900 'Var))

   (, 'GHC.Unit.Finder (Pre902 'GHC.Driver.Finder 'Finder))
   (, 'GHC.Unit.Module (Pre900 'Module))
   (, 'GHC.Unit.Module.Graph hscTypes)
   (, 'GHC.Unit.Module.ModSummary hscTypes)
   (, 'GHC.Unit.Home.ModInfo hscTypes)

   (, 'GHC.Utils.Error (Pre900 'ErrUtils))
   (, 'GHC.Utils.IO.Unsafe (Pre900 'FastFunctions))
   (, 'GHC.Utils.Lexeme (Pre900 'Lexeme))
   (, 'GHC.Utils.Misc (Pre900 'Util))
   (, 'GHC.Utils.Outputable (Pre900 'Outputable))])

;;; Auxiliary functions

(defn (:: rename-ghc-module (-> Code Code))
  [name]
  (lefn [(legacy [m1 m2]
           (if (<= 900 __glasgow_haskell__) m1 m2))
         (reloc (flip asLocOf name))
         (msg (++ "Could not find module ‘" (show name) "’"))]
    (case (lookup name ghc-module-name-aliases)
      _ (| ((<= 902 __glasgow_haskell__) name))
      (Just (Pre902 m900 m8xx)) (legacy (reloc m900) (reloc m8xx))
      (Just (Pre900 m8xx)) (legacy name (reloc m8xx))
      _ (throw (FinkelSrcError name msg)))))

(defn (:: make-import (-> Code Code))
  [form]
  `(import ,(rename-ghc-module (car form)) ,(curve (cadr form))))

;;; Exported macro

(defmacro imports-from-ghc
  "Macro for version compatible import declaration for @ghc@.

Expects module names in latest released @ghc@, returns old module name as
necessary. Takes multiple modules in single form.

This macro is for internal use. The covered modules are used by @finkel-core@
and @finkel-tool@ packages."
  form
  `(:begin
     ,@(map1 make-import form)))
