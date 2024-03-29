;;; -*- mode: finkel -*-
;;;; Finkel core language macros

(:doc "Core language macros.

Macros exported from this module are available in the @finkel@ executable by
default, i.e., available without /require/-ing the \"Finkel.Core\" module.")

(module Finkel.Core
  (:dh1 "Phase control")
  eval-when eval-and-compile

  (:dh1 "Module header")
  defmodule

  (:dh1 "Macro for macros")
  defmacro defmacro' defmacro- defmacroM defmacroM' defmacroM-

  (:dh1 "Temporary macros")
  macrolet macroletM

  (:dh1 "Declaring functions")
  defn defn' defn-

  (:dh1 "Expanding macros")
  macroexpand macroexpand-1 exported-macros

  (:dh1 "Compilation context macros")
  cond-expand

  (:dh1 "Error macro")
  macro-error

  (:dh1 "Expressions")
  case-do cond heredoc lcase lefn lept)

;; Internal
(import Finkel.Core.Internal.Stage1)
(import Finkel.Core.Internal.Stage2)
