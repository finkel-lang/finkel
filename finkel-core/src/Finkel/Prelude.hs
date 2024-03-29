;;; -*- mode: finkel -*-
;;;; Prelude module for Finkel.

(:doc "Module exporting fundamental functions to work with Finkel macros.

This module does not export macros, but functions to work with `Code' values for
defining macros. Intended usage is to import during compilation:

> (defmodule MyModule
>   (import-when [:compile]
>      (Finkel.Prelude))
>   ...)

to use functions for `Code', such as `cons', `car', `cdr', and so on.")

(module Finkel.Prelude
  (:dh1 "Re-exported modules")
  (module Me))

(import Prelude as Me)               ; base
(import Language.Finkel as Me)       ; finkel-kernel
(import Finkel.Core.Functions as Me) ; Internal
