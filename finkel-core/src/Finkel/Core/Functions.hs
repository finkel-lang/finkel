;;; -*- mode: finkel -*-
;;;; Exported functions defined in this package

(:doc "Module for exporting functions defined in the @finkel-core@ package.

This module does not export macros, but exports some of the functions to work
with code values when writing macros.")

(module Finkel.Core.Functions
  (:dh1 "Predicates")
  is-atom is-pair is-list is-hslist
  is-symbol is-string is-char is-integer is-fractional is-unit
  caris

  (:dh1 "Atom constructors")
  make-symbol

  (:dh1 "Atom extractors")
  mb-symbol-name mb-symbol-name-fs

  (:dh1 "Code constructors")
  cons list (Listable ..)

  (:dh1 "CXrs")
  (:dh2 "Basic cXrs")
  car cdr

  (:dh2 "Composed cXrs")
  (:doc$ cxr)

  caar cadr
  caaar caadr cadar caddr
  caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr

  cdar cddr
  cdaar cdadr cddar cdddr
  cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr

  (:dh1 "Converting")
  curve rev unsnoc

  (:dh1 "Higher order functions")
  reduce reduce1 map1 keep trav1 omni omniM

  (:dh1 "Exceptions")
  (FinkelListException ..) unsafeFinkelSrcError)

;; Internal
(import Finkel.Core.Internal.Stage0)
(import Finkel.Core.Internal.Stage2)

(:doc$ cxr "The `car' and `cdr' are the basic of /cxr/ functions.
Rest of the /cxr/ functions are composed from `car' and `cdr'.

E.g., definition of `cadr' is:

> (cadr x) == (car (cdr x))

and the definition of `cdadr' is:

> (cdadr x) == (cdr (car (cdr x)))")
