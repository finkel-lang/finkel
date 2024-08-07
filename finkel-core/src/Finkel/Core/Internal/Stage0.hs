;;; -*- mode: finkel -*-
;;;; Stage0 - functions used in stage 1.

;;; This module contains few functions and type to work with `Code', just enough
;;; to define some macros in the next `Finkel.Core.Internal.Stage1' module.

(module Finkel.Core.Internal.Stage0
  ;; Exported from other modules.
  (FinkelListException ..) car cdr cons

  ;; Internal, used in this package only.
  non-list the-macro-arg)

;; base
(import Control.Exception ((Exception ..) throw))

;; finkel-kernel
(import Language.Finkel)
(import Language.Finkel.Form (mkLocatedForm))


;;; Exception

(:doc "List related exception.")
(data FinkelListException
  (NonListValue Code String))

(instance (Show FinkelListException)
  (= show (NonListValue _ str) str))

(instance (Exception FinkelListException))

(:doc "Throw a `NonListValue' with label and cause.")
(:: non-list (-> String Code a))
(= non-list fname what
  (let ((= msg (++ fname ": non-list value `" (show what) "'")))
    (throw (NonListValue what msg))))
%p(INLINE non-list)


;;; CONS, CAR, CDR

(:doc "Extend the second argument with the first argument by appending
to the tip.

Consing to 'HsList' will result in 'List', and consing to non-list value
will create a new 'List' instead of a /dotted-pair/.

==== __Examples__

>>> (cons 'a '(b c))
(a b c)
>>> (cons 'a '[b c])
(a b c)
>>> (cons '(a b) '(c d))
((a b) c d)
>>> (cons '[a b] '[c d])
([a b] c d)
>>> (cons 'a 'b)
(a b)")
(:: cons (=> (Homoiconic a) (Homoiconic b) (-> a b Code)))
(= cons a b
  (let ((= (@ hd (LForm (L l0 _))) (toCode a))
        (= (@ whole (LForm (L _ xs))) (toCode b)))
    (LForm (L l0 (case xs
                   (List xs')   (List (: hd xs'))
                   (HsList xs') (List (: hd xs'))
                   _            (List [hd whole]))))))
%p(INLINABLE cons)

(:doc "Get the first element of given 'Code'.

The function 'car' returns the first element of 'List' and 'HsList' constructor,
or 'nil' value when the 'List' or 'HsList' were empty.  Throws a 'NonListValue'
when the given argument was non-list value.

==== __Examples__

>>> (car '(a b c))
a
>>> (car '[a b c])
a
>>> (car nil)
nil
>>> (car 'foo)
*** Exception: car: non-list value `foo'")
(:: car (-> Code Code))
(= car (@ lform (LForm (L l form)))
  (case form
    (List (: x _))   x
    (List [])        lform
    (HsList (: x _)) x
    (HsList [])      (LForm (L l (List [])))
    _                (non-list "car" lform)))
%p(INLINABLE car)

(:doc "Get a list without the first element.

The function 'cdr' returns list value without the first element of 'List' or
'HsList' argument. When the argument is a 'HsList', returned value is converted
to a 'List'. Like 'car', throws 'NonListValue' when the argument were non-list
value.

==== __Examples__

>>> (cdr '(a b c))
(b c)
>>> (cdr '[a b c])
(b c)
>>> (cdr nil)
nil
>>> (cdr 'foo)
*** Exception: cdr: non-list value `foo'
")
(:: cdr (-> Code Code))
(= cdr (@ lform (LForm (L l form)))
  (let ((= f xs
          (case (mkLocatedForm xs)
            (L l1 _) (LForm (L l1 (List xs))))))
    (case form
      (List (: _ xs))   (f xs)
      (List [])         (LForm (L l (List [])))
      (HsList (: _ xs)) (f xs)
      (HsList [])       (LForm (L l (List [])))
      _                 (non-list "cdr" lform))))
%p(INLINABLE cdr)


;;; Special symbol

(:doc "The symbol used for entire argument in macro function.")
(:: the-macro-arg Code)
(= the-macro-arg '__form__)
%p(INLINE the-macro-arg)
