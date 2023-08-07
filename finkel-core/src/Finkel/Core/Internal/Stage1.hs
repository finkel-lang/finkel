;;; -*- mode: finkel -*-
;;;; Stage1 - fundamental macros

;;; This module contains codes of some fundamental macros, such as `defmacro',
;;; `defmodule', `eval-when', `macrolet' ... etc, just enough to start writing
;;; other macros and function in `Finkel.Core.Stage2'.

(module Finkel.Core.Internal.Stage1
  defmacro defmacro' defmacro-
  defmacroM defmacroM' defmacroM-
  macrolet macroletM
  defn defn' defn-
  eval-when eval-and-compile)

;;; Imports

;; base
(import Control.Monad (foldM))

;; finkel-kernel
(import Language.Finkel)

;; Internal
(import Finkel.Core.Internal.Stage0)
(import Finkel.Core.Internal.Ghc.Compat)


;;; [Internally used macros]
;;; ~~~~~~~~~~~~~~~~~~~~~~~~
;;;
;;; First, defining `eval-when-compile-and-load' macro, to define functions and
;;; macros in current compilation context and compiled result. Then this module
;;; defines some auxiliary functions, and then a macro `define-macro'' to define
;;; macros for this module itself and compiled result.

(:eval-when-compile
  ;; base
  (import Prelude)

  ;; finkel-kernel
  (import Language.Finkel)

  (:: eval-when-compile-and-load Macro)
  (= eval-when-compile-and-load
    (Macro
     (\form
       (case (unCode form)
         (List (: _ rest)) (return `(:begin
                                      (:eval-when-compile ,@rest)
                                      ,@rest))
         _ (finkelSrcError form "eval-when-compile-and-load"))))))

(eval-when-compile-and-load
 (:doc "Code transformer function for macro declaration.")
 (:: macro-decl (-> Code Code Code (Fnk Code)))
 (= macro-decl name arg body
   (do (<- tmp (gensym' "tmp"))
       ;; XXX: Test the behaviour when the type signature in below is removed.
       (return `(= ,name
                  (let ((:: ,tmp (-> Code (Fnk Code)))
                        (= ,tmp ,arg ,body))
                    (Macro ,tmp))))))
 %p(INLINABLE macro-decl)

 (:doc "Body function of /macro-defining-macro/.")
 (:: dmac (-> Code (Fnk Code)))
 (= dmac form
   (let ((:: make-tsig (-> Code Code))
         (= make-tsig name `(:: ,name Macro)))
     (case (unCode form)
       (List [_ name arg body])
       (do (<- decl (macro-decl name arg body))
           (return `(:begin
                      ,(make-tsig name)
                      ,decl)))

       (List [_ name (@ doc (LForm (L _ (Atom (AString {}))))) arg body])
       (do (<- decl (macro-decl name arg body))
           (return `(:begin
                      ,(make-tsig name)
                      (:doc^ ,doc)
                      ,decl)))

       _ (finkelSrcError form "dmac: malformed macro"))))
 %p(INLINABLE dmac)

 (:doc "Function for body of `macrolet'.")
 (:: lmac (-> Code (Fnk Code)))
 (= lmac form
   (let ((= f x
           (case (unCode x)
             (List [name arg body]) (macro-decl name arg body)
             _ (finkelSrcError x "lmac: malformed macro"))))
     (case (unCode form)
       (List (: _ (LForm (L _l (List ms))) rest))
       (do (<- ms' (mapM f ms))
           (return `(:with-macro (,@ms') ,@rest)))
       _ (finkelSrcError form "lmac: malformed args"))))
 %p(INLINABLE lmac))

(:eval-when-compile
  (:doc "Simple macro for defining macro. This macro is used internally
in \"Finkel.Core\" module to define other macros.

==== __Syntax__

> DEFINE-MACRO ::= 'define-macro' NAME [DOC] ARGS BODY
> NAME         ::= varid
> DOC          ::= '\"' comment '\"'
> ARGS         ::= varid | '[' varid* ']' | '(' varid* ')'
> BODY         ::= form

==== __Examples__

Without documentation comment:

@
(define-macro m1a
  form
  (case (unCode form)
    (List [_ x]) (return `(print ,x))))
@

With documentation comment:

@
(define-macro m1b
  \"Documentation comment\"
  form
  (case (unCode form)
    (List [_ x]) (return `(print ,x))))
@
")
  (:: define-macro Macro)
  (= define-macro (Macro dmac)))


;;; Exported codes

(:doc "Macro to specify the /PHASE/s of evaluation of /BODY/ codes. Valid phases
are __@:compile@__ and __@:load@__. The @:compile@ phase evaluates the body
forms at the time of compilation, so that the compilation context can refer to
the function and macros definied in the /BODY/ forms. The @:load@ phase simply
emit the body forms to compiled result.

==== __Syntax__

> EVAL-WHEN ::= 'eval-when' PHASES BODY+
> PHASES    ::= '[' PHASE+ ']' | '(' PHASE+ ')'
> PHASE     ::= ':compile' | ':load'
> BODY      ::= form

==== __Example__

In following module, the function @f@ is defined inside 'eval_when', so that the
function could be called from the function @g@ at run-time, and temporally macro
@m@ at compilation time.

@
(defmodule Main
  (import-when [:compile :load]
    (Finkel.Prelude)))

(eval-when [:compile :load]
  (defn (:: f (-> Code Code))
    [x]
    `(print ,x)))

(defn (:: g (-> Code (IO ())))
  [x]
  (print (f x)))

(macrolet ((m [x]
             (f x)))
  (defn (:: main (IO ()))
    (do (g 'foo)
        (m 'bar))))
@
")
(define-macro eval-when form
  (let ((:: at-compile (-> [Code] Bool))
        (= at-compile (elem ':compile))
        (:: at-load (-> [Code] Bool))
        (= at-load (elem ':load))
        (:: emit (-> [Code] [Code] (Fnk Code)))
        (= emit phases body
          (| ((&& (at-compile phases) (at-load phases))
              (do (<- expanded (expands body))
                  (return
                   (cons ':begin
                         (cons (cons ':eval-when-compile expanded)
                               expanded)))))
             ((at-compile phases)
              (return `(:eval-when-compile ,@body)))
             ((at-load phases)
              (return `(:begin ,@body)))
             (otherwise
              (finkelSrcError form (++ "eval-when: invalid phase: "
                                       (show phases)))))))
    (case (unCode form)
      (List (: _ (LForm (L _ lst)) body))
      (| ((<- (List phases) lst) (emit phases body))
         ((<- (HsList phases) lst) (emit phases body)))
      _ (finkelSrcError form (++ "eval-when: invalid form: " (show form))))))

(:doc "Same as 'eval_when' macro with __@:compile@__ and __@:load@__ phases.

==== __Syntax__

> EVAL-AND-COMPILE ::= 'eval-and-compile' BODY*

==== __Example__

See 'eval_when'.")
(define-macro eval-and-compile form
  (return `(eval-when [:compile :load]
             ,@(cdr form))))


;;; Auxiliary functions for `defmacro'

(:: subst-gensyms (-> [(, Atom Atom)] Code Code))
(= subst-gensyms kvs
  (fmap (\x (case (lookup x kvs)
              (Just y) y
              Nothing  x))))
%p(INLINE subst-gensyms)

(:: replace-hyphens (-> String String))
(= replace-hyphens
  (map (\x (if (== x #'-) #'_ x))))
%p(INLINE replace-hyphens)

(:: acc-gensym-names (-> [(, Atom Atom)] Atom (Fnk [(, Atom Atom)])))
(= acc-gensym-names acc form
  (case form
    (ASymbol sym) (| ((<- (: #'$ (@ cs (: c _))) (unpackFS sym))
                      (elem c [#'a .. #'z])
                      (<- Nothing (lookup form acc))
                      (do (<- x (gensym' (replace-hyphens cs)))
                          (return (case (unCode x)
                                    (Atom gsym) (: (, form gsym) acc)
                                    _ acc)))))
    _ (return acc)))
%p(INLINE acc-gensym-names)

(:: gensymfy (-> Code (Fnk Code)))
(= gensymfy form
  (do (<- kvs (foldM acc-gensym-names [] form))
      (return (subst-gensyms kvs form))))
%p(INLINE gensymfy)

;; Function to make body of macro.
;;
;; XXX: Currently does not suuport lambda-list like pattern match in
;; macro argument.
(:: make-macro-body (-> String Code Code Code Code (Fnk Code)))
(= make-macro-body label whole name0 arg0 body0
  (let ((:: err (-> Code Code Code))
        (= err form-name name
          `(finkelSrcError
            ,form-name
            (++ ,(++ "in macro `" (show name) "'\ninvalid form: `")
                (show ,the-macro-arg) "'")))

        (:: atom-arg-body (-> Code Code Code Code Code))
        (= atom-arg-body name arg body form-name
          `(case ,form-name
             (LForm (L $loc (List (: _ __arg__))))
             (let ((= ,arg (LForm (L $loc (List __arg__))))
                   (= $tmp ,body))
               $tmp)
             _ ,(err form-name name)))

        (:: list-arg-body (-> SrcSpan Code [Code] Code Code Code))
        (= list-arg-body l1 name args body form-name
          (let ((:: abind Code)
                (= abind
                  `(LForm (L _loc
                             (List [_ ,@(LForm (L l1 (List args)))])))))
            `(case ,form-name
               ,abind (let ((= $tmp ,body))
                        $tmp)
               _ ,(err form-name name)))))

    (case (unLForm arg0)
      (L l1 (Atom AUnit))
      (gensymfy (list-arg-body l1 name0 [] body0 the-macro-arg))

      (L l1 (List args))
      (gensymfy (list-arg-body l1 name0 args body0 the-macro-arg))

      (L l1 (HsList args))
      (gensymfy (list-arg-body l1 name0 args body0 the-macro-arg))

      (L _ (Atom (ASymbol _)))
      (gensymfy (atom-arg-body name0 arg0 body0 the-macro-arg))

      _ (finkelSrcError whole (++ label ": invalid args")))))
%p(INLINE make-macro-body)

;; Function to make body expression of `defmacroM' and `defmacro'.
(:: make-defmacro-body (-> String Code (-> Code Code) (Fnk Code)))
(= make-defmacro-body label whole f
  (let ((:: emit (-> Code (Maybe Code) Code Code (Fnk Code)))
        (= emit name mb-doc arg body0
          (do (let ((= body1 (f body0))
                    (= docs (maybe [] pure mb-doc))))
              (<- body2 (make-macro-body label whole name arg body1))
              (dmac `(_ ,name ,@docs ,the-macro-arg ,body2)))))
    (case (unCode whole)
      (List [_ name (@ doc (LForm (L _ (Atom (AString {}))))) arg body])
      (emit name (Just doc) arg body)

      (List [_ name arg body])
      (emit name Nothing arg body)

      _ (finkelSrcError whole (++ label ": invalid form")))))
%p(INLINE make-defmacro-body)

;; Function to make body expression of `macrolet-m' and `macrolet'.
(:: make-macrolet-body (-> String Code (-> Code Code) (Fnk Code)))
(= make-macrolet-body label whole f
  (let ((:: make-macro (-> Code (Fnk Code)))
        (= make-macro code
          (case code
            (LForm (L l (List [name arg body0])))
            (do (let ((= body1 (f body0))))
                (<- body2 (make-macro-body label code name arg body1))
                (return (LForm (L l (List [name the-macro-arg body2])))))

            _ (finkelSrcError code (++ label ": invalid form"))))
        (:: emit (-> SrcSpan [Code] [Code] (Fnk Code)))
        (= emit l ms body
          (do (<- macros (mapM make-macro ms))
              (lmac `(:with-macro ,(LForm (L l (List macros)))
                       ,@body)))))

    (case (unCode whole)
      (List (: _ (LForm (L l macs)) rest))
      (| ((<- (List forms) macs) (emit l forms rest))
         ((<- (HsList forms) macs) (emit l forms rest)))

      _ (finkelSrcError whole (++ label ": invalid form")))))
%p(INLINE make-macrolet-body)

(:doc "Variant of 'macrolet', the body of each macro need to be a 'Code' value
wrapped in 'Fnk'. This macro has full access to 'Fnk' in compilation context.

==== __Syntax__

See 'macrolet'.

==== __Example__

Rewrite of the example shown in 'macrolet':

@
(macrolet-m ((m1 [x]
               (return `(+ ,x 1)))
             (m2 [a b]
               (return `[(m1 ,a) (m1 ,b)])))
  (m2 19 20))
;;; ==> [20,21]
@
")
(define-macro macroletM form
  (make-macrolet-body "macroletM" form id))

(:doc "Define temporary macros named /NAME/. The defined macros could be
referred from /BODY/. Each macro takes /ARGS/ parameter, and results in
/EXPR/. The parameter /ARGS/ works as in 'defmacro'.

==== __Syntax__

> MACROLET ::= 'macrolet' '(' MACRO* ')' BODY
> MACRO    ::= NAME ARGS EXPR
> NAME     ::= varid
> ARGS     ::= '(' varid* ')' | '[' varid* ']' | varid
> EXPR     ::= form
> BODY     ::= form

==== __Examples__

Temporary macros can refer other temporary macros:

@
(macrolet ((m1 [x]
             `(+ ,x 1))
           (m2 [a b]
             `[(m1 ,a) (m1 ,b)]))
  (m2 19 20))
;;; ==> [20,21]
@
")
(define-macro macrolet form
  (make-macrolet-body "macrolet" form (\body `(return ,body))))

(:doc "A macro similar to 'defmacro', but the body expression need to be a value
of type 'Fnk' 'Code'. This macro has full access to the 'Fnk' environment in
compilation context.

==== __Syntax__

See 'defmacro'.

==== __Examples__

A macro to read a file contents during compilation:

@
(defmacroM m1 [path]
  (| ((<- (Just path') (fromCode path))
      (do (<- contents (liftIO (readFile path')))
          (return `(putStrLn ,contents))))
     (otherwise
      (finkelSrcError path \"m1: not a file path.\"))))
@

Sample expansion:

>>> (macroexpand '(m1 \"/path/to/a/file.txt\")
(putStrLn \"... contents of the file ...\") ")
(define-macro defmacroM form
  (make-defmacro-body "defmacroM" form id))

(:doc "Variant of 'defmacroM', wrapped in 'eval_and_compile'.

==== __Syntax__

See 'defmacro'.

==== __Examples__

See 'defmacro' and 'defmacroM'.")
(define-macro defmacroM' form
  (return `(eval-and-compile
             (defmacroM ,@(cdr form)))))

(:doc "Variant of 'defmacroM', wrapped in @:eval_when_compile@.

==== __Syntax__

See 'defmacro'.

==== __Examples__

See 'defmacro' and 'defmacroM'. ")

(define-macro defmacroM- form
  (return `(:eval-when-compile
             (defmacroM ,@(cdr form)))))

(:doc "Macro to define a macro named /NAME/, similar to the macro with same
name found in other Lisps, such as Common Lisp, Clojure, LFE, Hy
... etc. The 'defmacro' can take an optional /DOC/ comment string in
second parameter. Next parameter is either a list of /ARGS/, or a single
varid to refer the entire parameter as a list of 'Code's. The last
parameter is a /BODY/ expression, which need to be a value of 'Code'
type.

Note that the 'defmacro' does not add the defined macro to REPL
session. To add macros in REPL session, use 'defmacro'' or write the
macro definition inside 'eval_when'.

==== __Syntax__

> DEFMACRO ::= 'defmacro' NAME [DOC] ARGS BODY
> NAME     ::= varid
> DOC      ::= '\"' comment '\"'
> ARGS     ::= '(' varid* ')' | '[' varid* ']' | varid
> BODY     ::= form

==== __Examples__

Macro taking single parameter named /x/, returns a form with 'print'
applied to the given parameter:

> (defmacro m1a [x]
>   `(print ,x))

Sample expansion:

>>> (macroexpand '(m1a False))
(print False)

Parameters could be enclosed in parentheses or brackets:

> (defmacro m1b (x)
>   `(print ,x))

Macro with documentation comment:

> (defmacro m2
>   \"Documentation comment.\"
>   [a b]
>   `(do (print ,a)
>        (print ,b)))

Sample expansion:

>>> (macroexpand '(m2 False #'x))
(do (print False) (print #'x))

Macro taking parameter as a list of 'Code':

@
(defmacro m3 args
  (case args
    (List [a])   `(print ,a)
    (List [a b]) `(>> (print ,a) (print ,b))
    (List xs)    `(do ,@(map (\\\\ x `(print ,x)) xs))))
@

Expansions of /m3/:

>>> (macroexpand '(m3 False))
(print False)
>>> (macroexpand '(m3 False #'x))
(>> (print False) (print #'x))
>>> (macroexpand '(m3 False #'x \"bar\"))
(do (print False) (print #'x) (print \"bar\"))
")
(define-macro defmacro form
  (make-defmacro-body "defmacro" form (\x `(return ,x))))

(define-macro defmacro'
  "Variant of 'defmacro', wrapped in 'eval_and_compile'.

==== __Syntax__

See 'defmacro'.

==== __Examples__

See 'defmacro'.
"
  form
  (return `(eval-and-compile
             (defmacro ,@(cdr form)))))

(define-macro defmacro-
  "Variant of 'defmacro', wrapped in @:eval-when-compile@.

==== __Syntax__

See 'defmacro'.

==== __Examples__

See 'defmacro'."
  form
  (return `(:eval-when-compile
             (defmacro ,@(cdr form)))))

(define-macro defn
  "Macro for defining function. Supports optional function type
signature /SIG/, which could be a name symbol or a list of name symbol
and type signature form. Parameter /ARGS/ could be enclosed in
parantheses or brackets. When multiple pairs of /ARGS/ and /BODY/ were
given, does expand to function definition with argument pattern
matchings.

==== __Syntax__

> DEFN    ::= 'defn' SIG [DOC] [ARGS] BODY ARGBODY*
> SIG     ::= varid | '(' varid typesig ')' | '(' '::' varid typesig ')'
> DOC     ::= '\"' comment '\"'
> ARGS    ::= '(' varid* ')' | '[' varid* ']'
> BODY    ::= form
> ARGBODY ::= ARGS BODY

==== __Examples__

Function without arguments:

> (defn v1 42)

Function without arguments, with type signature:

> (defn (:: v2 Int) 43)

Function with arguments, type signature, and documentation comment:

@
(defn (:: fib1 (-> Int Int))
  \"Documentation comment\"
  [n]
  (case n
    0 0
    1 1
    _ (+ (fib1 (- n 1)) (fib1 (- n 2)))))
@

Function with pattern matched arguments, type signature, and
documentation comment:

@
(defn (:: fib2 (-> Int Int))
  \"Documentation comment\"
  [0] 0
  [1] 1
  [n] (+ (fib2 (- n 1)) (fib2 (- n 2))))
@

The last /fib2/ example is same as below:

@
(:: fib2 (-> Int Int))
(:doc^ \"Documentation comment\")
(= fib2 0 0)
(= fib2 1 1)
(= fib2 n (+ (fib2 (- n 1)) (fib2 (- n 2))))
@
"
  form
  (let ((:: build-decls (-> Code [Code] (Fnk [Code])))
        (= build-decls name
          (let ((= go (: args body rest)
                  (do (<- bodies (go rest))
                      (return (: `(= ,name ,@args ,body) bodies))))
                (= go [] (pure []))
                (= go _ (finkelSrcError name "defn: wrong number of forms")))
            go))
        (:: build-doc (-> (Maybe Code) Code))
        (= build-doc mb-doc
          (case mb-doc
            (Just doc) `((:doc^ ,doc))
            Nothing     nil))
        (:: is-tuple (-> FastString Bool))
        (= is-tuple (== (fsLit ",")))
        (:: is-con (-> Code Bool))
        (= is-con name
          (case (unCode name)
            (Atom (ASymbol n)) (|| (isLexCon n) (is-tuple n))
            _ False))
        (:: build-sig (-> Code Code (Maybe Code) [Code] (Fnk Code)))
        (= build-sig name ty mb-doc bodies0
          (do (<- bodies1 (build-decls name bodies0))
              (return `(:begin
                         (:: ,name ,ty)
                         ,@(build-doc mb-doc)
                         ,@bodies1))))
        (:: build-nosig (-> Code (Maybe Code) [Code] (Fnk Code)))
        (= build-nosig name mb-doc bodies0
          (let ((= go bodies
                  (case mb-doc
                    Nothing (| ((<- [body] bodies)
                                (return body)))
                    _ (return `(:begin ,@bodies ,@(build-doc mb-doc))))))
            (>>= (build-decls name bodies0) go)))
        (:: build (-> Code (Maybe Code) [Code] (Fnk Code)))
        (= build sig mb-doc bodies
          (case (unCode sig)
            (List [dc name ty]) (| ((== dc '::)
                                    (build-sig name ty mb-doc bodies)))
            (List (: name _)) (| ((is-con name)
                                  (build-nosig sig mb-doc bodies)))
            (Atom _) (build-nosig sig mb-doc bodies)
            (HsList _) (build-nosig sig mb-doc bodies)
            _ (finkelSrcError sig "defn: invalid signature"))))

    (case (unCode form)
      ;; Declaration of string without documentation need to pattern match
      ;; before declarations with documentation, to support defining plain
      ;; string value without documentation.
      (List [_ sig body])
      (build sig Nothing [nil body])

      (List [_ sig (@ doc (LForm (L _ (Atom (AString {}))))) body])
      (build sig (Just doc) [nil body])

      (List (: _ sig (@ doc (LForm (L _ (Atom (AString {}))))) rest))
      (build sig (Just doc) rest)

      (List (: _ sig arg rest))
      (build sig Nothing (: arg rest))

      _ (finkelSrcError form "defn: invalid form"))))

(define-macro defn'
  "Macro to define a function for both of compilation time and load
time. This macro uses 'eval_and_compile' and 'defn'.

==== __Syntax__

See 'defn'.

==== __Examples__

See 'defn'."
  form
  (return `(eval-and-compile
             (defn ,@(cdr form)))))

(define-macro defn-
  "Macro to define a compilation time only function. This macro uses
  @:eval-when-compile@ and 'defn'.

==== __Syntax__

See 'defn'.

==== __Examples__

See 'defn'."
  form
  (return `(:eval-when-compile
             (defn ,@(cdr form)))))
