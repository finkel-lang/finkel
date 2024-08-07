;;; -*- mode: finkel -*-
;;;; Stage2 - module containing more macros and functions.

%p(LANGUAGE FlexibleInstances
            TypeSynonymInstances)

(module Finkel.Core.Internal.Stage2
  ;; Macros
  defmodule
  macroexpand macroexpand-1
  exported-macros cond-expand macro-error
  case-do cond heredoc lcase lefn lept

  ;; Functions
  is-atom is-pair is-list is-hslist is-symbol is-string is-char
  is-integer is-fractional is-unit

  make-symbol mb-symbol-name mb-symbol-name-fs

  list (Listable ..)

  caar cadr
  caaar caadr cadar caddr
  caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr

  cdar cddr
  cdaar cdadr cddar cdddr
  cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr

  map1 reduce reduce1 keep curve rev unsnoc trav1 omni omniM

  caris

  unsafeFinkelSrcError)

;; base
(import Control.Exception (throw))
(import Control.Monad (>=> foldM))
(import Data.Version ((Version ..)))
(import qualified System.Info)

;; finkel-kernel
(import Language.Finkel)
(import Language.Finkel.Form (mkLocatedForm aSymbol genSrc))

;; Internal
(import Finkel.Core.Internal.Stage0)
(import Finkel.Core.Internal.Ghc.Version)
(import Finkel.Core.Internal.Ghc.Compat)

(:require Finkel.Core.Internal.Stage1)

(eval-when [:compile]
  ;; base
  (import Prelude)
  ;; finkel-kernel
  (import Language.Finkel)
  (import Language.Finkel.Form (aSymbol genSrc)))


;;; ------------------------------------------------------------------------
;;;
;;; Macros
;;;
;;; ------------------------------------------------------------------------

(defmacro macro-error
  "Macro for showing error in macro function. Shows an error message
with the location of entire macro form.

==== __Syntax__

> MACRO-ERROR ::= 'macro-error' string

==== __Example__

Show error with non-integer literals:

> (defmacro e1 [a]
>   (case (fromCode a)
>     (Just n) (toCode (:: (+ n 1) Int))
>     _ (macro-error \"got non-integer literal\")))

Sample runs:

>>> (macroexpand '(e1 9))
10
>>> (macroexpand '(e1 foo))
<interactive>:2:15: error: got non-integer literal
"
  [msg]
  `(unsafeFinkelSrcError ,the-macro-arg ,msg))

;;; Expression

(defmacro cond
  "The 'cond' macro, found in many Lisp languages. The behaviour is same as
wrapping the body with @case@ expression with dummy unit, or @if@ with
@MultiWayIf@ GHC language extension.

==== __Syntax__

> COND   ::= 'cond' CLAUSE+
> CLAUSE ::= '(' guard+ expr ')' | '[' guard+ expr ']'

==== __Examples__

Simple function with weird tests:

@
(defn (:: c01 (-> Int Int Int))
  [a b]
  (cond
    [(even b) b]
    [(odd b) (> a b) b]
    [otherwise a]))
@

Sample run:

>>> (map (c01 5) [1 .. 10])
[1,2,3,4,5,6,5,8,5,10]
"
  body
  `(case ()
     _ ,(cons '| (map1 curve body))))

(defmacroM lcase
  "Same as @\\\\case@ enabled with the @LambdaCase@ extension.

==== __Syntax __

> LCASE    ::= PAT-EXPR+
> PAT-EXPR ::= PATTERN EXPR

==== __Example__

>>> (map (lcase 0 \"zero\" 1 \"one\" _ \"many\") [0 1 2 3]
[\"zero\", \"one\", \"many\",\"many\"]
"
  args
  (do (<- tmp (gensym' "lcasearg"))
      (pure `(\,tmp
               ,(cons 'case (cons tmp args))))))

(defmacroM case-do
  "Like @case@, but takes an expression with 'Monad' type.

==== __Syntax__

> CASE-DO  ::= 'case-do' EXPR PAT-EXPR+
> PAT-EXPR ::= PATTERN EXPR

==== __Example__

Following code:

@
(case-do getLine
  \"hello\" (putStrLn \"Hi!\")
  line (putStrLn (++ \"Got: \" line)))
@

Is same as:

@
(do (<- tmp getLine)
    (case tmp
      \"hello\" (putStrLn \"Hi!\")
      line (putStrLn (++ \"Got: \" line))))
@
"
  args
  (do (<- tmp gensym)
      (return `(do (<- ,tmp ,(car args))
                   (case ,tmp ,@(cdr args))))))

(defmacro lefn
  "Let-fn macro. Like @let@, but bindings take forms used in
'Finkel.Core.Stage1.defn'.

==== __Syntax__

> LEFN     ::= 'lefn' BINDINGS BODY
> BINDINGS ::= '[' BINDING* ']' | '(' BINDING* ')'
> BINDING  ::= '(' VAR [ARGS] expr ')' | SIG
> VAR      ::= varid | SIG
> SIG      ::= '(' '::' varid type ')'
> ARGS     ::= '[' varid* ']' | '(' varid* ')'

==== __Examples__

Following expresion:

@
(lefn [(x 100)
       (:: f (-> Int Int Int))
       (f [a b]
         (+ (* a b) 2))
       ((:: g (-> Int Int))
         [0] 0
         [n] (+ n 1))]
  (g (f x 3)))
@

expands to:

@
(let ((= x 100)
      (:: f (-> Int Int Int))
      (= f a b
        (+ (* a b) 2))
      (:: g (-> Int Int))
      (= g 0 0)
      (= g n (+ n 1)))
  (g (f x 3)))
@"
  args
  ;; Allow empty body to support `lefn' inside `do' syntax.
  (let ((= binds0 (car args))
        (= binds1 (if (|| (is-unit binds0) (null binds0))
                    '()
                    (curve (map1 (\bind
                                   (if (caris ':: bind)
                                     bind
                                     (cons 'defn (curve bind))))
                                 binds0))))
        (= body (cdr args)))
    (cons 'let (cons binds1 body))))

(defmacro lept
  "Let-pattern macro. Like @let@, but for pattern bindings only, does not
support function bindings. Patterns and expressions are concatenated to make a
flat bindings list with even number of elements. The pattern in the bindings
list could be a varid symbol, or a type signature list form.

==== __Syntax__

> lept     ::= 'lept' BINDINGS expr
> BINDINGS ::= '[' BINDING+ ']' | '(' BINDING+ ')'
> BINDING  ::= PATTERN expr
> PATTERN  ::= varid | '(' '::' varid type ')'

==== __Example__

Following expression:

@
(lept [a 1
       (:: b Int) 2
       (, c d) (, 3 4)
       f (\\\\ w x y z
           (+ w (* x (+ y z))))]
  (f a b c d))
@

expands to:

@
(let ((= a 1)
      (:: b Int)
      (= b 2)
      (= (, c d) (, 3 4))
      (= f (\\\\ w x y z
             (+ w (* x (+ y z))))))
  (f a b c d))
@"
  args
  (let ((= body (cdr args))
        (= f x (, mb-expr acc)
          (case mb-expr
            (Just expr) (, Nothing
                           (if (caris ':: x)
                             (: x `(= ,(cadr x) ,expr) acc)
                             (: `(= ,x ,expr) acc)))
            Nothing (, (Just x) acc)))
        (= z (, Nothing []))
        (= binds ($ curve toCode snd (reduce f z) car args)))
    (cons 'let (cons binds body))))

;; Auxiliary type for `heredoc'.

(data DocElem
  (Lit String)
  (Var String))

;; Auxliary functions for `heredoc'.

;;; Note [Brace character codes]
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;;
;;; Using #'\123 for `{', and #'\125 for `}'. These use of character codes are
;;; workaround for text editor to avoid mismatching parentheses containing forms
;;; with literal `{' and `}' characters. When literal characters were handling
;;; properly, character codes could be replaced with literal braces.

(defn (:: subst (-> Code String [DocElem]))
  [orig str]
  (let ((= go acc xs
          (case xs
            [] acc
            (: #'$ #'$ #'\123 rest) (case (go acc rest)
                                      (, tmp ss) (, (: #'$ #'\123 tmp) ss))
            (: #'$ #'\123 rest) (let ((= (, var ys) (break (== #'\125) rest))
                                      (= (, lit ss) (go acc (drop 1 ys))))
                                  (if (null var)
                                    (err "empty variable")
                                    (if (null ys)
                                      (err "missing `}'")
                                      (, [] (: (Var var)
                                               (if (null lit)
                                                 ss
                                                 (: (Lit lit) ss)))))))
            (: c rest) (case (go acc rest)
                         (, tmp ss) (, (: c tmp) ss))))
        (= err (unsafeFinkelSrcError orig)))
    (case (go (, [] []) str)
      (, xs ss) (if (null xs)
                  ss
                  (: (Lit xs) ss)))))

(defn (:: doc-elem-to-code (-> DocElem Code))
  [(Lit s)] (toCode s)
  [(Var v)] (make-symbol v))

(defn (:: hdoc (-> [DocElem] Code))
  [es]
  (case es
    [] (toCode "")
    [e] (doc-elem-to-code e)
    _ `(<> ,@(map doc-elem-to-code es))))

(defn (:: heredoc-aux (-> Code String Code))
  [orig]
  (. hdoc (subst orig)))

(defmacroM heredoc
  "Macro for writing literal string value with variable replacements.

The `heredoc' macro takes single literal string parameter. The format of the
macro expanded result preserves newlines and spaces in the input string.

Additionally, special syntax @${FOO}@ could be used to embed the value of the
variable named @FOO@ in the literal string. To write literal @${@, use @$${@.

Literal strings, new lines and variables are concatenated with `<>'. This will
enable using other type than `String' which is an instance of
`Data.String.IsString' and `Semigroup' with @OverloadedStrings@ language
extension.

==== __Syntax__

> HEREDOC ::= 'heredoc' string

==== __Example__

The code:

> (defn main
>   (putStrLn (heredoc \"foo
>   bar
>     buzz\")))

will print:

> foo
>   bar
>     buzz

And the code:

> (defn main
>   (lept [foo \"FOO\"
>          bar \"BAR\"]
>     (putStrLn (heredoc \"foo is ${foo} and bar is ${bar}\"))))

will print:

> foo is FOO and bar is BAR

Using `Data.ByteString.ByteString' with @OverloadedStrings@ language extension:

> %p(LANGUAGE OverloadedStrings)
>
> (defmodule Main
>   (import
>    (qualified Data.ByteString.Char8)))
>
> (defn main
>   (lept [foo \"overloaded bytestring\"]
>     (Data.ByteString.Char8.putStrLn (heredoc \"foo is ${foo}\"))))

will print:

> foo is overloaded bytestring

"
  [form]
  (case (fromCode form)
    (Just s) (pure (heredoc-aux form s))
    _ (finkelSrcError form "not a string")))

;;;; Module header

(defmacroM defmodule
  "Macro to define header of module named /NAME/. The /IMPEXP/ are forms
for imports and exports. Imports and exports forms can appear in any
order, and starts with one of the following symbols:

[@import@]: The form elements expand to @import@ declaration.

[@import-when@]: Like @import@, but the form elements are wrapped with
@eval-when@ with given phases.

[@require@]: The form elements expand to @:require@ declaration.

[@require-and-import@]: The form elements are expanded to both
@import@ and @:require@.

[@export@]: The form elements expand to export entities.

==== __Syntax__

> DEFMODULE ::= 'defmodule' NAME IMPEXP*
> NAME      ::= modid
> IMPEXP    ::= '(' 'import' form+ ')'
>             | '(' 'import-when' phases form+ ')'
>             | '(' 'require' form+ ')'
>             | '(' 'require-and-import' form+ ')'
>             | '(' 'export' form* ')'

==== __Examples__

Sample module header:

@
(defmodule Foo
  (require (Finkel.Prelude hiding (head tail)))
  (import-when [:compile]
    (Control.Monad (foldM)))
  (import (Control.Monad (when))
          (qualified Data.ByteString as BS))
  (export foo bar buzz))
@

Expands to:

@
(:begin
  (:require Finkel.Prelude hiding (head tail))
  (module Foo foo bar buzz)
  (eval-when [:compile]
    (import Control.Monad (foldM)))
  (import Control.Monad (when))
  (import qualified Data.ByteString as BS)
  (import Control.Monad (foldM)))
@
"
  form
  (case (unCode form)
    (List [name])
    (return `(module ,name))

    (List (: name rest1))
    (do (let ((= merge-sections acc lst
                (let ((= label (car lst))
                      (= payload (cdr lst)))
                  (case ()
                    _ (| ((== label 'export)
                          (return
                           (if (null (cdr lst))
                             (: (, 'export (cons '() nil)) acc)
                             (: (, 'export payload) acc))))
                         ((|| (== label 'use)
                              (== label 'import))
                          (return
                           (: (, 'import
                                 (map1 (\es `(import ,@(curve-el es)))
                                       payload))
                              acc)))
                         ((|| (== label 'load)
                              (== label 'require))
                          (return
                           (: (, ':require (map1 (\es `(:require ,@es))
                                                 payload))
                              acc)))
                         ((|| (== label 'require-and-import)
                              (== label 'load-use))
                          (return (: (, 'require-and-import payload) acc)))
                         ((== label 'import-when)
                          (let ((= phases (car payload))
                                (= body0 (cdr payload))
                                (= body1 (map1 (\e `(import ,@(curve-el e)))
                                               body0)))
                            (return (: (, 'eval-when (cons phases body1))
                                       acc))))
                         (otherwise
                          (finkelSrcError lst
                                          "defmodule: unknown section"))))))
              (= curve-el
                (map1 curve))))
        (<- alist (foldM merge-sections [] rest1))
        (let ((= emit add-load-too header
                (let ((= e1 (maybe nil id (lookup header alist)))
                      (= e2 (maybe nil
                                   (map1 (\es `(,header ,@es)))
                                   (lookup 'require-and-import alist))))
                  (if add-load-too
                    (mappend e1 e2)
                    e1)))))
        (return
         `(:begin
            ,@(emit True ':require)

            (module ,name ,@(emit False 'export))

            ,@(let ((= evalwhens (filter (\ (, k _) (== k 'eval-when))
                                         alist))
                    (= f (, _ phases-mdls)
                      (let ((= phases (car phases-mdls))
                            (= mdls (cdr phases-mdls)))
                        `(eval-when ,phases ,@mdls))))
                (map f evalwhens))

            ,@(emit True 'import))))

    _ (finkelSrcError form "defmodule: invalid form")))


;;; Compilation context macros

(defn (:: cond-expand-aux (-> HscEnv Code (Fnk Code)))
  "Auxiliary function for `cond-expand'."
  [hsc-env]
  (let ((= f x
          (case ()
            _ (| ((== x ':ghc)
                  (pure `(:: ,__glasgow_haskell__ Int)))
                 ((== x ':os)
                  (pure `(:: ,(toCode System.Info.os) String)))
                 ((== x ':arch)
                  (pure `(:: ,(toCode System.Info.arch) String)))
                 ((caris ':min-version x)
                  (do (<- v (getPackageVersion hsc-env (cadr x)))
                      (pure `(<= (:: [,@(cddr x)] [Int])
                                 ,(versionBranch v)))))
                 (otherwise
                  (pure x))))))
    (omniM f)))

(defmacroM cond-expand
  "Macro for conditional compilation. The `cond-expand' macro has same syntax as
the `cond' macro, but the evaluation of the tests are done at compilation
time. To gather compilation context information, the tests sections in the
`cond-expand' macro uses special keywords for getting such information. These
keywords are replaced by the described values:

[@:ghc@]: An `Int' value from @__GLASGOW_HASKELL__@, which is the major version
of the @ghc@ used during the compilation.

[@:arch@]: A `String' value from `System.Info.arch'.

[@:os@]: A `String' value from `System.Info.os'.

[@(:min-version \"@PKGNAME@\" V1 V2 V3 ...)@]: An expression resulting as a
`Bool' value. Compares the version of @PKGNAME@ with the version made from
integer values @V1@, @V2@, @V3@ and so on. This keyword resembles to the
@MIN_VERSION@ CPP macro.

==== __Syntax__

See `cond'.

==== __Examples__

Simple usage to show message:

@
(defn (:: msg String)
  (cond-expand
    [(<= 810 :ghc)
     \"ghc is newer than 8.10.0\"]
    [(== 808 :ghc) (== :arch \"x86_64\") (== :os \"linux\")
     \"ghc version 8.8.x, x86_64-linux\"]
    [(:min-version \"base\" 4 0 0)
     \"base version newer than or equals to 4.0.0\"]
    [otherwise
     \"unknown\"]))
@

When compiling above with ghc version @8.8.4@ on @x86_64@ machine running
@Linux@, the second test will pass and the expanded result would be:

@
(defn (:: msg String)
  \"ghc version 8.8.x, x86_64-linux\"
@
"
  forms
  (do (<- tmp (gensym' "cond-expand-tmp"))
      (<- hsc-env getSession)
      (let ((= make-quoted-last branches
              (case (unsnoc branches)
                (, bs b) (do (<- bs2 (cond-expand-aux hsc-env bs))
                             (pure `(,bs2 ',b)))))))
      (<- body (trav1 make-quoted-last forms))
      (pure `(macrolet [(,tmp []
                          (cond ,@body))]
               (,tmp)))))


;;; Macros for macros

(defmacroM macroexpand-1
  "Expand given form if the given form is a macro, otherwise return the
given form. Note that 'macroexpand_1' and 'macroexpand' are macros, not
functions.

==== __Syntax__

> MACROEXPAND-1 ::= 'macroexpand-1' form

==== __Examples__

>>> (macroexpand-1 '(defmacrom' m1 [x] `(+ ,x 1)))
(eval_and_compile
  (defmacro m1 [x]
    (:quasiquote (+ (:unquote x) 1))))
"
  [form]
  (case (unCode form)
    (List [q x]) (| ((|| (== q ':quote) (== q ':quasiquote))
                     (do (<- expanded (expand1 x))
                         (return `',expanded))))
    _ (return form)))

(defmacroM macroexpand
  "Macro for expanding macro. This macro recursively expands all sub
forms.

==== __Syntax__

> MACROEXPAND ::= 'macroexpand' form

==== __Examples__

>>> (macroexpand '(defn (:: foo (-> Int Int Int)) [a b] (+ a (* b 2))))
(:begin
  (:: foo (-> Int Int Int))
  (= foo a b (+ a (* b 2))))
"
  [form]
  (case (unCode form)
    (List [q x]) (| ((|| (== q ':quote) (== q ':quasiquote))
                     (let ((= go expr
                             (do (<- expr' (expand1 expr))
                                 (if (== expr expr')
                                   (return `',expr)
                                   (go expr')))))
                       (go x))))
    _ (return form)))

(defmacroM exported-macros
  "Macro to return macro names exported from given module as a list of
'String'.

==== __Syntax__

> EXPORTED-MACROS ::= 'exported-macros' modid

==== __Examples__

Listing exported macros in \"Finkel.Core\" module:

>>> (exported-macros Finkel.Core)
[\"eval_when\",\"eval_and_compile\", ...]
"
  [name]
  (let ((= f hsc-env mb-thing acc
          (case mb-thing
            (Just (@ thing (AnId var)))
            (| ((isMacro hsc-env thing)
                (: (showSDoc (hsc-dflags hsc-env) (ppr (varName var))) acc)))
            _ acc))
        (= get-exported-names name-str
          (do (<- mdl (lookupModule (mkModuleName name-str) Nothing))
              (<- mb-mod-info (getModuleInfo mdl))
              (case mb-mod-info
                Nothing (return [])
                (Just mi) (do (<- mb-things
                                (mapM lookupName (modInfoExports mi)))
                              (<- hsc-env getSession)
                              (return (foldr (f hsc-env) [] mb-things))))))
        (= invalid-err
          (++ "exported-macros: got non-module name symbol `"
              (show name) "'"))
        (= toCodes (. toCode (map toCode))))
    (case (mb-symbol-name name)
      (Just name-str) (do (<- names (get-exported-names name-str))
                          (return `(:: ,(toCodes names) [String])))
      Nothing (finkelSrcError name invalid-err))))


;;; ------------------------------------------------------------------------
;;;
;;; Functions
;;;
;;; ------------------------------------------------------------------------


;;; Predicates

(defn (:: is-atom (-> Code Bool))
  "True when the argument is an 'Atom' or 'nil'.

==== __Examples__

>>> (is-atom \'foo)
True
>>> (is-atom nil)
True
>>> (is-atom '(a b c))
False
>>> (is-atom '[a b c])
False"
  [(LForm (L _ form))]
  (case form
    (Atom _)  True
    (List []) True
    _         False))
%p(INLINABLE is-atom)

(defn (:: is-pair (-> Code Bool))
  "True when the argument is a non-nil 'List'."
  [(LForm (L _ (List (: _ _))))] True
  [_] False)
%p(INLINABLE is-pair)

(macrolet [(defpred [doc name pat]
                `(:begin
                   (:doc ,doc)
                   (:: ,name (-> Code Bool))
                   (= ,name (LForm (L _ form))
                     (case form
                       ,pat True
                       _    False))))]
  (defpred "True when the argument is a `List'."
      is-list (List _))
  (defpred "True when the argument is a `HsList'."
      is-hslist (HsList _))
  (defpred "True when the argument is an `Atom' of `ASymbol'."
      is-symbol (Atom (ASymbol _)))
  (defpred "True when the argument is an `Atom' of `AString'."
      is-string (Atom (AString _ _)))
  (defpred "True when the argument is an `Atom' of `AChar'."
      is-char (Atom (AChar _ _)))
  (defpred "True when the argument is an `Atom' of `AInteger'."
      is-integer (Atom (AInteger _)))
  (defpred "True when the argument is an `Atom' of `AFractional'."
      is-fractional (Atom (AFractional _)))
  (defpred "True when the argument is an `Atom' of `AUnit'."
      is-unit (Atom AUnit)))

(defn (:: caris (-> Code Code Bool))
  "`True' when the first argument equal to the `car' of the second argument.

==== __Examples__

>>> (caris 'foo '(foo bar buzz))
True
>>> (caris 'foo '(a b c))
False
>>> (caris 'foo 'foo)
False"
  [x lst]
  (&& (is-list lst) (== (car lst) x)))
%p(INLINABLE caris)


;;; Symbol functions

(defn (:: make-symbol (-> String Code))
  "Make an `Atom' of `ASymbol' from given `String'."
  (. LForm genSrc Atom aSymbol))
%p(INLINABLE make-symbol)

(defn (:: mb-symbol-name (-> Code (Maybe String)))
  "Extract string from given symbol.

Get `Just' `String' when the argument code was an `ASymbol', otherwise
`Nothing'."
  (. (fmap unpackFS) mb-symbol-name-fs))
%p(INLINABLE mb-symbol-name)

(defn (:: mb-symbol-name-fs (-> Code (Maybe FastString)))
  "Extract `FastString' from given symbol.

Like `mb_symbol_name', but returns `FastString'"
  [form]
  (case form
    (LForm (L _ (Atom (ASymbol name)))) (Just name)
    _ Nothing))
%p(INLINABLE mb-symbol-name-fs)


;;; Constructing list

(:doc "Type class for constructing 'List' with polyvariadic function.")
(class (Listable l)
  (:: list_ (-> [Code] l)))

(instance (Listable Code)
  (= list_ xs
    (case (mkLocatedForm (reverse xs))
      (L l ys) (LForm (L l (List ys))))))

(instance (=> (Homoiconic elem) (Listable l)
              (Listable (-> elem l)))
  (= list_ acc
    (\x (list_ (: (toCode x) acc)))))

(defn (:: list (=> (Listable lst) lst))
  "Make a list from given arguments. This function can take variable number of
arguments, but requires resulting type to be a concrete type.

==== __Examples__

>>> (:: (list \'a \'b \'c) Code)
(a b c)
>>> (:: (list \'a #\'b \"c\" (:: 0xd Int)) Code)
(a #\'b \"c\" 13)"
  (list_ []))


;;; CXR

(:doc$ cxr "Rest of /cxr/ functions are composed from 'car' and 'cdr'.

E.g., definition of 'cadr' is:

> (cadr x) == (car (cdr x))

and the definition of 'cdadr' is:

> (cdadr x) == (cdr (car (cdr x)))
")

(eval-when [:compile]
  (defn (:: ads [String])
    (let ((= f (concatMap (\x [(: #'a x) (: #'d x)]))))
      (concat (take 3 (drop 1 (iterate f [[#'a] [#'d]]))))))

  (defn (:: cxr-name (-> String Code))
    [x]
    ($ LForm genSrc Atom aSymbol concat ["c" x "r"]))

  (defn (:: doc (-> String Code))
    [xs]
    (let ((= f x (++ "`c" x "r'"))
          (= g ys (foldr1 (\y acc (++ y " of " acc))
                          (map (. f pure) ys))))
      (toCode (++ "Get the " (g xs) "."))))

  (defn (:: cxr (-> String [Code]))
    [xs]
    (let ((= name (cxr-name xs)))
      (case xs
        (: hd tl) [`(:doc ,(doc xs))
                   `(:: ,name (-> Code Code))
                   `(= ,name (. ,(cxr-name [hd]) ,(cxr-name tl)))
                   `%p(INLINABLE ,name)]
        _ (error (++ "cxr: invalid arg: " xs)))))

  (defmacro cxrs []
    `(:begin ,@(concatMap cxr ads))))

(cxrs)


;;; List and HsList functions

(defn (:: make-list-fn
        (-> (-> Code b)
            (-> (-> [(LForm a)] (LForm a)) [Code] b)
            Code
            b))
  "Auxiliary function for making higher order function. Make a function taking
`SrcSpan' and a list of `Code' from given arguments."
  [g f (@ orig (LForm (L l form)))]
  (case form
    (List xs) (f (. LForm (L l) List) xs)
    (HsList xs) (f (. LForm (L l) HsList) xs)
    _ (g orig)))
%p(INLINE make-list-fn)

(defn (:: map1 (-> (-> Code Code) Code Code))
  "Like `map', but for `Code'.  @map f x@ applies the function @f@ to each
element of @x@ when the @x@ is `List' or `HsList'. Otherwise directly applies
@f@ to @x@.

==== __Examples__

>>> (map1 (\\ x `(x is ,x)) '(foo bar buzz))
((x is foo) (x is bar) (x is buzz))
>>> (map1 (\\ x `(x is ,x)) 'foo)
(x is foo)"
  [f] (make-list-fn f (\c (. c (map f)))))
%p(INLINABLE map1)

(defn (:: reduce (-> (-> Code a a) a Code a))
  "Like `foldr', but for `Code'. If the second argument was not a list, applies
the function to the second argument and the initial value.

==== __Examples__

>>> (reduce cons nil '(a b c))
(a b c)
>>> (reduce (\\ x acc (cons `(x is ,x) acc)) nil '(a b c))
((x is a) (x is b) (x is c))
>>> (reduce + 0 '(1 2 3 4 5))
15
>>> (reduce cons nil 'foo)
(foo)"
  [f z] (make-list-fn (flip f z) (\_ (foldr f z))))
%p(INLINABLE reduce)

(defn (:: reduce1 (-> (-> Code Code Code) Code Code))
  "Like `foldr1', but for `Code'. Throws an exception if the second argument
was not a list.

==== __Examples__

>>> (reduce1 cons '(a b c))
(a b c)
>>> (reduce1 (\\ x acc (cons `(x is ,x) acc)) '(a b c))
((x is a) (x is b) c)
>>> (reduce1 cons 'foo)
*** Exception: reduce1: non-list value `foo'"
  [f] (make-list-fn (non-list "reduce1") (\_ (foldr1 f))))
%p(INLINABLE reduce1)

(defn (:: keep (-> (-> Code Bool) Code Code))
  "Like `filter', but for `Code'. Filter out immediate elements of `List' if
the test result is `False'.

==== __Examples__

>>> (keep (/= 'a) '(a b r a c a d a b r a))
(b r c d b r)
>>> (keep (/= 'a) 'foo)
*** Exception: keep: non-list value `foo'"
  [test] (make-list-fn (non-list "keep") (\c (. c (filter test)))))
%p(INLINABLE keep)

(defn (:: curve (-> Code Code))
  "Convert `HsList' to `List'. The original value is retained if the given
argument was not a `HsList' value.

==== __Examples__

>>> (curve '[a b c])
(a b c)
>>> (curve 'foo)
foo"
  [(@ orig (LForm (L l form)))]
  (case form
    (HsList xs) (LForm (L l (List xs)))
    _ orig))
%p(INLINABLE curve)

(defn (:: rev (-> Code Code))
  "Like `reverse', but for `Code'. Reverse the given `List' or `HsList'. Other
values are kept as-is.

==== __Examples__

>>> (rev '(a b c))
(c b a)
>>> (rev 'foo)
foo"
  (make-list-fn id (\c (. c reverse))))
%p(INLINABLE rev)

(defn (:: unsnoc (-> Code (, Code Code)))
  "Split `List' and `HsList' to the elements but last and the last element.

==== __Examples__

>>> (unsnoc '(a b c d e))
((a b c d),e)"
  [form]
  (case (rev form)
    mrof (, (rev (cdr mrof)) (car mrof))))
%p(INLINABLE unsnoc)

(defn (:: trav1 (=> (Applicative f) (-> (-> Code (f Code)) Code (f Code))))
  "Like `traverse', but for `Code'. Applies given function to the direct element
of the given `Code'.

==== __Examples__

>>> (trav1 (\\ x (>> (print x) (pure x))) '(a (b c) d))
a
(b c)
d
(a (b c) d)
>>> (trav1 Just 'foo)
Just foo"
  [f] (make-list-fn f (\c (. (fmap c) (traverse f)))))
%p(INLINABLE trav1)

(defn (:: omni (-> (-> Code Code) Code Code))
  "Applies given function to every elements.

==== __Examples__

>>> (omni (\\ x (if (== 'foo x) 'bar x)) '(a foo (foo b c) (d foo)))
(a bar (bar b c) (d bar))
>>> (omni (\\ x (if (caris 'foo x) (cons 'bar (cdr x)) x)) '(a foo (foo b c) (d foo)))
(a foo (bar b c) (d foo))
>>> (omni (\\ x (if (== x 'foo) 'bar x)) 'foo)
bar
>>> (omni (\\ x (if (== x 'foo) 'bar x)) nil)
nil"
  [f]
  (make-list-fn f (\c (. f c (map (omni f))))))
%p(INLINABLE omni)

(defn (:: omniM (=> (Monad m) (-> (-> Code (m Code)) Code (m Code))))
  "Applies given monadic function to every elements.

==== __Examples__

>>> (omniM (\\ x (>> (print x) (return x))) '(a (b c) d))
a
b
c
(b c)
d
(a (b c) d)
(a (b c) d)
"
  [f]
  (make-list-fn f (\c (>=> (traverse (omniM f)) (. f c)))))
%p(INLINABLE omniM)

;;; Error

(defn (:: unsafeFinkelSrcError (-> Code String a))
  "Throw exception with 'FinkelSrcError', with given code and message. This
function uses `Control.Exception.throw' from the @base@ package."
  (. (fmap throw) FinkelSrcError))
