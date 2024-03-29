;;; -*- mode: finkel -*-
;;;; Module to test macros.

(:require Finkel.Core)

(defmodule CoreTest
  (export coreTests
          macroTests)
  (require
   ;; Internal
   (TestAux))
  (import-when [:compile]
    ;; Internal
    (Finkel.Prelude))
  (import
   ;; base
   (Control.Exception [(SomeException)])
   (Data.List [isSubsequenceOf])
   (System.Info [os arch])

   ;; hspec
   (Test.Hspec)

   ;; finkel-kernel
   (Language.Finkel)
   (Language.Finkel.Form [aString aIntegral])
   (Language.Finkel.Fnk [(FnkEnv ..) makeEnvMacros mergeMacros])

   ;; finkel-core
   (Finkel.Core)
   (Finkel.Core.Functions [cadr])
   (Finkel.Core.Internal)

   ;; Internal
   (TestAux)))

(defn (:: coreTests Spec)
  (describe "Macro" macroTests))

(defn (:: subseqErr (-> String SomeException Bool))
  [str e]
  (isSubsequenceOf str (show e)))

(defn (:: macroTests Spec)
  (do
    (describe "eval-when"
      (do
        (it "should expand to (:begin (:eval-when-compile ...) ...)"
          (expandTo
           (eval-when (:compile :load)
             (:: foo Int)
             (= foo 42))
           (:begin
             (:eval-when-compile
               (:: foo Int)
               (= foo 42))
             (:: foo Int)
             (= foo 42))))
        (it "should expand to (:eval-when-compile ...)"
          (expandTo
           (eval-when (:compile)
             (:: foo Int)
             (= foo 42))
           (:eval-when-compile
             (:: foo Int)
             (= foo 42))))
        (it "should expand to (:begin ...)"
          (expandTo
           (eval-when (:load)
             (:: foo Int)
             (= foo 42))
           (:begin
             (:: foo Int)
             (= foo 42))))
        (it "should support phases in bracket"
          (expandTo
           (eval-when [:compile :load]
             (= foo True))
           (:begin
             (:eval-when-compile
               (= foo True))
             (= foo True))))
        (it "throws an exception with unknown phase"
          (expandFailureWith
           (eval-when (:foo :bar :buzz)
             (:: foo Int)
             (= foo 42))
           (subseqErr "invalid phase")))
        (it "throws and exception on non-list phase"
          (expandFailureWith
           (eval-when :compile
             (:: foo Int)
             (= foo 42))
           (subseqErr "eval-when")))))

    (describe "eval-and-compile"
      (it "should expand to (eval-when ...)"
        (expandTo
         (eval-and-compile
           (:: foo Int)
           (= foo 42))
         (eval-when [:compile :load]
           (:: foo Int)
           (= foo 42)))))

    (describe "defmacroM"
      (do (it "should expand to Macro"
            (expandTo
             (defmacroM m1 (a)
               (return `(putStrLn (++ "hello, " ,a))))
             (:begin
               (:: m1 Macro)
               (= m1
                 (let ((:: $tmp (-> Code (Fnk Code)))
                       (= $tmp __form__
                         (case __form__
                           (LForm (L _loc (List [_ a])))
                           (let ((= $tmp
                                   (return
                                    (:quasiquote
                                     (putStrLn
                                      (++ "hello, " (:unquote a)))))))
                             $tmp)
                           _ (finkelSrcError
                              __form__
                              (++ "in macro `m1'\ninvalid form: `"
                                  (show __form__) "'")))))
                   (Macro $tmp))))))
          (it "should replace nil arg with _"
            (expandTo
             (defmacroM m1 ()
               (return `(print True)))
             (:begin
               (:: m1 Macro)
               (= m1
                 (let ((:: $tmp (-> Code (Fnk Code)))
                       (= $tmp __form__
                         (case __form__
                           (LForm (L _loc (List [_])))
                           (let ((= $tmp
                                   (return (:quasiquote (print True)))))
                             $tmp)
                           _ (finkelSrcError
                              __form__
                              (++ "in macro `m1'\ninvalid form: `"
                                  (show __form__) "'")))))
                   (Macro $tmp))))))
          (it "should expand symbol arg to rest"
            (expandTo
             (defmacroM m1 args
               (return `(print ,@args)))
             (:begin
               (:: m1 Macro)
               (= m1
                 (let ((:: $tmp (-> Code (Fnk Code)))
                       (= $tmp __form__
                         (case __form__
                           (LForm (L $loc (List (: _ __arg__))))
                           (let ((= args
                                   (LForm (L $loc (List __arg__))))
                                 (= $tmp (return
                                          (:quasiquote
                                           (print
                                            (:unquote-splice args))))))
                             $tmp)
                           _ (finkelSrcError
                              __form__
                              (++ "in macro `m1'\ninvalid form: `"
                                  (show __form__) "'")))))
                   (Macro $tmp))))))
          (it "should allow names with operator symbol"
            (expandTo
             (defmacroM $$$ ()
               (return `(print True)))
             (:begin
               (:: $$$ Macro)
               (= $$$
                 (let ((:: $tmp (-> Code (Fnk Code)))
                       (= $tmp __form__
                         (case __form__
                           (LForm (L _loc (List [_])))
                           (let ((= $tmp
                                   (return (:quasiquote (print True)))))
                             $tmp)
                           _ (finkelSrcError
                              __form__
                              (++ "in macro `$$$'\ninvalid form: `"
                                  (show __form__) "'")))))
                   (Macro $tmp))))))
          (it "should replace gensym name with hyphens"
            (expandTo
             (defmacroM m1 [a]
               (return `(let ((= $b-c-d (* ,a 2)))
                          (+ $b-c-d $b-c-d))))
             (:begin
               (:: m1 Macro)
               (= m1
                 (let ((:: $tmp (-> Code (Fnk Code)))
                       (= $tmp __form__
                         (case __form__
                           (LForm (L _loc (List [_ a])))
                           (let ((= $tmp
                                   (return (:quasiquote
                                            (let ((= $b_c_d
                                                    (* (:unquote a) 2)))
                                              (+ $b_c_d $b_c_d))))))
                             $tmp)
                           _ (finkelSrcError
                              __form__
                              (++ "in macro `m1'\ninvalid form: `"
                                  (show __form__) "'")))))
                   (Macro $tmp))))))
          (it "should not replace operator starting with `$'"
            (expandTo
             (defmacroM m1 [a]
               (return `($$ print show ,a)))
             (:begin
               (:: m1 Macro)
               (= m1
                 (let ((:: $tmp (-> Code (Fnk Code)))
                       (= $tmp __form__
                         (case __form__
                           (LForm (L _loc (List [_ a])))
                           (let ((= $tmp
                                   (return
                                    (:quasiquote
                                     ($$ print show (:unquote a))))))
                             $tmp)
                           _ (finkelSrcError
                              __form__
                              (++ "in macro `m1'\ninvalid form: `"
                                  (show __form__) "'")))))
                   (Macro $tmp))))))
          (it "should expand documentation arg"
            (expandTo
             (defmacroM m1 "doccomments" [] (return '(print True)))
             (:begin
               (:: m1 Macro)
               (:doc^ "doccomments")
               (= m1
                 (let ((:: $tmp (-> Code (Fnk Code)))
                       (= $tmp __form__
                         (case __form__
                           (LForm (L _loc (List [_])))
                           (let ((= $tmp
                                   (return (:quote (print True)))))
                             $tmp)
                           _ (finkelSrcError
                              __form__
                              (++ "in macro `m1'\ninvalid form: `"
                                  (show __form__) "'")))))
                   (Macro $tmp_12Jw))))))
          (it "should detect invalid arg"
            (expandFailureWith
             (defmacroM m1 "string"
               (return 'True))
             (subseqErr "defmacroM")))
          (it "should detect invalid form"
            (expandFailure
             (defmacroM m1 too many parameters passed)))
          (it "should detect invalid doc arg"
            (expandFailure
             (defmacroM m1 [a b] [c d] `[,a ,b ,c ,d])))))

    (describe "defmacroM'"
      (it "expands to (eval-when (...) (defmacroM ...))"
        (expandTo
         (defmacroM' m1 (a)
           `(return (putStrLn ,a)))
         (eval-and-compile
           (defmacroM m1 (a)
             (:quasiquote (return (putStrLn (:unquote a)))))))))

    (describe "defmacroM-"
      (it "expands to (:eval-when-compile (defmacroM ...)"
        (expandTo
         (defmacroM- m1 [a]
           (return `(putStrLn ,a)))
         (:eval-when-compile
           (defmacroM m1 [a]
             (return (:quasiquote (putStrLn (:unquote a)))))))))

    (describe "defmacro"
      (do
        (it "should expand to defmacroM"
          (expandTo
           (defmacro m1 (a)
             `(putStrLn (++ "hello, " ,a)))
           (:begin
             (:: m1 Macro)
             (= m1
               (let ((:: $tmp (-> Code (Fnk Code)))
                     (= $tmp __form__
                       (case __form__
                         (LForm (L _loc (List [_ a])))
                         (let ((= $tmp
                                 (return
                                  (:quasiquote
                                   (putStrLn (++ "hello, " (:unquote a)))))))
                           $tmp)
                         _ (finkelSrcError
                            __form__
                            (++ "in macro `m1'\ninvalid form: `"
                                (show __form__) "'")))))
                 (Macro $tmp))))))
        (it "should fail with invalid form"
          (expandFailureWith
           (defmacro m1 [arg1 arg2]
             too many body forms)
           (subseqErr "defmacro")))))

    (describe "defmacro'"
      (it "expands to (eval-and-compile (...) (defmacro ...))"
        (expandTo
         (defmacro' m1 (a)
           `(putStrLn ,a))
         (eval-and-compile
           (defmacro m1 (a)
             (:quasiquote (putStrLn (:unquote a))))))))

    (describe "defmacro-"
      (it "expands to (:eval-when-compile (...) (defmacro ...))"
        (expandTo
         (defmacro- m1 (a)
           `(putStrLn ,a))
         (:eval-when-compile
           (defmacro m1 (a)
             (:quasiquote (putStrLn (:unquote a))))))))

    (describe "macro containing `$foo'"
      (it "should replace `$foo' with gensym"
        (do (let ((= f code
                    (runFnk (macroFunction defmacroM code)
                            defaultFnkEnv))))
            (<- e (f '(defmacroM m1 (a b)
                       (let ((= $foo (+ ,a ,b)))
                         (return $foo)))))
            (shouldNotBe (elem (ASymbol (fsLit "$foo")) e)
                         True))))

    (describe "macroletM"
      (do
        (it "should expand to :with-macro"
          (expandTo
           (macroletM [(m1 [a b]
                         (return `(+ ,a ,b)))]
             (m1 20 22))
           (:with-macro
               ((= m1
                  (let ((:: $m1 (-> Code (Fnk Code)))
                        (= $m1 __form__
                          (case __form__
                            (LForm (L _loc (List [_ a b])))
                            (let ((= $tmp
                                    (return
                                     (:quasiquote
                                      (+ (:unquote a) (:unquote b))))))
                              $tmp)
                            _ (finkelSrcError
                               __form__
                               (++ "in macro `m1'\ninvalid form: `"
                                   (show __form__) "'")))))
                    (Macro $m1))))
             (m1 20 22))))
        (it "should replace () arg with _"
          (expandTo
           (macroletM ((m1 ()
                         (return `(print #'x))))
             (m1))
           (:with-macro ((= m1
                           (let ((:: $m1 (-> Code (Fnk Code)))
                                 (= $m1 __form__
                                   (case __form__
                                     (LForm (L _loc (List [_])))
                                     (let ((= $tmp
                                             (return
                                              (:quasiquote (print #'x)))))
                                       $tmp)
                                     _ (finkelSrcError
                                        __form__
                                        (++ "in macro `m1'\ninvalid form: `"
                                            (show __form__) "'")))))
                             (Macro $m1))))
             (m1))))
        (it "should detect invalid form"
          (expandFailureWith
           (macroletM)
           (subseqErr "macroletM")))
        (it "should detect invalid local macro form"
          (expandFailureWith
           (macroletM ((m1 ()
                         foo bar buzz))
             (m1))
           (subseqErr "invalid form")))))

    (describe "macrolet"
      (do
        (it "should expand to macro with `return'"
          (expandTo
           (macrolet ((m (a b)
                        `(+ ,a ,b)))
             (m 20 22))
           (:with-macro
               ((= m (let ((:: $m (-> Code (Fnk Code)))
                           (= $m __form__
                             (case __form__
                               (LForm (L _loc (List [_ a b])))
                               (let ((= $tmp
                                       (return
                                        (:quasiquote (+ (:unquote a)
                                                        (:unquote b))))))
                                 $tmp)
                               _ (finkelSrcError
                                  __form__
                                  (++ "in macro `m'\ninvalid form: `"
                                      (show __form__) "'")))))
                       (Macro $m))))
             (m 20 22))))
        (it "should detect invalid form"
          (expandFailureWith
           (macrolet)
           (subseqErr "macrolet")))))

    (describe "macro-error"
      (it "should expand to (unsafeFinkelSrcError ...)"
        (expandTo
         (macro-error "message")
         (unsafeFinkelSrcError __form__ "message"))))

    (describe "defn"
      (do
        (it "should expand to function declaration"
          (expandTo
           (defn foo (a b) (+ a b))
           (= foo a b (+ a b))))
        (it "should expand to function with type signature"
          (expandTo
           (defn (:: foo (-> Int Int Int)) (a b)
             (+ a b))
           (:begin
             (:: foo (-> Int Int Int))
             (= foo a b (+ a b)))))
        (it "should expand to function with no arguments"
          (expandTo
           (defn foo 42)
           (= foo 42)))
        (it "should expand to string"
          (expandTo
           (defn foo "bar")
           (= foo "bar")))
        (it "should expand to pattern match for `Just'"
          (expandTo
           (defn (Just foo) (pure True))
           (= (Just foo) (pure True))))
        (it "should expand to pattern match for list"
          (expandTo
           (defn [a b c] ["foo" "bar" "buzz"])
           (= [a b c] ["foo" "bar" "buzz"])))
        (it "should expand to pattern match for list with rest"
          (expandTo
           (defn (: a b c _) [1 2 ..])
           (= (: a b c _) [1 2 ..])))
        (it "should expand to pattern match for tuple"
          (expandTo
           (defn (, a b c) (, True #'x "string"))
           (= (, a b c) (, True #'x "string"))))
        (it "should expand argument patterns"
          (expandTo
           (defn foo
             [a 0] (* a 2)
             [a b] (+ a b))
           (:begin
             (= foo a 0 (* a 2))
             (= foo a b (+ a b)))))
        (it "should expand argument patterns with type signature"
          (expandTo
           (defn (:: foo (-> Int Int Int))
             [a 0] (* a 2)
             [a b] (+ a b))
           (:begin
             (:: foo (-> Int Int Int))
             (= foo a 0 (* a 2))
             (= foo a b (+ a b)))))
        (it "should expand doc without type signature"
          (expandTo
           (defn foo "doc" 42)
           (:begin
             (= foo 42)
             (:doc^ "doc"))))
        (it "should expand doc with type signature"
          (expandTo
           (defn (:: foo Int) "doc" 42)
           (:begin
             (:: foo Int)
             (:doc^ "doc")
             (= foo 42))))

        (it "should detect invalid form"
          (expandFailureWith
           (defn foo)
           (subseqErr "defn")))
        (it "should fail on invalid signature"
          (expandFailureWith
           (defn (foo (Int) (Int)) (a b)
             (+ a b))
           (subseqErr "invalid signature")))
        (it "should fail on odd number of body forms"
          (expandFailureWith
           (defn (:: foo (-> Int Int))
             0 1
             2 3
             4)
           (subseqErr "wrong number of forms")))))

    (describe "defn'"
      (it "should expand to (eval-and-compile (..) (defn ...))"
        (expandTo
         (defn' foo (a b)
           (+ a b))
         (eval-and-compile
           (defn foo (a b)
             (+ a b))))))

    (describe "defn-"
      (it "should expand to (:eval-when-compile (defn ...))"
        (expandTo
         (defn- foo [a b]
           (+ a b))
         (:eval-when-compile
           (defn foo [a b]
             (+ a b))))))

    (describe "cond"
      (it "should expand to case"
        (expandTo
         (cond [(even x) 0] [otherwise 1])
         (case () _ (| ((even x) 0) (otherwise 1))))))

    (describe "lcase"
      (it "should expand to lambda with case"
        (expandTo
         (lcase (Right a) "a" (Left b) "b")
         (\ $tmp (case $tmp (Right a) "a" (Left b) "b")))))

    (describe "case-do"
      (it "should expand to do with case"
        (expandTo
         (case-do getLine
           "hello" (putStrLn "hi")
           line (putStrLn (++ "Got: " line)))
         (do (<- $tmp getLine)
             (case $tmp
               "hello" (putStrLn "hi")
               line (putStrLn (++ "Got: " line)))))))

    (describe "heredoc"
      (do
        (it "should expand to literal string"
          (expandTo
           (heredoc "literal string")
           "literal string"))

        (it "should concat multiple lines"
          (cond-expand
            [(== :os "mingw32")
             (expandTo
              (heredoc "foo
  bar
    buzz")
              "foo\r\n  bar\r\n    buzz")]
            [otherwise
             (expandTo
              (heredoc "foo
  bar
    buzz")
              "foo\n  bar\n    buzz")]))

        (it "should preserve newline at the end"
          (cond-expand
            [(== :os "mingw32")
             (expandTo
              (heredoc "foo
")
              "foo\r\n")]
            [otherwise
             (expandTo
              (heredoc "foo
")
              "foo\n")]))
        (it "should replace variable"
          (expandTo
           (heredoc "foo is ${foo}")
           (<> "foo is " foo)))
        (it "should replace multiple variables"
          (expandTo
           (heredoc "foo=${foo} bar=${bar} buzz=${buzz}")
           (<> "foo=" foo " bar=" bar " buzz=" buzz)))
        (it "should replace variable without literal"
          (expandTo
           (heredoc "${foo}")
           foo))
        (it "should escape variable replacement"
          (expandTo
           (heredoc "escaping $${var}")
           "escaping ${var}"))
        (it "should result to empty string"
          (expandTo
           (heredoc "")
           ""))

        (it "should show error with empty variable name"
          (expandFailureWith
           (heredoc "empty var ${}")
           (subseqErr "empty variable")))
        (it "should show error with unbalanced brace"
          (expandFailureWith
           (heredoc "unbalanced ${foo")
           (subseqErr "missing")))
        (it "should show error on non-string parameters"
          (expandFailureWith
           (heredoc 42)
           (subseqErr "not a string")))))

    (describe "lefn"
      (do (it "should expand to let"
            (expandTo
             (lefn [(v1 1)
                    ((:: v2 Int) 2)
                    (:: f (-> Int Int))
                    (f
                      [0] 0
                      [1] 1
                      [n] (+ (f (- n 1)) (f (- n 2))))
                    ((:: g (-> Int Int))
                      [n]
                      (+ n 1))]
               (f (g (+ v1 v2))))
             (let ((defn v1 1)
                   (defn (:: v2 Int) 2)
                   (:: f (-> Int Int))
                   (defn f
                     [0] 0
                     [1] 1
                     [n] (+ (f (- n 1)) (f (- n 2))))
                   (defn (:: g (-> Int Int))
                     [n]
                     (+ n 1)))
               (f (g (+ v1 v2))))))
          (it "should expand to let without body"
            (expandTo
             (lefn [(a 1) (b 2)])
             (let ((defn a 1) (defn b 2)))))
          (it "should expand to let with empty binds"
            (expandTo
             (lefn [] True)
             (let () True)))
          (it "should expand to let with units"
            (expandTo
             (lefn () True)
             (let () True)))))

    (describe "lept"
      (do (it "should expand to let"
            (expandTo
             (lept [a 1 b 2 c 3]
               (+ a b c))
             (let ((= a 1) (= b 2) (= c 3))
               (+ a b c))))
          (it "should expand to let with signatures"
            (expandTo
             (lept [(:: a Int) 1 (:: b Double) 1]
               (>> (print a) (print b)))
             (let ((:: a Int)
                   (= a 1)
                   (:: b Double)
                   (= b 1))
               (>> (print a) (print b)))))))

    (describe "macroexpand-1"
      (do (it "should expand to '(toCode 3)"
            (expandTo
             (macroexpand-1 '(:quasiquote (:unquote 3)))
             '(toCode 3)))
          (it "should expand to '(toCode 4)"
            (expandTo
             (macroexpand-1 `(:quasiquote (:unquote 4)))
             '(toCode 4)))
          (it "should expand to (car nil)"
            (expandTo
             (macroexpand-1 (car nil)) (car nil)))
          (it "should expand to itself"
            (expandTo (macroexpand-1 42) 42))

          (it "should expand to form containing :begin"
            (lept [m1 (Macro (\form
                               (return `(:begin
                                          (:: foo String)
                                          (= foo ,(show (cadr form)))))))
                   my-macros (mergeMacros
                              (envMacros defaultFnkEnv)
                              (makeEnvMacros [(, "m1" m1)]))
                   my-env (defaultFnkEnv {(= envMacros my-macros)})]
              (expand-form-with-env my-env
                                    shouldBe
                                    macroexpand-1
                                    '(macroexpand-1 '(m1 True))
                                    '(:quote (:begin
                                               (:: foo String)
                                               (= foo "True"))))))))

    (describe "macroexpand"
      (do (it "should expand to '(toCode 3)"
            (expandTo
             (macroexpand '(:quasiquote (:unquote 3)))
             '(toCode 3)))
          (it "should expand to '(toCode 4)"
            (expandTo
             (macroexpand `(:quasiquote (:unquote 4)))
             '(toCode 4)))
          (it "should expand to (car nil)"
            (expandTo
             (macroexpand (car nil)) (car nil)))
          (it "should expand to itself"
            (expandTo (macroexpand 42) 42))))

    (describe "defmodule"
      (do (it "should expand to module header"
            (expandTo
             (defmodule Foo
               (export (FooClass ..) f1 f2)
               (require
                (Data.Maybe)
                (Data.List))
               (require-and-import
                (Control.Monad))
               (import (qualified Foo.Types as Types)
                       (Foo.Buzz (buzz1 buzz2))))
             (:begin
               (:require Data.Maybe)
               (:require Data.List)
               (:require Control.Monad)
               (module Foo (FooClass ..) f1 f2)
               (import qualified Foo.Types as Types)
               (import Foo.Buzz (buzz1 buzz2))
               (import Control.Monad))))
          (it "should ignore export when not given"
            (expandTo
             (defmodule Foo
               (require (Data.Maybe))
               (import (Control.Monad)))
             (:begin
               (:require Data.Maybe)
               (module Foo)
               (import Control.Monad))))
          (it "should import when compile"
            (expandTo
             (defmodule Foo
               (import-when [:compile]
                 (Prelude)
                 (Language.Finkel))
               (import
                (Control.Monad)))
             (:begin
               (module Foo)
               (eval-when [:compile]
                 (import Prelude)
                 (import Language.Finkel))
               (import Control.Monad))))
          (it "should export nothing"
            (expandTo
             (defmodule Foo
               (import (Control.Monad))
               (export))
             (:begin
               (module Foo ())
               (import Control.Monad))))
          (it "should expand to plain (module ...)"
            (expandTo
             (defmodule Foo)
             (module Foo)))
          (it "should convert [] to () for entity list"
            (expandTo
             (defmodule Foo
               (import
                (Prelude [tail head read])))
             (:begin
               (module Foo)
               (import Prelude (tail head read)))))
          (it "should fail on unknown section"
            (expandFailureWith
             (defmodule Foo
               (bar-buzz-quux
                (Control.Monad)))
             (subseqErr "unknown section")))
          (it "should fail on too few parameters"
            (expandFailure
             (defmodule)))))

    (describe "cond-expand"
      (do (it "should contain compile time information"
            (expandWithPackageDbSatisfy
             (cond-expand
               [(<= 700 :ghc) (== :arch "x86_64") (== :os "linux")
                (:min-version "base" 3 99 99)
                "ghc newer than 7.0.0, x86_64-linux, base > 3.99.99"]
               [otherwise
                "other"])
             (\form
               (&& (elem (aIntegral __glasgow_haskell__) form)
                   (elem (aString NoSourceText arch) form)
                   (elem (aString NoSourceText os) form)
                   (elem (aString NoSourceText "other") form)

                   ;; Below test works when using "base-4.x.x" package.
                   (elem (aIntegral (:: 4 Int)) form)))))

          (it "should fail with non-string package"
            (expandFailureWith
             (cond-expand
               [(:min-version 0xdeadbeaf 1 2 3)
                "3735928495"]
               [otherwise
                "No such package"])
             (\e
               (&& (subseqErr "want package name" e)
                   (subseqErr "3735928495" e)))))
          (it "should fail with non-existing package"
            (expandWithPackageDbFailure
             (cond-expand
               [(:min-version "no-such-package" 2 4 0)
                "Found no-such-package"]
               [otherwise
                "No such package"])
             (\e
               (&& (subseqErr "cannot find package" e)
                   (subseqErr "no-such-package" e)))))))))
