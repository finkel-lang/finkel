;;; -*- mode: finkel -*-
;;;; Auxiliary macros for test

%p(LANGUAGE TypeApplications)

(:require Finkel.Core)

(defmodule TestAux
  (export
   (GensymCode ..)
   expand-form
   expand-form-satisfies
   expand-form-with
   expand-form-with-env
   expand-form-with-package-db-satisfies
   expand-form-with-package-db-failure
   expandTo
   expandFailure
   expandFailureWith
   expandSatisfy
   expandWithPackageDbSatisfy
   expandWithPackageDbFailure)
  (import
   ;; base
   (Control.Exception [(SomeException ..) try])
   (Control.Monad [unless])
   (Data.Function [on])

   ;; hspec
   (Test.Hspec
    [Expectation expectationFailure shouldBe shouldSatisfy])

   ;; finkel-kernel
   (Language.Finkel.Make [initSessionForMake])
   (Language.Finkel.Fnk [(FnkEnv ..)])

   ;; finkel-core
   (Finkel.Prelude)
   (Finkel.Core.Internal)))

;;; Types

;; Using a newtype to compare 'Code's containing symbols generated
;; with `gensym'.
(newtype GensymCode (GensymCode Code)
  (deriving Show))

(instance (Eq GensymCode)
  (= == eqGensymCode))

(defn (:: eqGensymCode (-> GensymCode GensymCode Bool))
  [(GensymCode a) (GensymCode b)]
  (eqGensymCode1 a b))

(defn (:: eqGensymCode1 (-> Code Code Bool))
  [a b]
  (eqGensymCode2 (unCode a) (unCode b)))

(defn (:: eqGensymCode2 (-> (Form Atom) (Form Atom) Bool))
  [(Atom (@ asym (ASymbol a))) (Atom (@ bsym (ASymbol b)))]
  (| ((nullFS a) (nullFS b))
     ((<- (: #'$ _) (show asym)) True)
     ((<- (: #'$ _) (show bsym)) True)
     (otherwise (== a b)))
  [(List as) (List bs)] (eqGensymCodes as bs)
  [(HsList as) (HsList bs)] (eqGensymCodes as bs)
  [a b] (== a b))

(defn (:: eqGensymCodes (-> [Code] [Code] Bool))
  [[] []] True
  [[]  _] False
  [ _ []] False
  [(: x xs) (: y ys)] (&& (eqGensymCode1 x y) (eqGensymCodes xs ys)))


;;; Functions

(defn (:: expand-form (-> Macro Code Code Expectation))
  (expand-form-with (on shouldBe GensymCode)))

(defn (:: expand-form-satisfies (-> Macro Code (-> Code Bool) Expectation))
  [macro in-form test]
  (expand-form-with (\a _ (shouldSatisfy a test)) macro in-form nil))

(defn (:: expand-form-with (-> (-> Code Code Expectation)
                               Macro Code Code Expectation))
  (expand-form-with-env defaultFnkEnv))

(defn (:: expand-form-with-env (-> FnkEnv
                                   (-> Code Code Expectation)
                                   Macro Code Code Expectation))
  [fnk-env test macro in-form out-form]
  (lept [p (either (. expectationFailure show) (flip test out-form))]
    (expand-form-with-pre (pure ()) fnk-env p macro in-form)))

(defn (:: expand-form-with-package-db-satisfies
        (-> Macro Code (-> Code Bool) Expectation))
  [macro in-form test]
  (lept [p (either (. expectationFailure show) (flip shouldSatisfy test))]
    (expand-form-with-package-db defaultFnkEnv p macro in-form)))

(defn (:: expand-form-with-package-db-failure
        (-> Macro Code (-> SomeException Bool) Expectation))
  [macro in-form test]
  (lept [p (either (\e (unless (test e)
                         (expectationFailure "test function failed")))
                   (const (expectationFailure "no exception thrown")))]
    (expand-form-with-package-db defaultFnkEnv p macro in-form)))

(defn (:: expand-form-with-package-db
        (-> FnkEnv
            (-> (Either SomeException Code) Expectation)
            Macro Code Expectation))
  (expand-form-with-pre initSessionForMake))

(defn (:: expand-form-with-pre
        (-> (Fnk ()) FnkEnv (-> (Either SomeException Code) Expectation)
            Macro Code Expectation))
  [pre fnk-env test macro in-form]
  (>>= (try (runFnk (do pre
                        (macroFunction macro in-form))
                    fnk-env))
       test))

;;; Macros

(defmacro expandTo [in-form out]
  `(expand-form ,(car in-form) ',in-form ',out))

(defmacro expandFailureWith [in-form test]
  `(let ((= act
           (do (<- r (runFnk (macroFunction ,(car in-form) ',in-form)
                             defaultFnkEnv))
               (seq r (pure r)))))
     (shouldThrow act ,test)))

(defmacro expandFailure [in-form]
  `(expandFailureWith ,in-form anyException))

(defmacro expandSatisfy [in-form test]
  `(expand-form-satisfies ,(car in-form) ',in-form ,test))

(defmacro expandWithPackageDbSatisfy [in-form test]
  `(expand-form-with-package-db-satisfies ,(car in-form) ',in-form ,test))

(defmacro expandWithPackageDbFailure [in-form test]
  `(expand-form-with-package-db-failure ,(car in-form) ',in-form ,test))
