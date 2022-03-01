;;; -*- mode: finkel -*-
;;;; Module to test functions

(:require Finkel.Core)

(defmodule FunctionTest
  (export functionTests
          cxrTests
          listTests)
  (require
   ;; finkel-core
   (Finkel.Core.Internal))
  (import
   ;; base
   (Data.List [isSubsequenceOf])

   ;; finkel-kernel
   (Language.Finkel)

   ;; hspec
   (Test.Hspec)

   ;; quickcheck
   (Test.QuickCheck)

   ;; Internal
   (Finkel.Prelude)
   (Orphan [])))

(defn (:: functionTests Spec)
  (do (describe "Error" errorTests)
      (describe "Cxr" cxrTests)
      (describe "List" listTests)))

(defn (:: errorTests Spec)
  (describe "unsafeFinkelSrcError"
    (it "should throw src error"
      (lept [(:: srcError (-> FinkelException Bool))
             (. (isSubsequenceOf "foo") show)]
        (shouldThrow (unsafeFinkelSrcError nil "foo") srcError)))))

(defn (:: cxrTests Spec)
  (do (lept [d describe
             t 't
             tt '(t t)
             caxr (. (it "should be t") (== 't))
             cdxr (. (it "should be (t t)") (== '(t t)))])
      (d "car"
         (do (caxr (car (cons t tt)))
             (it "returns t when arg was '[t t]"
               (== (car '[t t]) t))
             (it "returns nil when arg was nil"
               (== (car nil) nil))
             (it "returns nil when arg was '[]"
               (== (car '[]) nil))
             (it "should show error when applied to non-list"
               (expectFailure (=== nil (car 'foo))))))
      (d "cdr"
         (do (cdxr (cdr (cons t tt)))
             (it "returns '(t) when arg was '[t t]"
               (== (cdr '[t t]) '(t)))
             (it "returns nil when arg was nil"
               (== (cdr nil) nil))
             (it "returns nil when arg was '[]"
               (== (cdr '[]) nil))
             (it "should show error when applied to non-list"
               (expectFailure (=== nil (cdr 'foo))))))

      (d "caar"
         (caxr (caar '((t _) _ _ _))))
      (d "cadr"
         (caxr (cadr '(_ t _ _ _))))
      (d "cdar"
         (cdxr (cdar '((_ t t) _ _ _))))
      (d "cddr"
         (cdxr (cddr '((_ _ _) _ t t))))

      (d "caaar"
         (caxr (caaar '(((t _) _ _) _ _ _))))
      (d "caadr"
         (caxr (caadr '(_ (t _ _) _ _))))
      (d "cadar"
         (caxr (cadar '((_ t _) _ _ _))))

      (d "caddr"
         (caxr (caddr '(_ _ t _))))
      (d "cdaar"
         (cdxr (cdaar '(((_ t t) _) _))))
      (d "cdadr"
         (cdxr (cdadr '(_ (_ t t) _))))
      (d "cddar"
         (cdxr (cddar '((_ _ t t) _))))
      (d "cdddr"
         (cdxr (cdddr '(_ _ _ t t))))

      (d "caaaar"
         (caxr (caaaar '((((t _) _) _) _))))
      (d "caaadr"
         (caxr (caaadr '(_ ((t _) _)))))
      (d "caadar"
         (caxr (caadar '((_ (t _)) _))))
      (d "caaddr"
         (caxr (caaddr '(_ _ (t _)))))
      (d "cadaar"
         (caxr (cadaar '(((_ t) _)))))

      (d "cadadr"
         (caxr (cadadr '(_ ((_ _) t)))))
      (d "caddar"
         (caxr (caddar '((_ _ t)))))
      (d "cadddr"
         (caxr (cadddr '(_ _ _ t))))

      (d "cdaaar"
         (cdxr (cdaaar '((((_ t t) _) _) _))))
      (d "cdaadr"
         (cdxr (cdaadr '(_ ((_ t t) _)))))
      (d "cdadar"
         (cdxr (cdadar '((_ (_ t t)) _))))
      (d "cdaddr"
         (cdxr (cdaddr '(_ _ (_ t t)))))
      (d "cddaar"
         (cdxr (cddaar '(((_ _ t t) _) _))))
      (d "cddadr"
         (cdxr (cddadr '(_ (_ _ t t)))))
      (d "cdddar"
         (cdxr (cdddar '((_ _ _ t t) _))))
      (d "cddddr"
         (cdxr (cddddr '(_ _ _ _ t t))))))

(defn (:: listTests Spec)
  (do (let ((= d describe)))
      (d "list of x, y, and z"
         (it "should be a list"
           (let ((:: f (-> Int Char String Bool))
                 (= f x y z
                   (is-list (list x y z))))
             (property f))))

      (d "filtering pair"
         (it "should be pair"
           (property (\x
                       (or [(&& (is-atom x) (not (is-pair x)))
                            (&& (== nil x) (not (is-pair x)))
                            (&& (is-hslist x) (not (is-pair x)))
                            (is-pair x)])))))

      (d "filtering string"
         (it "should be AString"
           (property (\x
                       (==> (is-string (toCode x))
                            (case x
                              (AString _ _) True
                              _ False))))))

      (d "filtering char"
         (it "should be AChar"
           (property (\x
                       (==> (is-char (toCode x))
                            (case x
                              (AChar _ _) True
                              _ False))))))

      (d "filtering integer"
         (it "should be AInteger"
           (property (\x
                       (==> (is-integer (toCode x))
                            (case x
                              (AInteger _) True
                              _ False))))))

      (d "filtering fractional"
         (it "should be AFractional"
           (property (\x
                       (==> (is-fractional (toCode x))
                            (case x
                              (AFractional _) True
                              _ False))))))

      (d "filtering ()"
         (it "should be AUnit"
           (property (\x
                       (==> (is-unit (toCode x))
                            (case x
                              AUnit True
                              _ False))))))

      (d "length of atom"
         (it "should be 1 or nil"
           (let ((:: f (-> Code Property))
                 (= f x
                   (==> (is-atom x) (|| (== 1 (length x)) (null x)))))
             (property f))))

      (d "cons"
         (do (let ((= x 'x)
                   (= ret1 (cons x '[b c d]))
                   (= ret2 (cons x 'b))))
             (it "returns a List when consing to List"
               (is-list (cons 'a '(b c d))))
             (it "returns a List when consing to HsList"
               (is-list ret1))
             (it "has x at car of HsList-consed-list"
               (&& (== (car ret1) x)
                   (== (cdr ret1) '(b c d))))
             (it "returns a List when consing to Atom"
               (is-list ret2))
             (it "has x at car of atom-consed-list"
               (&& (== (car ret2) x)
                   (== (cdr ret2) '(b))))))

      (d "caris"
         (do (it "returns True"
               (== (caris 'a '(a b c)) True))
             (it "returns False"
               (== (caris 'a '(c b a)) False))))

      (d "make-symbol"
         (it "returns a symbol"
           (== (make-symbol "foo") 'foo)))

      (d "mb-symbol-name"
         (do (it "returns a Just String from symbol"
               (== (mb-symbol-name 'foo) (Just "foo")))
             (it "returns a Nothing from non-symbol"
               (== Nothing (mb-symbol-name '(foo bar buzz))))))

      (d "curve"
         (do (it "returns list from hslist"
               (== (curve '[a b c]) '(a b c)))
             (it "returns original value otherwise"
               (== 'foo (curve 'foo)))))

      (d "list"
         (do (it "returns list"
               (== (list 'a 'b 'c) '(a b c)))
             (it "returns nil with no arguments"
               (== nil (list)))))

      (d "reduce"
         (do (it "returns the original list"
               (== (reduce cons nil '(a b c d e)) '(a b c d e)))
             (it "should apply given function on non-list"
               (== (reduce cons nil 'foo) '(foo)))))

      (d "reduce1"
         (do (it "returns the original list"
               (== (reduce1 cons '(a b c d e)) '(a b c d e)))
             (it "should throw exception on non-list"
               (expectFailure (shouldBe (reduce1 cons 'foo) nil)))))

      (d "map1"
         (do (it "replaces non-symbols"
               (== (map1 (\x (if (is-symbol x) x '_))
                         '(foo bar (a b c) buzz 3 "string"))
                   '(foo bar _ buzz _ _)))
             (it "replaces non-symbols in HsList"
               (== (map1 (\x (if (is-symbol x) x '_))
                         '[foo bar (a b c) buzz 3 "string"])
                   '[foo bar _ buzz _ _]))
             (it "apply given function on non-list arg2"
               (== (map1 (\x (cons x x)) 'foo) '(foo foo)))))

      (d "keep"
         (do (it "removes non atom"
               (== (keep is-atom '(a (b c) d e (f g h))) '(a d e)))
             (it "throws an exception on non-list"
               (expectFailure (shouldBe (keep is-atom '"string") nil)))))

      (d "rev"
         (do (it "reverses List"
               (== (rev '(a b c)) '(c b a)))
             (it "reverses HsList"
               (== (rev '[a b c]) '[c b a]))
             (it "does nothing to non-list values"
               (== (rev 'foo) 'foo))))

      (d "unsnoc"
         (it "should split the last element"
           (== (unsnoc '(a b c d e)) (, '(a b c d) 'e))))

      (d "trav1"
         (do (it "should traverse list"
               (== (trav1 Just '(a b c)) (Just '(a b c))))
             (it "should apply given function on non-list"
               (== (trav1 Just 'foo) (Just 'foo)))))

      (d "omni"
         (do (it "should replace `a' to `b'"
               (== (omni (\x (if (== 'a x) 'b x))
                         '(a a (x a x) [y a y] a))
                   '(b b (x b x) [y b y] b)))
             (it "should apply given function on non-list argument"
               (== (omni (\x (cons x x)) 'foo) '(foo foo)))))

      (d "omniM"
         (do (it "should print all elements"
               (== (omniM Just '(a a (x a x) [y a y] a))
                    (Just '(a a (x a x) [y a y] a))))
             (it "should apply given function to non-list argument"
               (== (omniM Just 'foo) (Just 'foo)))))))
