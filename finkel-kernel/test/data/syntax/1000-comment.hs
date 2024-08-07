;;; -*- mode: finkel -*-

;;;; | File with documentation header comments.
;;;;
;;;; Some more documentation strings in consequent lines. Some more
;;;; documentation strings in consequent lines. Some more documentation
;;;; strings in consequent lines.
;;;;
;;;; Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do
;;;; eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim
;;;; ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut
;;;; aliquip ex ea commodo consequat. Duis aute irure dolor in
;;;; reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla
;;;; pariatur. Excepteur sint occaecat cupidatat non proident, sunt in
;;;; culpa qui officia deserunt mollit anim id est laborum.

(module Main)

{-

Sample block comment. All literals between character sequence `#' `|',
and `|' `#' are block comment. Block comments understand UNICODE
characters:

- 我能吞下玻璃而不伤身体。
- ฉันกินกระจกได้ แต่มันไม่ทำให้ฉันเจ็บ
- მინას ვჭამ და არა მტკივა.

-}

;;; * The main function

;;; $foo
;;;
;;; Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do
;;; eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim
;;; ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut
;;; aliquip ex ea commodo consequat. Duis aute irure dolor in
;;; reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla
;;; pariatur. Excepteur sint occaecat cupidatat non proident, sunt in
;;; culpa qui officia deserunt mollit anim id est laborum.

;;; | Main entry function.
(= main
  ;; This is not a documentation comment.
  (foo "Module with doc comments."))

;;; * Other functions
;;; ** The foo function

(= foo str
  (>> (foo-aux str)
      (bar 15 27)))
;;; ^ Comment for function foo.

;;; *** Auxiliary function for foo

(= foo-aux putStrLn)

{-onelineblockcommentwithoutspaces-}

;;; ** The bar function

;;; | Comment for function bar.
;;;
;;; This comment spans multiple lines. Bar bar bar bar bar bar bar bar
;;; bar bar bar bar bar bar bar bar bar bar.
;;;
;;; Bar bar bar bar bar bar bar bar bar bar bar bar bar bar bar bar bar
;;; bar bar bar bar bar bar bar bar bar bar bar bar bar bar bar bar bar
;;; bar bar bar.
;;;
;;; Some unicode strings:
;;;
;;; - 我能吞下玻璃而不伤身体。
;;; - ฉันกินกระจกได้ แต่มันไม่ทำให้ฉันเจ็บ
;;; - მინას ვჭამ და არა მტკივა.

(:: bar (-> Int                 ; ^ Arg 1.
            Int                 ; ^ Arg 2.
            (IO ())))
(= bar a b
  (putStrLn {-more-}
   (++ {-block-} "From bar: " {-comments-} (show (+ a b)))))

;;; ** The buzz function

;;; | Comment for function buzz.
;;;
;;; This comment is written on the line above type signature of buzz.
;;;
(:: buzz (-> Int Int))
(= buzz %_(codes inside this list is ignored) n
  %_this_symbol_is_ignored
  (+ n %_"ignored string literal" n))
