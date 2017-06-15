;;; Tests for quote, quasiquote, unquote, and unquote-splice.

(module Main)

(import SK.Core.Form)

(= (f1 arg)
  (print ['a 'b arg 'd]))

(= (f2 arg)
  (print `(a b ,arg d)))

(= (f3 arg)
  (print `(a b ,@arg d)))

(:: main (IO ()))
(= main
  (do
    ;; Quotes can nest.
    (print 'foo)
    (print (quote foo))
    (print ''foo)
    (print '''foo)

    ;; Quoting literals
    (print '"string")
    (print '42)
    (print '[1 2 3])
    (print '())

    ;; Quasiquote.
    (print `foo)
    (print (quasiquote foo))

    (f1 'foo)
    (f2 'foo)
    (f3 '(foo bar buzz))))
