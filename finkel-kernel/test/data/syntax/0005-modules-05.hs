;;; Module without header, with imports.

(import Control.Monad (foldM))

(:: main (IO ()))
(= main
  (where go
    (= go
      (do (<- n (foldM f 0 [#'a .. #'z]))
          (print n)))
    (= f n a
      (do (print a)
          (return (+ n 1))))))
