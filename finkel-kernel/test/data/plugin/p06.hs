;;; -*- mode: finkel -*-

(:eval-when-compile
  (import Prelude)
  (import Language.Finkel)
  (:: m1 Macro)
  (= m1
    (Macro
     (\_ (do (<- tmp gensym)
             (pure `(let ((= ,tmp (not True)))
                      (print ,tmp))))))))

(module Main)

(:: main (IO ()))
(= main (m1))
