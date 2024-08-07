;;; -*- mode: finkel -*-

(:require P1)

(module P2)

(:eval-when-compile
  (import Prelude)
  (import Language.Finkel))

(import Language.Finkel)
(import P1)

(:eval-when-compile
  (define-macro define-foo form
    (case (unCode form)
      (List [_ name]) (return `(define-macro ,name _args
                                 (return '(putStrLn "foo"))))
      _ (finkelSrcError form "define-foo: error")))

  (:: show-d-fn (-> D Code))
  (= show-d-fn d `(show ,d))

  (define-macro show-d form
    (case (unCode form)
      (List [_ o]) (| ((<- (Just d) (fromCode o))
                       (return `(,(show-d-fn d))))
                      (otherwise
                       (finkelSrcError form (++ "say-d: cannot get D from `"
                                                (show o) "'"))))
      _ (finkelSrcError form "say-d: error"))))

(define-foo foo)

(:: print-d (-> D (IO ())))
(= print-d d (putStrLn (show-d D2)))
