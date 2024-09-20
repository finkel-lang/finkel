;;; -*- mode: finkel -*-

(module M04b)

(import Language.Finkel)

(:: defmac Macro)
(= defmac
  (Macro (\ (LForm (L _ (List (: _ forms))))
           (case forms
             [name args body] (pure `(:begin
                                       (:: ,name Macro)
                                       (= ,name
                                         (Macro
                                          (\ (LForm (L _ (List (: _ _forms))))
                                            (case _forms
                                              ,args (pure ,body)
                                              _ (error "defmac: yikes!")))))))
             _ (error "defmac: ahh!")))))
