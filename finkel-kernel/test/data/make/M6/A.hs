;;; -*- mode: finkel -*-

(module M6.A m6a1 m6a2)

(import Language.Finkel)

(:: m6a1 Macro)
(= m6a1 (Macro
         (\form
           (case (unCode form)
             (List [_]) (return `(putStrLn "From M6.A.m6a1"))
             _ (error "m6a1")))))

(:: m6a2 Macro)
(= m6a2 (Macro
         (\form
           (case (unCode form)
             (List [_]) (return `(putStrLn "From M6.A.m6a2"))
             _ (error "m6a2")))))
