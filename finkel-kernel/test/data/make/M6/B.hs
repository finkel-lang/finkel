;;; -*- mode: finkel -*-

(module M6.B m6b1 m6b2)

(import Language.Finkel)

(:: m6b1 Macro)
(= m6b1 (Macro
         (\form
           (case (unCode form)
             (List [_]) (return `(putStrLn "From M6.B.m6b1"))
             _ (error "m6b1")))))

(:: m6b2 Macro)
(= m6b2 (Macro
         (\form
           (case (unCode form)
             (List [_]) (return `(putStrLn "From M6.B.m6b2"))
             _ (error "m6b2")))))
