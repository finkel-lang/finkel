;;; -*- mode: finkel -*-

(module M3 greet (Greet ..) (Greetable ..))

(import Language.Finkel)

(newtype (Greet a) (Greet a))

(class (Greetable a)
  (:: gg (-> a String)))

(:: greet Macro)
(= greet
  (Macro (\form
           (case (unCode form)
             (List [_ body]) (return `(putStrLn ,body))
             _ (finkelSrcError form "greet: malformed body")))))
