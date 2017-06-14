;;; File containing forms using macros.

(module Main)

(import SK.Core.Form)
(import SK.Core.SKC)

(defmacro-transformer m1
  (\ (form)
   (return `(putStrLn (++ "Hello, " ~(car form))))))

(defn main
  (m1 "macro"))
