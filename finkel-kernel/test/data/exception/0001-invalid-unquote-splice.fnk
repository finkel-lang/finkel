(:eval-when-compile
  (import Prelude)
  (import Language.Finkel)
  (import Control.Monad.IO.Class (liftIO))

  (:: m Macro)
  (= m
    (Macro (\ (LForm (L _ (List [_ arg1])))
             (return `(:a 1 :b ,@arg1))))))

(:: main (IO ()))
(= main
  (print (m foo)))
