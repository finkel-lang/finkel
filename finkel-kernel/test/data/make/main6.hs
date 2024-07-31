;;; -*- mode: finkel -*-

(:: main (IO ()))
(= main
  (do (print (f1 41))
      (print (f2 40))
      (print (f3 39))))

(foreign import ccall safe "f1" (:: f1 (-> Int Int)))
(foreign import ccall safe "f2" (:: f2 (-> Int Int)))
(foreign import ccall safe "f3" (:: f3 (-> Int Int)))
