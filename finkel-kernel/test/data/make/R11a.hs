;;; -*- mode: finkel -*-

(module R11a)

(import R01)

(:: r11a (-> String (IO ())))
(= r11a putStrLn)

(:: r11a-run (IO ()))
(= r11a-run (r11a foo-function))
