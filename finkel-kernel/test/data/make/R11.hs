;;; -*- mode: finkel -*-

;;; R11 --import-> {R11a, R11b} --import-> R01

(module R11)

(import R11a)
(import R11b)

(:: main (IO ()))
(= main (r11a r11b))
