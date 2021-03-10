;;;; Forms containing FFI.

(module Main)

(import Foreign.C.String ((CString) peekCAString))

;;; 8.4 Foreign Declarations

;;; 8.4.3 Import Declarations

(foreign import ccall (:: rand (IO Int)))

(foreign import ccall "sin"
  (:: csin (-> Double Double)))

(foreign import ccall safe "cos"
  (:: ccos (-> Double Double)))

(foreign import ccall unsafe "math.h tan"
  (:: ctan (-> Double Double)))

(:: s8_4_3 (IO ()))
(= s8_4_3
  (do (print (csin (* pi 0.5)))
      (print (ccos (* pi 2)))
      (print (ctan pi))))

;;; 8.4.4 Export Declarations

(:: printSomeThing (-> CString (IO ())))
(= printSomeThing something
  (do (putStrLn "From printSomething")
      (<- str (peekCAString something))
      (putStrLn str)))

(foreign export ccall "printSomeThing"
  (:: printSomeThing (-> CString ( IO ()))))

;; XXX: Not working. This code compiles with fnkc, but fails to
;; compile the generated Haskell code. 'Outputable.ppr' for operator
;; function in FFI export does not use parenthesis in ghc. This also
;; happens when running ghc with "-ddump-parsed" option.
;;
;; (foreign export ccall "addInt"
;;   (:: + (-> Int Int Int)))

;;; Main

(:: main (IO ()))
(= main
  s8_4_3)
