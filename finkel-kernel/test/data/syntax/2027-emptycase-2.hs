;;; -*- mode: finkel -*-

%p(LANGUAGE EmptyCase
            DataKinds
            KindSignatures
            GADTs)
%p(OPTIONS_GHC -Werror=incomplete-patterns)

(import Data.Kind (Type))

;; Types

(data D O C L)

(data (:: SD (-> D Type))
  (:: SO (SD 'O))
  (:: SC (SD 'C))
  (:: SL (SD 'L)))

(data (:: K (-> D Type))
  (:: KC (K 'C))
  (:: KL (K 'L)))

(data Void)

(data (Decision a)
  (Proved a)
  (Disproved (-> a Void)))

;; Function

(:: isA (-> (SD s) (Decision (K s))))
(= isA s
  (case s
    SO (Disproved (\x (case x)))
    SC (Proved KC)
    SL (Proved KL)))

(:: main (IO ()))
(= main
  (let ((= f s
          (putStrLn (case (isA s)
                      (Proved _) "Proved"
                      (Disproved _) "Disproved"))))
    (do (f SO)
        (f SC)
        (f SL))))
