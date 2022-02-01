; Using DEADBEEF as pragma string

(module Main)

(import Control.Monad)

(:: main (IO ()))
(= main (forM- (Just "From fnk05.hs") putStrLn))

;;; Local variables:
;;; mode: finkel
;;; fill-columns: 72
;;; comment-column: 0
;;; End:
