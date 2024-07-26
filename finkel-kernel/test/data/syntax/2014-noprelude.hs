;;;; File containing NoImplicitPrelude.

%p(LANGUAGE NoImplicitPrelude)
%p(OPTIONS_GHC -Wall)

(module Main)

(import Prelude hiding (^ read))

(:: read (-> String Bool))
(= read "true" True)
(= read _ False)

(:: ^ (-> [a] [a] [a]))
(= ^ ++)

(:: main (IO ()))
(= main
  (putStrLn (^ "(read true) ==> " (show (read "true")))))
