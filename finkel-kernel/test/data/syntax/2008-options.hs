;;;; OPTIONS_GHC and OPTIONS_HADDOCK pragma.

%p(LANGUAGE DeriveFoldable)
%p(OPTIONS_GHC -Wall)
%p(LANGUAGE DeriveFunctor)
%p(OPTIONS_HADDOCK prune)
%p(OPTIONS_GHC -fspec-constr-keen)
%p(LANGUAGE GeneralizedNewtypeDeriving)

(module Main)

(:: main (IO ()))
(= main
  (putStrLn "File with OPTIONS_GHC"))
