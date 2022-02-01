;;; -*- mode: finkel -*-

(defmodule Main
  (require
   (Required.Modules.Are.Ignored.In.Downsweep))
  (import
   (Control.Monad [forM- when])
   (qualified Data.ByteString as BS)
   (Data.Maybe hiding [fromJust]))
  (import-when [:compile]
    (Compile.Time.Only.Import))
  (import-when [:compile :load]
    (Control.Applicative [liftA3])))

(:: main (IO ()))
(= main (putStrLn "preprocess/fnk12.hs"))
