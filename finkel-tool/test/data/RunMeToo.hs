;;; -*- mode: finkel -*-

(defmodule RunMeToo
  (import
   (System.Environment (getArgs))
   (System.Exit (exitFailure))))

(defn (:: main-one (IO ()))
  (putStrLn "From RunMeToo.main-one"))

(defn (:: main-two (IO ()))
  (putStrLn "From RunMeToo.main-two"))

(defn (:: main-three (IO ()))
  (do (<- args getArgs)
      (case args
        ["dog"] (putStrLn "WUFF WUFF WUFF!")
        ["cat"] (putStrLn "MEOW MEOW MEOW!")
        _ (>> (putStrLn "I don't know what to do")
              exitFailure))))

(defn (:: main (IO ()))
  main-one)
