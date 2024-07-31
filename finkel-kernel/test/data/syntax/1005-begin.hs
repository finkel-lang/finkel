;;; Tests for begin.

(:begin
  (module Main)
  (:begin
    (:: bgn01 (IO ()))
    (= bgn01
      (do (putStrLn "=== start bgn01 ===")
          (putStrLn ":begin")
          (putStrLn "in")
          (putStrLn "do")
          (putStrLn "=== end bgn01 ===")))))

(:begin
  (:: bgn02 (IO ())))
(:begin
  (= bgn02
    (let ((= f str
            (putStrLn str))
          (= g str
            (concat ["=== " str " ==="])))
      (f (g "bgn02")))))

(:begin
  (:begin
    (:: bgn03 (IO ())))
  (:begin
    (:begin
      (= bgn03
        (let ((= f str (++ "f:" str))
              (= g str (++ "g:" str)))
          (putStrLn (f (g "bgn03"))))))))

(:: main (IO ()))
(= main
  (do bgn01
      bgn02
      bgn03))
