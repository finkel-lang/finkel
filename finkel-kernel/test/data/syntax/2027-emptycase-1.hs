;;;; File containing code using `EmptyCase' language extension

%p(LANGUAGE EmptyCase)

(data Void)

(:: absurd (-> Void a))
(= absurd a (case a))

(:: main (IO ()))
(= main
  (putStrLn "EmptyCase language extension"))
