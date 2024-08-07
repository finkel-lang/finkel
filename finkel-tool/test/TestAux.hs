;;; -*- mode: finkel -*-
;;;; Auxiliary codes for tests

(defmodule TestAux
  (export (TestIO ..) (TestIOState ..) runTestIO
          (EvalTestFns ..) makeEvalTestFns quietly)
  (require
   ;; finkel-core
   (Finkel.Tool.Internal.Macro.Ghc))
  (import-when [:compile]
    ;; finkel-core
    (Finkel.Prelude))
  (import
   ;; base
   (Control.Concurrent
    [ThreadId newEmptyMVar killThread putMVar takeMVar])
   (Control.Monad [mplus])
   (Control.Monad.IO.Class [(MonadIO ..)])
   (Data.Function [on])
   (System.Environment [lookupEnv])
   (System.IO [stderr stdout])

   ;; filepath
   (System.FilePath [</>])

   ;; hspec
   (Test.Hspec)

   ;; silently
   (System.IO.Silently [hSilence])

   ;; finkel-kernel
   (Language.Finkel)

   ;; Internal
   (Finkel.Tool.Command.Repl)
   (Finkel.Tool.Internal.CLI)
   (Finkel.Tool.Internal.Eval)
   (Finkel.Tool.Internal.Loop)
   (Finkel.Tool.Internal.Types)))

(imports-from-ghc
 (GHC.Settings.Config (cProjectVersion)))


;;; Extra imports

(cond-expand
  [(:min-version "base" 4 11 0)
   (:begin)]
  [otherwise
   (import Data.Monoid (<>))])

;;; Test IO

(data TestIOState
  (TestIOState {(:: tst-inputs [String])
                (:: tst-outputs [String])
                (:: tst-exitcode (Maybe ExitCode))
                (:: tst-replstate ReplState)}))

(instance (Monoid TestIOState)
  (= mempty emptyTestIOState)
  (cond-expand
    [(:min-version "base" 4 16 0)
     (:begin)]
    [otherwise
     (= mappend appendTestIOState)]))

(cond-expand
  [(:min-version "base" 4 11 0)
   (instance (Semigroup TestIOState)
     (= <> appendTestIOState))]
  [otherwise
   (:begin)])

(defn (:: appendTestIOState (-> TestIOState TestIOState TestIOState))
  [s1 s2]
  (TestIOState
   {(= tst-inputs (on mappend tst-inputs s1 s2))
    (= tst-outputs (on mappend tst-outputs s1 s2))
    (= tst-exitcode (mplus (tst-exitcode s2) (tst-exitcode s1)))
    (= tst-replstate (on mappend tst-replstate s1 s2))}))

(defn (:: emptyTestIOState TestIOState)
  (TestIOState {(= tst-inputs [])
                (= tst-outputs [])
                (= tst-exitcode Nothing)
                (= tst-replstate initial-repl-state)}))

;;; Newtype wrapper to test IO actions, combination of TestIOState state
;;; monad and IO.
(newtype (TestIO a)
  (TestIO {(:: unTestIO (-> TestIOState (IO (, a TestIOState))))}))

(defn (:: runTestIO (-> (TestIO a) [String] (IO (, a TestIOState))))
  [test-io inputs]
  (unTestIO test-io (mempty {(= tst-inputs inputs)})))

(instance (Functor TestIO)
  (= fmap f (TestIO m)
    (TestIO (\st0 (fmap (\ (, a st) (, (f a) st)) (m st0))))))

(instance (Applicative TestIO)
  (= pure x
    (TestIO (\st (pure (, x st)))))
  (= <*> (TestIO ft) (TestIO xt)
    (TestIO (\st0 (do (<- (, f st1) (ft st0))
                      (<- (, x st2) (xt st1))
                      (return (, (f x) st2)))))))

(instance (Monad TestIO)
  (= return pure)
  (= >>= (TestIO m) k
    (TestIO (\st0 (do (<- (, a st1) (m st0))
                      (unTestIO (k a) st1))))))

(instance (MonadIO TestIO)
  (= liftIO io
    (TestIO (\st (fmap (\x (, x st)) io)))))

(instance (CLI TestIO)
  (= getString _prompt
    (TestIO (\tst
              (case (tst-inputs tst)
                (: s rest) (lept [tst' (tst {(= tst-inputs rest)})]
                             (pure (, (Just s) tst')))
                [] (pure (, Nothing tst))))))

  (= putString str
    (TestIO
     (\st (lept [tst-outputs' (<> (tst-outputs st) [str])]
            (pure (, () (st {(= tst-outputs tst-outputs')})))))))

  ;;; XXX: Does nothing.
  (= handleInterrupt _handler act act)

  ;;; XXX: Does nothing.
  (= withInterrupt act act)

  (= exitWith ec
    (TestIO (\st (pure (, () (st {(= tst-exitcode (Just ec))})))))))

(instance (HasReplState TestIO)
  (= putReplState rst
    (TestIO (\st (pure (, () (st {(= tst-replstate rst)}))))))
  (= getReplState
    (TestIO (\st (pure (, (tst-replstate st) st))))))


;;; Repl test environment

(data EvalTestFns
  (EvalTestFns
   {(:: etf-ok (-> Code String Spec))
    (:: etf-ng (-> Code String Spec))
    (:: etf-satisfy (-> Code (-> Result Bool) Spec))
    (:: etf-cleanup (IO ()))
    (:: etf-tid ThreadId)}))

(defn (:: if-ghc-package-path-is-set (=> (MonadIO m) (-> (m a) (m a) (m a))))
  "Perform the first action if @GHC_PACKAGE_PATH@ is set in environment
  variable, otherwise perform the second."
  [set-act not-set-act]
  (case-do (liftIO (lookupEnv "GHC_PACKAGE_PATH"))
    (Just _) set-act
    Nothing  not-set-act))

(defn (:: init-etf-args-for-cabal (IO [String]))
  "Initialization arguments for running eval tests with `cabal-install'."
  (lept [ghc-ver (++ "ghc-" cProjectVersion)
         inplacedb (</> ".." "dist-newstyle" "packagedb" ghc-ver)
         args ["-package-db" inplacedb]]
    (pure args)))

(defn (:: init-etf-args (IO [String]))
  "Initialization arguments for `EvalTestFns'."
  (if-ghc-package-path-is-set (return []) init-etf-args-for-cabal))

(defn (:: makeEvalTestFns (IO EvalTestFns))
  (do (<- out-mv newEmptyMVar)
      (<- (@ resources (, _tmpfile hdl in-mv)) acquire-repl)
      (<- ghc-args init-etf-args)
      (<- etid (fork-eval-loop ghc-args hdl in-mv repl-env))
      (lefn [(eval-form [right-or-left form expect]
               (describe (++ "evaluate " (show form))
                 (it "evaluates to expected result"
                   (quietly
                    (do (putMVar in-mv (Input Connection form out-mv))
                        (<- ret (takeMVar out-mv))
                        (shouldBe ret (right-or-left expect)))))))
             (ok (eval-form Right))
             (ng (eval-form Left))
             (satisfy [form test]
               (describe (++ "evaluate " (show form))
                 (it "satisfies predicate"
                   (quietly
                    (do (putMVar in-mv (Input Connection form out-mv))
                        (<- ret (takeMVar out-mv))
                        (shouldSatisfy ret test))))))
             (cleanup
               (do (killThread etid)
                   (cleanup-repl resources)))])
      (putMVar in-mv (Input Connection '(:begin) out-mv))
      (<- _ (takeMVar out-mv))
      (return (EvalTestFns {(= etf-ok ok)
                            (= etf-ng ng)
                            (= etf-satisfy satisfy)
                            (= etf-cleanup cleanup)
                            (= etf-tid etid)}))))

(defn (:: quietly (-> (IO a) (IO a)))
  (hSilence [stderr stdout]))
