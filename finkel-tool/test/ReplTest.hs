;;; -*- mode: finkel -*-
;;; Tests for REPL.

%p(LANGUAGE OverloadedStrings)
%p(OPTIONS_GHC -Wno-orphans)

(defmodule ReplTest
  (export replTests listenTests)
  (require
   ;; finkel-tool
   (Finkel.Tool.Internal.Macro.Ghc))
  (import-when [:compile]
    ;; finkel-core
    (Finkel.Prelude))
  (import
   ;; base
   (Control.Concurrent
    [forkIO newEmptyMVar killThread putMVar takeMVar threadDelay yield])
   (Control.Exception [IOException finally throwIO])
   (qualified Control.Exception as ControlException)
   (Control.Monad [forever replicateM_ when])
   (Data.List [isSubsequenceOf])
   (Data.String [(IsString ..)])
   (GHC.Conc [(ThreadStatus ..) threadStatus])
   (GHC.IO.Exception [(AsyncException ..)])

   ;; filepath
   (System.FilePath [</>])

   ;; haskeline
   (System.Console.Haskeline [defaultSettings runInputT])

   ;; hspec
   (Test.Hspec)

   ;; network
   (Network.Socket
    [(AddrInfo ..) (SocketType ..) close connect defaultHints getAddrInfo
     socket])
   (Network.Socket.ByteString [sendAll recv])

   ;; finkel-kernel
   (Language.Finkel)

   ;; Internal
   (Finkel.Tool.Command.Repl)
   (Finkel.Tool.Internal.IO)
   (Finkel.Tool.Internal.Types)
   (TestAux)))

(imports-from-ghc
 (GHC.Data.StringBuffer (StringBuffer stringToStringBuffer)))


;;; Extra imports

(cond-expand
  [(<= 810 :ghc)
   (import Control.Monad.Catch
           ((MonadThrow ..) (MonadCatch ..) (MonadMask ..)))]
  [otherwise
   (:begin)])

(cond-expand
  [(:min-version "base" 4 11 0)
   (:begin)]
  [otherwise
   (import Data.Monoid (<>))])

;;; Exported test

(defn (:: replTests (-> EvalTestFns Spec))
  [etf]
  (do (describe "ReplState" replStateTests)
      (describe "Exception" exceptionTests)
      (describe "Read" readTests)
      (describe "ReadPrint" readPrintTests)
      (describe "Eval"
        (cond-expand
          [(== :os "mingw32")
           (it "should be skipped under Windows"
             (pendingWith "Windows not supported yet"))]
          [otherwise (evalTests etf)]))))

(defn (:: listenTests (-> EvalTestFns Spec))
  [etf]
  (describe "Listen"
    (cond-expand
      [(== :os "mingw32")
        (it "should be skipped under Windows"
          (pendingWith "Windows not supported yet"))]
      [otherwise (listenTests1 etf)])))

;;; Orphan

(instance (IsString StringBuffer)
  (= fromString stringToStringBuffer))

;;; Internal

(defn (:: replStateTests Spec)
  (do (lept [rs1 (mempty {(= pending-input (Just "(foo"))})
             rs2 (mempty {(= pending-input (Just " bar)"))})])
      (describe "Show instance"
        (it "should show pending inputs"
          (shouldBe
           (show rs1)
           "ReplState {pending_input = Just <stringbuffer(4,0)>, \
\prompt_string = \"> \"}")))
      (describe "Eq instance"
        (do (it "should be itself" (shouldBe rs1 rs1))
            (it "should not be different pending input"
              (shouldNotBe rs1 rs2))))
      (describe "Monoid laws for ReplState"
        (do (it "should have an identity element"
              (shouldBe (<> mempty rs1) rs1))
            (it "should satisfy associativity law"
              (shouldBe (<> (<> rs1 mempty) rs2)
                        (<> rs1 (<> mempty rs2))))))
      (describe "get and put ReplState for InputT"
        (do (lept [act (run-repl (runInputT defaultSettings work) mempty)
                   work (>> (putReplState mempty) getReplState)])
            (it "should return the given ReplState"
              (shouldReturn act mempty))))
      (lept [run-repl' (flip run-repl mempty)
             repl1 (pure True)])
      (describe "Functor instance of Repl"
        (do (it "should satisfy identity law"
              (shouldReturn (run-repl' (fmap id repl1)) True))
            (it "should satisfy composition law"
              (shouldReturn
               (run-repl' (fmap show (fmap not repl1)))
               (show (not True))))
            (it "should return second arg with <$"
              (shouldReturn
               (run-repl' (<$ True (pure False)))
               True))))
      (describe "Applicative instance of Repl"
        (it "should satisfy applicative law"
          (shouldReturn
           (run-repl' (<*> (pure not) repl1))
           False)))))

(defn (:: do-repl (-> (Repl a) (IO a)))
  (flip run-repl initial-repl-state))

(defn (:: exceptionTests Spec)
  (describe "exceptions instances for REPL"
    (cond-expand
      [(<= 810 :ghc)
       (do (lept [throw-sof-on-no-input
                  (>>= getReplState
                       (. (maybe (throwM StackOverflow)
                                 (const (return 42)))
                          pending-input))])
           (it "should throw exception"
             (shouldThrow (do-repl (throwM StackOverflow)) anyException))
           (it "should catch exception"
             (lefn [(act (catch throw-sof-on-no-input handler))
                    (:: handler (-> AsyncException (Repl Int)))
                    (handler [ae]
                      (do (<- st getReplState)
                          (case (, ae (pending-input st))
                            (, StackOverflow Nothing) (return 42)
                            _ (throwM ae))))]
               (shouldReturn (do-repl act) 42)))
           (it "should throw exception from mask when unmasked"
             (lept [act (mask (\unmask
                                (unmask throw-sof-on-no-input)))]
               (shouldThrow (do-repl act) anyException)))
           (it "should throw exception from uninterruptibleMask when unmasked"
             (lept [act (uninterruptibleMask
                         (\unmask
                           (unmask throw-sof-on-no-input)))]
               (shouldThrow (do-repl act) anyException))))]
      [otherwise
       (it "should throw, catch, and mask exceptions as necessary"
         (pendingWith "... on newer versions of ghc"))])))

(defn (:: readTests Spec)
  (do (describe "reading single line form"
        (it "returns '(foo bar buzz)"
          (do (<- form (do-repl (read-form "(foo bar buzz)")))
              (shouldBe form (Just '(foo bar buzz))))))
      (describe "reading multi line form"
        (it "returns '(a b c)"
          (do (<- form (do-repl (do (<- _ (read-form "(a "))
                                    (<- _ (read-form "b "))
                                    (read-form "c)"))))
              (shouldBe form (Just '(a b c))))))))

(defn (:: readPrintTests Spec)
  (describe "read and print loop"
    (do (rptest "multi line form" ["(print" "(+" "10" "32" "))"])
        (rptest "quitting with \"(quit)\"" ["(quit)"])
        (rptest "\",t\" command" [",t False"])
        (rptest "\",!\" command" [",! echo foo bar"])
        (rptest "\",q\" command" [",q"]))))

(defn (:: rptest (-> String [String] Spec))
  [label inputs]
  (lept [run (do (<- in-mv newEmptyMVar)
                 (<- tid (forkIO (forever
                                  (do (<- (Input _ _ out-mv) (takeMVar in-mv))
                                      (putMVar out-mv (Right ""))))))
                 (return (, in-mv tid)))]
    (describe label
      (it "should have no pending inputs"
        (do (<- (, in-mv tid) run)
            (<- (, _ tst) (runTestIO (read-print-loop nil in-mv tid) inputs))
            (finally
             (shouldSatisfy (pending-input (tst-replstate tst)) null)
             (killThread tid)))))))

(defn (:: evalTests (-> EvalTestFns Spec))
  [(EvalTestFns {(= etf-ok ok) (= etf-satisfy satisfy)})]
  (do
    ;; Statements and declarations
    (ok '(+ 10 32) "42")
    (ok '(defn (:: f1 (-> Int Int))
          [n]
          (+ n 1))
        "; f1 :: Int -> Int")
    (ok '(f1 41) "42")
    (ok '(:begin
          (:: x y Int)
          (= x 1)
          (= y 2))
        "; x :: Int\n; y :: Int")
    (ok '(<- z (return True)) "")
    (ok '(defn (:: f2 (-> (Maybe Int) Int))
          [(Just n)] (* n 2)
          [Nothing] 0)
        "; f2 :: Maybe Int -> Int")
    (ok '(f2 (Just 21)) "42")
    (ok '(data Foo (Foo Int))
        (concat ["; $tcFoo :: TyCon\n"
                 "; $tc'Foo :: TyCon\n"
                 "; Type constructor ‘Foo’"]))

    ;; Import
    (ok '(import Control.Monad)
        "; import Control.Monad")
    (ok '(import qualified Data.Functor as DF)
        "; import qualified Data.Functor as DF")
    (ok '(import Control.Monad (liftM ap))
        "; import Control.Monad ( liftM, ap )")

    ;; Eval wrapper
    (ok 'System.Environment.getArgs "[]")

    ;; Expansion quoted codes in REPL
    (ok '(macroexpand ''foo)
        "(:quote foo)")

    ;; Exported macros
    (satisfy '(exported-macros Finkel.Core)
             (lcase
               (Right str) (isSubsequenceOf "defmacro" str)
               _ False))

    ;; Errors
    (satisfy 'buzz
             (lcase
               (Left str) (isSubsequenceOf "Variable not in scope: buzz" str)
               _ False))
    (satisfy '(= f a (+ a 1) (+ a 2))
             (lcase
               (Left str) (isSubsequenceOf "syntax error on input" str)
               _ False))

    (satisfy '(head [])
             (lcase
               (Left str) (isSubsequenceOf "*** Exception: Prelude.head: empty list"
                                           str)
               _ False))))

(defn (:: listenTests1 (-> EvalTestFns Spec))
  [(EvalTestFns {(= etf-tid etid)})]
  (lefn [(short-pause
           (threadDelay 50000))
         (wait-until-killed [tid]
           (do (<- st (threadStatus tid))
               (putStrLn (++ "listenTests1: " (show st)))
               (when (notElem st [ThreadFinished ThreadDied])
                 (do short-pause
                     (wait-until-killed tid)))))
         (acquire
           (do (wait-until-killed etid)
               (<- tid (forkIO
                        ;; Passing a file to work for, so that the REPL thread
                        ;; will not terminate before the testing client connect.
                        (replMain [(++ "--listen=" port)
                                   (++ "--file=" input-file)
                                   "--prompt="
                                   "--quiet"])))
               ;; Pause for a bit after forking the server action.
               (replicateM_ 5 short-pause)
               (<- addr (resolve "127.0.0.1" port))
               (<- conn (socket (addrFamily addr)
                                (addrSocketType addr)
                                (addrProtocol addr)))
               (with-retry (:: 20 Int)
                 (connect conn (addrAddress addr)))
               (return (, conn tid))))
         (with-retry [n act]
           (ControlException.catch act
                                   (\e
                                     (if (< 0 n)
                                       (do yield
                                           short-pause
                                           (with-retry (- n 1) act))
                                       (throwIO (:: e IOException))))))
         (release [(, conn tid)]
           (do (sendAll conn ",quit")
               (<- _msg (recv conn 1024))
               (close conn)
               (killThread tid)))
         (port "50322")
         (input-file (</> "test" "data" "sleep-for-while.fnk"))
         (resolve [host portnum]
           (lefn [(hints (defaultHints {(= addrSocketType Stream)}))]
             (case-do (getAddrInfo (Just hints) (Just host) (Just portnum))
               (: addr _) (return addr)
               _ (error "REPL client: address error"))))
         (work [(, conn _)]
           (do (<- _msg1 (recv conn 1024))
               (sendAll conn "(* 7 (+ 4 2))")
               (recv conn 1024)))
         (listener-test
           (describe "listener"
             (it "evaluates a form sent from connected client"
               (\args (shouldReturn (work args) "42")))))]
    (before acquire (after release listener-test))))
