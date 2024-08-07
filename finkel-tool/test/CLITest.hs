;;; -*- mode: finkel -*-
;;; Tests for CLI type class and its instances

%p(language RankNTypes)

(defmodule CLITest
  (export cliTests)
  (import-when [:compile]
    ;; finkel-core
    (Finkel.Prelude))
  (import
   ;; base
   (Control.Exception [bracket])
   (Control.Monad [replicateM])
   (GHC.IO.Handle [hDuplicate hDuplicateTo])
   (System.IO
    [(IOMode ..) hClose hSetNewlineMode noNewlineTranslation openFile stdin])

   ;; filepath
   (System.FilePath [</>])

   ;; haskeline
   (System.Console.Haskeline [defaultSettings runInputT])

   ;; hspec
   (Test.Hspec)

   ;; Internal
   (Finkel.Tool.Internal.CLI)
   (TestAux)))

(defn (:: cliTests Spec)
  (do (describe "IO instance" io-tests)
      (describe "InputT instance" inputT-tests)))

(defn (:: io-tests Spec)
  (make-cli-tests id))

(defn (:: inputT-tests Spec)
  (make-cli-tests (runInputT defaultSettings)))

(defn (:: make-cli-tests
        (=> (CLI m) (-> (forall a (-> (m a) (IO a))) Spec)))
  [toIO]
  (do (describe "getString"
        (lept [expected
               (cond-expand
                 [(== :os "mingw32")
                  [(Just "First line\r") (Just "Second line\r") Nothing]]
                 [otherwise
                  [(Just "First line") (Just "Second line") Nothing]])]
          (it "should end with Nothing"
            (shouldReturn (with-test-stdin "input01.txt"
                            (toIO (replicateM 3 (getString ""))))
                          expected))))
      (describe "putString"
        (it "should run successfully"
          (quietly (toIO (putString "foo")))))
      (describe "handleInterrupt"
        (it "should run the given action"
          (toIO (handleInterrupt (return ()) (return ())))))
      (describe "withInterrupt"
        (it "should run the given action"
          (toIO (withInterrupt (return ())))))
      (describe "exitWith"
        (it "should throw exit failure"
          (shouldThrow (toIO (exitWith (ExitFailure 1)))
                       (== (ExitFailure 1)))))))

(defn (:: with-test-stdin (-> String (IO a) (IO a)))
  [path act]
  (bracket
   (do (<- stdin2 (hDuplicate stdin))
       (<- hdl (openFile (datafile path) ReadMode))
       (hSetNewlineMode hdl noNewlineTranslation)
       (hDuplicateTo hdl stdin)
       (return (, hdl stdin2)))
   (\ (, hdl stdin2)
     (do (hDuplicateTo stdin2 stdin)
         (hClose hdl)
         (hClose stdin2)))
   (const act)))

(defn (:: datafile (-> String FilePath))
  [name]
  (</> "test" (</> "data" name)))
