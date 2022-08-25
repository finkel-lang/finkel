;;; -*- mode: finkel -*-
;;; Tests for main function

(:require Finkel.Core)

(defmodule MainTest
  (export mainTests)
  (import-when [:compile]
    (Finkel.Prelude))
  (import
   ;; base
   (Control.Concurrent [forkIO killThread threadDelay])
   (Control.Exception [bracket])
   (Control.Monad.IO.Class [(MonadIO ..)])
   (Data.Version [showVersion])
   (System.Environment [getEnvironment setEnv withArgs])

   ;; directory
   (System.Directory [makeAbsolute withCurrentDirectory])

   ;; filepath
   (System.FilePath [</>])

   ;; finkel-core
   (qualified Paths_finkel_core)

   ;; hspec
   (Test.Hspec)

   ;; Internal
   (Finkel.Tool.Command)
   (Finkel.Tool.Command.Help)
   (Finkel.Tool.Command.Version)
   (Finkel.Tool.Internal.CLI)
   (Finkel.Tool.Internal.Commit)
   (Finkel.Tool.Main)
   (TestAux)))

;;; Tests

(defn (:: mainTests Spec)
  (do cliTests
      helpTests
      evalTests
      replTests
      runTests
      sdistTests
      versionTests))

(defn (:: cliTests Spec)
  (do (describe "main with no argument"
        (it "should start repl"
          (do (<- tid (forkIO (main' [])))
              (threadDelay 20000)
              (killThread tid))))
      (describe "main with invalid command"
        (it "should show usage"
          (main' ["no-such-command"])))
      (describe "main with help command"
        (do (it "should show usage"
              (main' ["help"]))
            (it "should show help of repl command"
              (main' ["help" "repl"]))
            (it "should show help of make command"
              (main' ["help" "make"]))))
      (describe "main with make command"
        (it "should show help on --fnk-help"
          (main' ["make" "--fnk-help"])))
      (describe "main with version command"
        (it "should show version message by default"
          (main' ["version"])))))

(defn (:: helpTests Spec)
  (describe "help command"
    (it "should contain command names in usage message"
      (do (<- (, _ lns) (runTestIO (show-usage commands) []))
          (lept [messageShouldContain (shouldContain
                                       (unlines (tst-outputs lns)))])
          (messageShouldContain "eval")
          (messageShouldContain "repl")
          (messageShouldContain "make")
          (messageShouldContain "version")))))

(defn (:: evalTests Spec)
  (describe "eval command"
    (do (lefn [(failure [args]
                 (shouldThrow (main' args) anyExitFailure))])
        (it "should show help with --help"
          (main' ["eval" "--help"]))
        (it "should understand debug option"
          (main' ["eval" "--fnk-verbosity=3" "(not False)"]))
        (it "should evaluate '(+ 1 2 3 4 5)'"
          (main' ["eval" "(+ 1 2 3 4 5)"]))
        (it "should load module and evaluate given form"
          (main' ["eval" (++ "-i" test-data-dir) "LoadMe.fnk"
                  "(from-load-me \"LOADED\")"]))
        (it "should show error message when invoked without form"
          (failure ["eval"]))
        (it "should exit with non-zero on compile error"
          (failure ["eval" "(+ 1 True)"]))
        (it "should exit with non-zero on parse error"
          (failure ["eval" "(+ 1 True)))"])))))

(defn (:: replTests Spec)
  (lept [print-int (test-data "print-int.fnk")
         print-load-me (test-data "print-load-me.fnk")
         err001 (test-data "Err001.fnk")]
    (describe "repl command"
      (do (it "should show help on --help"
            (main' ["repl" "--help"]))
          (it "should show warning messages"
            (main' ["repl" (++ "--file=" print-int) "-v2" "-O"]))
          (it "should evaluate file contents after loading module"
            (main' ["repl" "--quiet" (++ "--file=" print-load-me) load-me]))
          (it "should show compilation error when loading invalid module"
            (shouldThrow
             (main' ["repl" "--quiet" (++ "--file=" print-int) print-int])
             anyExitFailure))
          (it "should show load failure with type error on load"
            (shouldThrow
             (main' ["repl" "--quiet" (++ "--file=" print-int) err001])
             anyExitFailure))
          (it "should compilain missing argument"
            (shouldThrow
             (main' ["repl" (++ "--file=" print-int) "--prompt"])
             anyExitFailure))))))

(defn (:: runTests Spec)
  (lefn [(run [args]
           (main' (: "run" args)))
         (failure [args]
           (shouldThrow (run args) anyExitFailure))]
    (describe "run command"
      (do (it "should show help on --help"
            (run ["--help"]))
          (it "should run run-me.fnk"
            (run ["-v0" (test-data "run-me.fnk")]))
          (it "should search directory with ghc option"
            (run ["-v0" (++ "-i" test-data-dir) "run-me.fnk"]))
          (it "should run given function"
            (run ["-v0" "--main" "main-two" (test-data "RunMeToo.fnk")]))
          (it "should pass arguments after `--'"
            (run ["-v0" "--main" "main-three" (test-data "RunMeToo.fnk")
                  "--" "dog"]))
          (it "should complain with malformed argument"
            (failure ["-v0" "--main"]))
          (it "should fail when input file does not exist"
            (failure ["-v0" "no-such-file.fnk"]))
          (it "should exit with status from given script"
            (failure ["-v0" "--main" "main-three" (test-data "RunMeToo.fnk")
                      "--" "elephant"]))))))

(defn (:: sdistTests Spec)
  (describe "sdist command"
    (do (lept [sdist (. main' (: "sdist"))
               failure (. (flip shouldThrow anyExitFailure) sdist)])
        (it "should show help message"
          (sdist ["--help"]))
        (it "should list options"
          (sdist ["--list-options"]))
        (it "should list sources"
          (sdist ["--list-sources=sources" (test-data "p02")]))
        (it "should make tarball with .cabal in current directory"
          (sdist []))
        (it "should make tarball with .cabal in given directory"
          (do (<- builddir (makeAbsolute (test-data "sdist")))
              (sdist ["--verbose=2"
                      (++ "--builddir=" builddir)
                      (test-data "p02")])))
        (it "should fail without .cabal file in current directory"
          (withCurrentDirectory ".." (failure [])))
        (it "should show error on invalid argument"
          (failure ["--foo"])))))

(defn (:: versionTests Spec)
  (describe "version command"
    (do (lefn [(version [args]
                 (fmap (. unlines (. tst-outputs snd))
                       (liftIO (runTestIO (versionMain args) []))))])
        (it "should not throw exceptions with git command"
          (do (<- commit-id (liftIO get-git-commit))
              (quietly (print commit-id))))
        (it "should not throw exception when git command is not found"
          (cond-expand
            ;; The use of `setEnv' may have problem under Windows ...
            [(/= :os "mingw32")
             (do (<- mb-commit-id (liftIO (with-tmp-env [(, "PATH" ".")]
                                            get-git-commit)))
                 (shouldBe mb-commit-id Nothing))]
            [otherwise
             (pendingWith "problem with `setEnv'")]))
        (it "should show ghc version in default message"
          (do (<- msg (version []))
              (shouldContain msg "ghc")))
        (it "should show \"--help\" in help message"
          (do (<- msg (version ["--help"]))
              (shouldContain msg "--help")))
        (it "should show numeric version with \"--numeric\" option"
          (do (<- msg (version ["--numeric"]))
              (shouldContain msg (showVersion Paths_finkel_core.version))))
        (it "should complain unrecognized option"
          (do (<- msg (version ["--no-such-option"]))
              (shouldContain msg "--no-such-option"))))))

;;; Auxiliary

(defn (:: with-tmp-env (-> [(, String String)] (IO a) (IO a)))
  [envvars act]
  (bracket getEnvironment
           (mapM_ (uncurry setEnv))
           (const (>> (mapM_ (uncurry setEnv) envvars) act))))

(defn (:: main' (-> [String] (IO ())))
  (. quietly (flip withArgs defaultMain)))

(defn (:: anyExitFailure (Selector ExitCode))
  [(ExitFailure _)] True
  [_] False)

(defn (:: test-data-dir FilePath)
  (</> "test" "data"))

(defn (:: test-data (-> FilePath FilePath))
  (</> test-data-dir))

(defn (:: load-me FilePath)
  (test-data "LoadMe.fnk"))
