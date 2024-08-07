;;; -*- mode: finkel -*-
;;;; Auxiliary codes for tests.

(defmodule Doc.TestAux
  (export (BuildTool ..)
          get-build-tool
          get-stack-resolver
          get-ghc-lib
          run-console-tests
          run-console-test
          remove-compiled)
  (import-when [:compile]
   ;; finkel-core
   (Finkel.Prelude))
  (import
   ;; base
   (Control.Exception [catch throw])
   (Control.Monad [unless when])
   (Data.List [isSubsequenceOf isPrefixOf sort])
   (Data.Maybe [fromMaybe])
   (Data.Version [(Version ..) parseVersion])
   (System.Environment [getExecutablePath lookupEnv])
   (System.Exit [(ExitCode ..)])
   (System.IO [hGetContents hGetLine])
   (Text.ParserCombinators.ReadP [readP-to-S])

   ;; directory
   (System.Directory [listDirectory removeFile])
   (System.Directory.Internal.Prelude [isDoesNotExistError])

   ;; filepath
   (System.FilePath [</> <.> takeExtension])

   ;; hspc
   (Test.Hspec
    [(Expectation) (Spec) expectationFailure it pendingWith runIO shouldBe])

   ;; process
   (System.Process
    [(CreateProcess ..) (StdStream ..) createProcess proc waitForProcess])))

(cond-expand
  [(<= 900 :ghc)
   (import GHC.Settings.Config (cProjectVersion))]
  [otherwise
   (import Config (cProjectVersion))])

;;; Exported

(defn (:: remove-compiled (-> [String] (IO ())))
  "Remove given compiled files if exist.
Remove the given file @FILE@, @FILE.o@, and @FILE.hi@."
  [name]
  (lefn [(go0 [exe]
           (mapM go1 [exe
                      (<.> exe "o")
                      (<.> exe "hi")
                      (<.> exe "dyn_o")
                      (<.> exe "dyn_hi")]))
         (go1 [file]
           (catch (removeFile file)
             (\e
               (unless (isDoesNotExistError e)
                 (throw e)))))]
    (mapM_ go0 name)))

(defn (:: get-build-tool (IO BuildTool))
  "Get the running `BuildTool'."
  (do (<- me getExecutablePath)
      (pure (cond
              [(isSubsequenceOf ".stack" me) Stack]
              [(isSubsequenceOf "dist-newstyle" me) Cabal]
              [otherwise Raw]))))

(defn (:: get-stack-resolver (IO String))
  "Return @RESOLVER@ environment variable."
  (fmap (fromMaybe "lts-16") (lookupEnv "RESOLVER")))

(defn (:: get-ghc-lib (IO (Maybe FilePath)))
  (do (<- build-tool get-build-tool)
      (<- p0
        (case build-tool
          Cabal (return (proc "cabal" ["v2-exec" "--" "ghc" "--print-libdir"]))
          Stack (do (<- resolver get-stack-resolver)
                    (return (proc "stack" ["--resolver" resolver "exec" "--"
                                           "ghc" "--print-libdir"])))
          Raw (return (proc "ghc" ["--print-libdir"]))))
      (lept [p1 (p0 {(= std-out CreatePipe)})])
      (<- (, _mb-in mb-out _mb-err ph) (createProcess p1))
      (<- _ec (waitForProcess ph))
      (case mb-out
        (Just out) (fmap pure (hGetLine out))
        Nothing (return Nothing))))

(:doc "List of pair of name and condition for skipping console test.

Pair of cosole filename (without the directory, with extension) and a function
taking GHC version. If the function evaluates to `True', the console test will
be skipped.")
(type ConsoleSkip [(, String (-> Version BuildTool Bool))])

(defn (:: get-console-files (-> FilePath (IO [FilePath])))
  (lept [is-console-file (. (== ".console") takeExtension)]
    (. (fmap (. sort (filter is-console-file))) listDirectory)))

(defn (:: run-console-tests (-> FilePath ConsoleSkip Spec))
  "Run test for @*.console@ files in given directory."
  [dir skips]
  (do (lefn [(test [build-tool file]
               (it (++ "runs " file " successfully")
                 (case (do (<- f (lookup file skips))
                           (<- v mb-ghc-version)
                           (return (f v build-tool)))
                   (Just True) (pendingWith "skipped")
                   _ (run-console-test dir file))))
             (mb-ghc-version
               (case (filter (. null snd)
                             (readP-to-S parseVersion cProjectVersion))
                 (: (, v _) _) (Just v)
                 _ Nothing))])
      (<- console-files (runIO (get-console-files dir)))
      (<- build-tool (runIO get-build-tool))
      (mapM_ (test build-tool) console-files)))

(data BuildTool
  ;; | This package is build with `cabal-install'.
  Cabal

  ;; | This package is build with `stack'.
  Stack

  ;; | This package is build with raw invokation of available commands.
  Raw
  (deriving Eq))

(defn (:: run-console-test (-> FilePath ; ^ Directory to run the command.
                               FilePath ; ^ Console file.
                               Expectation))
  [dir file]
  (do (<- contents (readFile (</> dir file)))
      (<- build-tool get-build-tool)
      (<- finkel
        (case build-tool
          Cabal (return (, "cabal" ["v2-exec" "-v0" "--" "finkel"]))
          Stack (do (<- resolver get-stack-resolver)
                    (return (, "stack" ["--resolver" resolver "--silent"
                                        "exec" "--" "finkel"])))
          Raw (return (, "finkel" []))))
      (lept [tests (parse-console contents)
             aliases [(, "finkel" finkel)]])
      (mapM_ (run-console dir aliases) tests)))

(data ConsoleTest
  (ConsoleTest String           ; ^ The command.
               [String]         ; ^ Arguments passed to the command.
               [String])        ; ^ Expected output lines.
  (deriving Eq Show))

(defn (:: run-console
        (-> String                           ; ^ Directory to run the test
            [(, String (, String [String]))] ; ^ Alias for command.
            ConsoleTest                      ; ^ The test.
            (IO ())))
  [dir aliases (ConsoleTest cmd args expected)]
  (do (lept [(, cmd' args') (case (lookup cmd aliases)
                              (Just (, rc ra)) (, rc (++ ra args))
                              Nothing (, cmd args))])
      (<- (, _mbin mbout mberr ph)
        (createProcess ((proc cmd' args') {(= cwd (Just dir))
                                           (= std-out CreatePipe)})))
      (<- ec (waitForProcess ph))
      (<- errs (case mberr
                 (Just hdl) (hGetContents hdl)
                 Nothing (return "")))
      (when (not (null errs))
        (putStrLn (++ "stderr: " errs)))
      (<- outs0 (case mbout
                 (Just out) (fmap lines (hGetContents out))
                 Nothing (return [])))
      (lept [outs1 (remove-loaded-package-env outs0)])
      (case ec
        ExitSuccess (shouldBe (unlines outs1) (unlines expected))
        _ (expectationFailure (++ "Got exit code " (show ec))))))

(defn (:: remove-loaded-package-env (-> [String] [String]))
  "In ghc-8.10.1, the `Loaded package environment from ...' message has changd
  its output from stderr to stdout. Removing the message line from command
  outputs."
  (filter (. not (isPrefixOf "Loaded package environment from"))))

(defn (:: parse-console (-> String [ConsoleTest]))
  "Parse the contents of console file."
  (lefn [(go [ls]
           (case (span is-cmd-line ls)
             (, (: cl _) r0) (case (words (drop 1 cl))
                               (: cmd args) (case (break is-cmd-line r0)
                                              (, os r1) (: (ct cmd args os)
                                                           (go r1)))
                               _ (go r0))
             _ []))
         (ct ConsoleTest)]
    (. go lines)))

(defn (:: is-cmd-line (-> String Bool))
  "True if the line starts with @$@."
  [(: #'$ _)] True
  [_] False)
