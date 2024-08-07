;;; -*- mode: finkel -*-
;;;; Tests for REPL macros

(defmodule ReplMacroTest
  (export replMacroTests)
  (import-when [:compile]
    ;; finkel-core
    (Finkel.Prelude))
  (import
   ;; base
   (Control.Exception [catch throw])
   (Data.List [intercalate isSubsequenceOf])
   (System.IO.Error [isDoesNotExistError])

   ;; filepath
   (System.FilePath [</>])

   ;; directory
   (System.Directory [getCurrentDirectory getTemporaryDirectory removeFile])

   ;; hspec
   (Test.Hspec)

   ;; finkel-core
   (Finkel.Prelude)

   ;; Internal
   (TestAux)))

(defn (:: replMacroTests (-> EvalTestFns Spec))
  [etf]
  (cond-expand
    [(== :os "mingw32")
     (it "should skip under Windows"
       (pendingWith "Windows not yet supported"))]
    [otherwise (replMacroTests-1 etf)]))

(defn (:: replMacroTests-1 (-> EvalTestFns Spec))
  [(EvalTestFns {(= etf-ok ok) (= etf-ng ng) (= etf-satisfy satisfy)})]
  (do (<- current-dir (runIO getCurrentDirectory))
      (<- tmp-dir (runIO getTemporaryDirectory))
      (lefn [(testdata [name]
               (</> "test" "data" name))
             (delines (intercalate "\n"))
             (m02-dot-hs (</> tmp-dir "m02.hs"))
             (m02-dot-hi (</> tmp-dir "m02.hi"))
             (main-dot-o (</> tmp-dir "Main.o"))
             (clear-m02-files
               (mapM_ remove-if-exist [m02-dot-hs m02-dot-hi main-dot-o]))
             (with-right [form test]
               (satisfy form (either (const False) test)))
             (with-left [form test]
               (satisfy form (either test (const False))))
             (with-left-subseq [form str0]
               (satisfy form (either (isSubsequenceOf str0) (const False))))])
      (beforeAll_
       clear-m02-files
       (afterAll_
        clear-m02-files
        (do
          ;; !
          (ok '(repl-macro ! echo foo) "")

          ;; ?, help
          (with-right '(repl-macro ?)
            (isSubsequenceOf ",type EXPR"))
          (with-right '(repl-macro help)
            (isSubsequenceOf "show this help"))

          ;; browse
          (with-right '(repl-macro browse Unsafe.Coerce)
            (isSubsequenceOf "Unsafe.Coerce.unsafeCoerce :: a -> b"))
          (with-left-subseq '(repl-macro browse foo bar)
            "browse: invalid form `foo'")

          ;; cd
          (ok `(repl-macro cd ,current-dir) "")
          (ok `(repl-macro cd ,(testdata "")) "")
          (ok '(repl-macro cd ../../) "")
          (with-left `(repl-macro cd (foo bar) buzz)
            (isSubsequenceOf "invalid form"))

          ;; expand
          (ok '(repl-macro expand
                (defn f (where g (defn g [x] (print (++ "g: " x))))))
              "(= f (where g (defn g [x] (print (++ \"g: \" x)))))")
          (ok '(repl-macro expand) "")
          (with-left-subseq '(repl-macro expand foo bar)
            "expand: invalid form `foo'")

          ;; expand!
          (ok '(repl-macro expand!
                (defn f (where g (defn g [x] (print (++ "g: " x))))))
              "(= f (where g (= g x (print (++ \"g: \" x)))))")

          ;; info
          (ok '(repl-macro info putStr)
              (cond-expand
                [(<= 910 :ghc)
                 "putStr :: String -> IO () \t-- Defined in ‘GHC.Internal.System.IO’"]
                [otherwise
                 "putStr :: String -> IO () \t-- Defined in ‘System.IO’"]))
          (ok '(repl-macro info ++)
              (cond-expand
                [(<= 910 :ghc)
                 "(++) :: [a] -> [a] -> [a] \t-- Defined in ‘GHC.Internal.Base’\n\
                 \infixr 5 ++"]
                [otherwise
                 "(++) :: [a] -> [a] -> [a] \t-- Defined in ‘GHC.Base’\n\
                 \infixr 5 ++"]))
          (with-right '(repl-macro info ())
            (isSubsequenceOf "instance Show ()"))
          (with-right '(repl-macro info [])
            (isSubsequenceOf
             (cond-expand
               [(<= 906 :ghc)
                "data List a = [] | a : [a]"]
               [otherwise
                "data [] a = [] | a : [a]"])))

          ;; kind
          (ok '(repl-macro kind Maybe)
              "Maybe :: * -> *")
          (with-left-subseq '(repl-macro kind (foo bar) buzz)
            "kind: invalid form `(foo bar)'")

          ;; pwd
          (ok '(repl-macro pwd) (show current-dir))

          ;; set
          (ok '(repl-macro set -foo -bar -buzz) "")
          (ok '(repl-macro set --fnk-verbose=3) "")
          (ok '(repl-macro set --fnk-verbose=1) "")
          (ng '(repl-macro set)
              "<finkel generated code>: error: set: empty form")

          ;; show
          (ok '(defn (:: f1 (-> Int Int)) [n] (+ n 1))
              "; f1 :: Int -> Int")
          (with-right '(repl-macro show bindings)
            (. (elem "f1 :: Int -> Int = _") lines))
          (with-right '(class (C a) (:: cm1 (-> a Int)))
            (. (elem "; Class ‘C’") lines))
          (with-right '(repl-macro show bindings)
            (. (elem "class C a") lines))
          (lept [interpreter-backend-line
                 (cond-expand
                   [(<= 906 :ghc)
                    ";  backend: byte-code interpreter"]
                   [(<= 902 :ghc)
                    ";  backend: Interpreter"]
                   [otherwise
                    ";  backend: HscInterpreted"])])
          (ok '(repl-macro show context)
              (delines
               ["; context"
                ";  IIDecl: import Prelude"]))
          (ok '(repl-macro show dflags)
              (delines
               ["; dflags:"
                ";  ghcLink: LinkInMemory"
                ";  ghcMode: CompManager"
                interpreter-backend-line
                ";  objectDir: Nothing"
                ";  homeUnitId: main"
                ";  forceRecomp: False"]))
          (ok '(:begin
                (repl-macro set -odir /tmp)
                (repl-macro show dflags))
              (delines
               ["; dflags:"
                ";  ghcLink: LinkInMemory"
                ";  ghcMode: CompManager"
                interpreter-backend-line
                ";  objectDir: Just \"/tmp\""
                ";  homeUnitId: main"
                ";  forceRecomp: False"]))
          (ok '(repl-macro show hpt)
              "show: no home package table found")
          (ok '(repl-macro show language)
              (delines
               (cond-expand
                 [(<= 902 :ghc)
                  ["base language is: GHC2021"
                   "with the following modifiers:"]]
                 [otherwise
                  ["base language is: Haskell2010"
                   "with the following modifiers:"
                   "  -XNoDatatypeContexts"
                   "  -XNondecreasingIndentation"]])))
          ;; show linker command uses 'showLinkerState' from ghc package,
          ;; which does printing action, so not returning 'String' value.
          (ok '(repl-macro show linker) "")
          (with-right '(repl-macro show macros)
            (. (elem ";  defmacroM'") lines))
          (ok '(repl-macro show modules) "")
          (with-right '(repl-macro show options)
            (isSubsequenceOf "-fimplicit-import-qualified"))
          (with-right '(repl-macro show options!)
            (isSubsequenceOf "-Wno-orphans"))
          (with-right '(repl-macro show packages)
            (isSubsequenceOf "; packages"))
          (with-right '(:begin
                        (repl-macro set -hide-package bytestring)
                        (repl-macro show packages))
            (isSubsequenceOf "hiding"))
          (ok '(repl-macro show paths)
              (concat
               ["; current working directory:\n"
                ";   " current-dir "\n"
                "; module import search paths:\n"
                ";    ."]))
          (ok '(repl-macro show targets)
              "; targets: none")
          (with-left '(repl-macro show (foo bar) buzz)
            (isSubsequenceOf "targets"))

          ;; type
          (ok '(repl-macro type putStrLn)
              "putStrLn :: String -> IO ()")
          (ok '(repl-macro type (foldr + (:: 0 Int)))
              "(foldr + (:: 0 Int)) :: Foldable t => t Int -> Int")
          (ok '(repl-macro type 'x)
              "(:quote x) :: Language.Finkel.Form.Code")
          (with-left-subseq '(repl-macro type (foo bar) buzz)
            "type: invalid form `(foo bar)'")

          ;; verbose
          (ok '(repl-macro verbose) "Verbosity level is 1")
          (ok '(repl-macro verbose 2)
              "Verbosity level set to 2")
          (ok '(repl-macro verbose 1)
              "Verbosity level set to 1")

          ;; load and reload
          (lept [m01-dot-hs (testdata "m01.hs")])
          (ok `(:begin
                 (repl-macro load ,(make-symbol m01-dot-hs))
                 main)
              "=== m01.fnk ===")
          (ok '(repl-macro reload)
              "; reloaded test/data/m01.hs")
          (ok '(repl-macro browse)
              (delines
               ["main :: IO ()"
                "foo :: String"
                "bar :: Int -> Int"]))

          (ok '(repl-macro show targets)
              (cond-expand
                [(<= 904 :ghc) "; targets: main:test/data/m01.hs"]
                [(<= 902 :ghc) "; targets: test/data/m01.hs"]
                [otherwise     "; targets: *test/data/m01.hs"]))
          (with-right '(repl-macro show context)
            (isSubsequenceOf "IIModule: Main"))

          (ok `(writeFile ,m02-dot-hs ";;; m02.hs\n(defn main (print True))")
              "")
          (cond-expand
            [(== :os "darwin")
             (describe "evaluate (repl-macro load m02.fnk)"
               (it "should skip under darwin"
                 (pendingWith "OSX not supported yet")))]
            [otherwise
             (ok `(:begin
                    (repl-macro load ,(make-symbol m02-dot-hs))
                    main)
                 "True")])
          (ok `(writeFile ,m02-dot-hs ";;; m02.hs\n(defn main (print False))")
              "")

          (cond-expand
            [(== :os "darwin")
             (describe "evaluate (repl-macro reload)"
               (it "should skip under darwin"
                 (pendingWith "OSX not supported yet")))]
            [otherwise
             (ok '(repl-macro reload)
                 (++ "; reloaded " m02-dot-hs))])
          (cond-expand
            [(== :os "darwin")
             (describe "evaluate main"
               (it "should skip under darwin"
                 (pendingWith "OSX not supported yet")))]
            [otherwise
             (ok 'main "False")])

          ;; Compiling object code
          (cond-expand
            [(== :os "darwin")
             (describe "Compiling object code"
               (it "should be skipped under darwin"
                 (pendingWith "OSX not supported yet")))]
            [otherwise
             (do
               (ok `(writeFile ,m02-dot-hs ";;; m02.hs\n(defn main (print True))") "")
               (ok `(:begin
                      (repl-macro set -fobject-code)
                      (repl-macro load ,m02-dot-hs)
                      main)
                   "True")
               (ok '(repl-macro reload)
                   (++ "; reloaded " m02-dot-hs)))])

          ;; Errors
          (with-left-subseq '(repl-macro load (foo bar))
            "load: not a FilePath: (foo bar)")
          (with-left-subseq '(repl-macro load (foo bar) buzz)
            "load: invalid form `(foo bar)'")

          ;; Calling functions from Prelude after load error:
          (ok '(:begin
                (repl-macro load /no/such/file.fnk)
                (print (not False)))
              "True")

          ;; Errors
          (with-left-subseq '(repl-macro info (foo bar))
            "info: invalid form `(foo bar)'")
          (with-left-subseq '(repl-macro)
            "invalid args: nil"))))))

(defn (:: remove-if-exist (-> FilePath (IO ())))
  [path]
  (catch (removeFile path)
    (\e (if (isDoesNotExistError e)
          (return ())
          (throw e)))))
