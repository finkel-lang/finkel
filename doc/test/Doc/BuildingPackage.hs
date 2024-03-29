;;; -*- mode: finkel -*-
;;;; Test codes for "building-package.rst".

(defmodule Doc.BuildingPackage
  (export spec)
  (import
   ;; base
   (Control.Exception [bracket])
   (Control.Monad [zipWithM_])
   (Data.Char (isSpace))
   (Data.Function [on])
   (Data.List [isPrefixOf sort])
   (System.Exit [(ExitCode ..)])
   (System.Process [(CreateProcess ..) createProcess proc waitForProcess])

   ;; directory
   (System.Directory
    [canonicalizePath doesDirectoryExist doesFileExist getTemporaryDirectory
     listDirectory removeDirectoryRecursive])

   ;; filepath
   (System.FilePath [</> takeExtension takeFileName])

   ;; hspec
   (Test.Hspec [(Spec) describe expectationFailure it pendingWith shouldBe])

   ;; Internal
   (Doc.TestAux)))


;;; Spec

(defn (:: spec Spec)
  (describe "building cabal package"
    (it "matches package made with hsfiles template"
      (case-do get-build-tool
        Cabal (pendingWith "not running with cabal-install")
        Raw (pendingWith "not running with raw build")
        Stack compare-new-package))))


;;; Auxiliary

(defn (:: compare-new-package (IO ()))
  (bracket make-tmp-dir
           remove-tmp-dir
           compare-package-dirs))

(defn (:: make-tmp-dir (IO (, FilePath String)))
  (fmap (flip (,) "my-new-package") getTemporaryDirectory))

(defn (:: remove-tmp-dir (-> (, FilePath String) (IO ())))
  (. removeDirectoryRecursive (uncurry </>)))

(defn (:: compare-package-dirs (-> (, FilePath String) (IO ())))
  [(, tmpdir pkgname)]
  (case-do (stack-new tmpdir pkgname)
    ;; Ignoring "stack.yaml", since the file contains version number of the
    ;; stack executable, which may change when the stack was upgraded.
    ;;
    ;; XXX: This approach will make the tests to pass. However, cannot detect
    ;; the validity of stack.yaml file, so consider taking different way to pass
    ;; the test.
    (Right ExitSuccess) (compare-directory-contents
                         ["stack.yaml"]
                         (</> "include" "building-package" pkgname)
                         (</> tmpdir pkgname))
    (Right ec) (expectationFailure (++ "stack new failed with " (show ec)))
    (Left msg) (expectationFailure msg)))

(defn (:: compare-directory-contents (-> [FilePath] FilePath FilePath (IO ())))
  "Recursively compare directory contents."
  [ignored path1 path2]
  (if (elem (takeFileName path1) ignored)
      (return ())
      (do (<- path1-is-file (doesFileExist path1))
          (<- path2-is-file (doesFileExist path2))
          (if (&& path1-is-file path2-is-file
                  (notElem (takeFileName path1) ignored))
              (do (<- contents1 (readFile path1))
                  (<- contents2 (readFile path2))
                  (on shouldBe trim contents1 contents2))
              (do (<- path1-is-dir (doesDirectoryExist path1))
                  (<- path2-is-dir (doesDirectoryExist path2))
                  (if (&& path1-is-dir path2-is-dir)
                      (do (<- ls1 (list-directory path1))
                          (<- ls2 (list-directory path2))
                          (lept [add-dir (. map </>)
                                 ls1' (add-dir path1 ls1)
                                 ls2' (add-dir path2 ls2)])
                          (zipWithM- (compare-directory-contents ignored)
                                     ls1' ls2'))
                      (expectationFailure
                       (++ "differed: " path1 ", " path2))))))))

(defn (:: trim (-> String String))
  "Trim white spaces at the beginning and end."
  (. (dropWhile isSpace) reverse (dropWhile isSpace) reverse))

(defn (:: list-directory (-> FilePath (IO [FilePath])))
  "List directory contents, filter outs some ignored files."
  (lefn [(ignored [path]
           (&& (/= ".stack-work" path)
               (/= ".tix" (takeExtension path))))]
    (. (fmap (. sort (filter ignored))) listDirectory)))

(defn (:: stack-new (-> FilePath String (IO (Either String ExitCode))))
  "Run stack new command to generate new package."
  [dir pkgname]
  (do (<- template get-template-path)
      (<- mb-resolver (get-resolver-version pkgname))
      (case mb-resolver
        (Just resolver) (fmap Right
                              (run (Just dir)
                                   "stack" ["--resolver" resolver "--silent"
                                            "new" pkgname
                                            "--omit-packages" template]))
        Nothing (return (Left "Failed to get the resolver version")))))

(defn (:: get-template-path (IO FilePath))
  "Get canonicalized template path."
  ;; XXX: May move template to separate repository to support
  ;; `github:finkel-lang/simple' style template argument.
  ;;
  ;; See: https://docs.haskellstack.org/en/stable/GUIDE/#templates
  ;;
  (canonicalizePath (</> ".." "finkel-tool" "finkel.hsfiles")))

(defn (:: get-resolver-version (-> String (IO (Maybe String))))
  "Get stack resolver version from YAML file used in `my-new-package'."
  [pkgname]
  (lept [yaml-path (</> "include" "building-package" pkgname "stack.yaml")
         resolver-line (. (concatMap words)
                          (filter (isPrefixOf "resolver:"))
                          lines)]
    (case-do (fmap resolver-line (readFile yaml-path))
      [_ version] (return (Just version))
      _ (return Nothing))))

(defn (:: run (-> (Maybe String) String [String] (IO ExitCode)))
  "Run command and wait."
  [mb-dir cmd args]
  (do (<- (, _mbin _mbout _mberr ph)
        (createProcess ((proc cmd args) {(= cwd mb-dir)})))
      (waitForProcess ph)))
