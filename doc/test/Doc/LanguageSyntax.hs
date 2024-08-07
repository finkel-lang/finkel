;;; -*- mode: finkel -*-
;;;; Test codes for "language-syntax.rst"

;;; This module contains codes to tests the code snippets shown in the "Language
;;; Syntax" chapter of the documentation. The tests parses expressions and
;;; declarations from file, and parse the Haskell code and Finkel code, then
;;; compare the parsed results with `showPpr'.

(defmodule Doc.LanguageSyntax
  (export spec)
  (import-when [:compile]
    ;; finkel-lang
    (Finkel.Prelude))
  (import
   ;; base
   (Data.List [sort])

   ;; directory
   (System.Directory [listDirectory])

   ;; filepath
   (System.FilePath [</> <.> dropExtension takeExtension])

   ;; finkel-kernel
   (Language.Finkel.Builder [(Builder) evalBuilder])
   (Language.Finkel.Reader [parseSexprs])
   (qualified Language.Finkel.Syntax as FnkParser)

   ;; hspec
   (Test.Hspec [(Spec) (SpecWith) beforeAll describe expectationFailure it runIO
                shouldBe])

   ;; Internal
   (Doc.TestAux)))

;; ghc

(import GHC (DynFlags getSessionDynFlags runGhc))

(cond-expand
  [(<= 902 :ghc)
   (import GHC.Driver.Ppr (showPpr))]
  [(<= 900 :ghc)
   (import GHC.Utils.Outputable (showPpr))]
  [otherwise
   (import Outputable (showPpr))])

(cond-expand
  [(<= 900 :ghc)
   (:begin
     (import GHC.Data.FastString (fsLit))
     (import GHC.Utils.Outputable ((Outputable ..)))
     (import GHC.Parser.Lexer ((P ..) (ParseResult ..)))
     (import GHC.Types.SrcLoc ((GenLocated ..) mkRealSrcLoc interactiveSrcSpan))
     (import GHC.Data.StringBuffer (hGetStringBuffer))
     (import qualified GHC.Parser as GhcParser)
     (import qualified GHC.Parser.Lexer as GhcLexer))]
  [otherwise
   (:begin
     (import FastString (fsLit))
     (import Outputable ((Outputable ..)))
     (import Lexer ((P ..) (ParseResult ..)))
     (import SrcLoc ((GenLocated ..) mkRealSrcLoc interactiveSrcSpan))
     (import StringBuffer (hGetStringBuffer))
     (import qualified Parser as GhcParser)
     (import qualified Lexer as GhcLexer))])

(cond-expand
  [(<= 904 :ghc)
   (:begin
     (import GHC.Driver.Config.Parser (initParserOpts))
     (import GHC.Parser.PostProcess ((ECP ..) runPV)))]
  [(<= 902 :ghc)
   (:begin
     (import GHC.Driver.Config (initParserOpts))
     (import GHC.Parser.PostProcess ((ECP ..) runPV)))]
  [(<= 900 :ghc)
   (import GHC.Parser.PostProcess (runECP-P))]
  [otherwise
   (import RdrHsSyn (runECP-P))])


;;; Functions

(defn (:: spec Spec)
  (beforeAll
   ;; Running `runGhc' to set the `unsafeGlobalDynFlags' with `initGhcMonad'.
   (do (<- mb-ghc-lib get-ghc-lib)
       (runGhc mb-ghc-lib getSessionDynFlags))
   (describe "language syntax"
     (do (lept [expFromECP (cond-expand
                             ;; The function `runECP_P' was added in ghc 8.10.x,
                             ;; and removed in ghc 9.2.1. Using explicit lambda
                             ;; for `runPV' and `unECP' to make the type
                             ;; concrete.
                             [(<= 902 :ghc) (\p (runPV (unECP p)))]
                             [otherwise runECP-P])])
         (describe "expression"
           (parser-tests "expr" (Parsers (>>= GhcParser.parseExpression
                                              expFromECP)
                                         FnkParser.parseExpr)))
         (describe "declaration"
           (parser-tests "decl" (Parsers (fmap pure GhcParser.parseDeclaration)
                                         FnkParser.parseTopDecls)))
         (describe "module"
           (parser-tests "module" (Parsers GhcParser.parseModule
                                           (fmap (L interactiveSrcSpan)
                                                 FnkParser.parseModule))))
         (describe "import"
           (parser-tests "import" (Parsers (fmap pure GhcParser.parseImport)
                                           FnkParser.parseImports)))
         (describe "ffi"
           (parser-tests "ffi" (Parsers (fmap pure GhcParser.parseDeclaration)
                                        FnkParser.parseTopDecls)))))))

(defn (:: parser-tests
        (=> (Outputable a) (-> FilePath (Parsers a) (SpecWith DynFlags))))
  [subdir parsers]
  (do (lefn [(run-it [name]
               (it (++ "should parse tests in " name)
                 (\dflags
                   (parser-test dflags parsers (</> (language-dir subdir) name)))))])
      (<- files (runIO (list-base-names (language-dir subdir))))
      (mapM_ run-it files)))

(data (Parsers a)
  (Parsers {(:: hs-parser (P a))
            (:: fnk-parser (Builder a))}))

(defn (:: parser-test
        (=> (Outputable a) (-> DynFlags (Parsers a) FilePath (IO ()))))
  [dflags parsers basename]
  (do (lept [fnk (<.> basename "fnk")
             hs (<.> basename "hs")])
      (<- buf (hGetStringBuffer hs))
      (lept [loc (mkRealSrcLoc (fsLit "<test>") 1 1)
             ps (cond-expand
                  [(<= 902 :ghc)
                   (GhcLexer.initParserState (initParserOpts dflags) buf loc)]
                  [otherwise
                   (GhcLexer.mkPState dflags buf loc)]) ])
      (case (GhcLexer.unP (hs-parser parsers) ps)
        (POk _st hres) (do (<- fstr (parse-fnk dflags parsers fnk))
                           (shouldBe fstr (showPpr dflags hres)))
        _ (expectationFailure "failed to parse haskell code"))))

(defn (:: parse-fnk
        (=> (Outputable a)
            (-> DynFlags (Parsers a) FilePath (IO String))))
  [dflags parser path]
  (do (<- buf (hGetStringBuffer path))
      (<- (, forms _sp) (parseSexprs (Just path) buf))
      (case (evalBuilder dflags False (fnk-parser parser) forms)
        (Right fexp) (return (showPpr dflags fexp))
        (Left err) (return (show err)))))

(defn (:: language-dir (-> String FilePath))
  [subdir]
  (</> "include" "language-syntax" subdir))

(defn (:: list-base-names (-> FilePath (IO [FilePath])))
  [dir]
  (do (<- files (listDirectory dir))
      (return
       (sort [(dropExtension file)
              | (<- file files) (== ".fnk" (takeExtension file))]))))
