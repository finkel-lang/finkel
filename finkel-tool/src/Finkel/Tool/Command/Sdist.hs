;;; -*- mode: finkel -*-

;;; Finkel sdist command, to create tar.gz of cabal package
;;;
;;; XXX: Consider removing this command when the support for "*.fnk" file
;;; extension is dropped.

(defmodule Finkel.Tool.Command.Sdist
  (export sdistMain)
  (import-when [:compile]
    ;; finkel-core
    (Finkel.Prelude))
  (import
   ;; base
   (Control.Exception [throwIO])
   (Control.Monad [>=>])
   (Control.Monad.IO.Class [liftIO])
   (System.Environment [getProgName])

   ;; Cabal
   (Distribution.PackageDescription.Configuration [flattenPackageDescription])

   (Distribution.Simple.BuildPaths [srcPref])
   (Distribution.Simple.Command [(CommandParse ..) (CommandUI ..)
                                 commandParseArgs])
   (Distribution.Simple.PreProcess [knownSuffixHandlers])
   (Distribution.Simple.Setup [(SDistFlags ..) defaultSDistFlags sdistCommand])
   (Distribution.Simple.SrcDist [sdist])
   (Distribution.Simple.Utils [findPackageDesc])

   (Distribution.Simple.PreProcess [PPSuffixHandler (PreProcessor ..)
                                    mkSimplePreProcessor])

   ;; directory
   (System.Directory [withCurrentDirectory])

   ;; Internal
   (Finkel.Tool.Internal.CLI)
   (Finkel.Tool.Internal.Exception)))


;;; Extra imports for version compatibility

(cond-expand
  [(:min-version "Cabal" 3 12 0)
   (:begin
     (import Distribution.Simple.Errors (exceptionMessage))
     (import Distribution.Simple.PreProcess ((Suffix ..))))]
  [otherwise
   (:begin)])

(cond-expand
  [(:min-version "Cabal" 3 8 0)
   (import Distribution.Simple.PreProcess (unsorted))]
  [otherwise
   (:begin)])

(cond-expand
  [(:min-version "Cabal" 3 0 1)
   (:begin)]
  [otherwise
   (import Distribution.Simple.Configure (findDistPrefOrDefault))])

(cond-expand
  [(:min-version "Cabal" 3 0 1)
   (import Distribution.Simple.Flag (fromFlag))]
  [(:min-version "Cabal" 2 4 0)
   (import Distribution.Simple.Flag (fromFlag toFlag))]
  [otherwise
   (import Distribution.Simple.Setup (fromFlag toFlag))])

(cond-expand
  [(:min-version "Cabal" 3 8 0)
   (import Distribution.Simple.PackageDescription
           (readGenericPackageDescription))]
  [(:min-version "Cabal" 2 2 0)
   (import Distribution.PackageDescription.Parsec
           (readGenericPackageDescription))]
  [otherwise
   (import Distribution.PackageDescription.Parse
           (readGenericPackageDescription))])

;;; Main action

(defn (:: sdistMain (=> (CLI m) (-> [String] (m ()))))
  [args]
  (lefn [(write-tgzs [parsed]
           (do (lept [(, update-flags non-opts) parsed
                      flags1 (update-flags defaultSDistFlags)])
               (<- flags2 (update-dist-pref flags1))
               (lept [verbosity (fromFlag (sDistVerbosity flags2))
                      write (write-tgz verbosity flags2)])
               (if (null non-opts)
                 (write ".")
                 (mapM- write non-opts))))
         (write-tgz [verbosity flags dir]
           (>>= (findPackageDesc dir)
                (either (cond-expand
                          [(:min-version "Cabal" 3 12 0)
                           (. throwIO FinkelToolException exceptionMessage)]
                          [otherwise
                           (. throwIO FinkelToolException)])
                        (>=> (readGenericPackageDescription verbosity)
                             (write-tgz-2 flags dir)))))
         (write-tgz-2 [flags dir descr]
           (lept [sd (run-sdist (flattenPackageDescription descr) flags)]
             (if (== dir ".")
               sd
               (withCurrentDirectory dir sd))))
         (update-dist-pref [flags]
           (cond-expand
             [(:min-version "Cabal" 3 0 0)
              (pure flags)]
             [otherwise
              ;; Until Cabal 3.x, seems like the "sDistDistPref" field was set
              ;; by "configure" command, setting it manually.
              (do (<- pref (findDistPrefOrDefault (sDistDistPref flags)))
                  (pure (flags {(= sDistDistPref (toFlag pref))})))]))
         (run-sdist [pd flags]
           (lept [sufs (: finkelPPHandler knownSuffixHandlers)]
             (cond-expand
               [(:min-version "Cabal" 3 4 0)
                (sdist pd flags srcPref sufs)]
               [otherwise
                (sdist pd Nothing flags srcPref sufs)])))
         (my-sdist-cmd
           (sdistCommand
            {(= commandUsage
               (\pname
                 (++ "Usage: " pname " sdist [FLAGS] [DIRS]\n")))}))]
    (liftIO
     (case (commandParseArgs my-sdist-cmd False args)
       (CommandHelp f) (>>= getProgName (. putStrLn f))
       (CommandList os) ($ putStr unlines os)
       (CommandReadyToGo parsed) (write-tgzs parsed)
       (CommandErrors es) (throwIO (ArgumentErrors "sdist" es))))))

;;; Preprocessor suffix handler to merely register files with @"*.fnk"@ files.
(defn (:: finkelPPHandler PPSuffixHandler)
  (where (, suffix do_nothing_pp)
    (defn suffix
      (cond-expand
        [(:min-version "Cabal" 3 12 0) (Suffix "fnk")]
        [otherwise "fnk"]))
    (defn do_nothing_pp [_ _ _]
      (cond-expand
        [(:min-version "Cabal" 3 8 0)
         (PreProcessor {(= platformIndependent True)
                        (= ppOrdering unsorted)
                        (= runPreProcessor (mkSimplePreProcessor
                                            (\ _ _ _ (pure ()))))})]
        [otherwise
         (PreProcessor {(= platformIndependent True)
                        (= runPreProcessor (mkSimplePreProcessor
                                            (\ _ _ _ (pure ()))))})]))))
