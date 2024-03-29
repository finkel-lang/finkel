;;; -*- mode: finkel -*-
;;;; Finkel sdist command, to create tar.gz of cabal package

(:require Finkel.Core)

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

   ;; directory
   (System.Directory [withCurrentDirectory])

   ;; finkel-setup
   (Distribution.Simple.Finkel [finkelPPHandler])

   ;; Internal
   (Finkel.Tool.Internal.CLI)
   (Finkel.Tool.Internal.Exception)))


;;; Extra imports for version compatibility

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
                (either (. throwIO FinkelToolException)
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
