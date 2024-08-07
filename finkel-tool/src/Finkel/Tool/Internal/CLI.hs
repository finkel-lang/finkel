;;; -*- mode: finkel -*-
;;; Command line interface utilities.

(defmodule Finkel.Tool.Internal.CLI
  (export
   (CLI ..)
   (Command ..)
   (ExitCode ..)
   find-command
   partition-descrs
   others-passed-to-ghc)
  (import-when [:compile]
   ;; finkel-core
   (Finkel.Prelude))
  (import
   ;; base
   (Control.Exception [throwIO])
   (Control.Monad.IO.Class [(MonadIO ..)])
   (Data.Foldable [find])
   (Data.List [isPrefixOf])
   (System.Console.GetOpt [(ArgDescr ..) (OptDescr ..)])
   (System.Exit [(ExitCode ..)])
   (System.IO.Error [isEOFError])
   (qualified Control.Exception)
   (qualified System.Exit as Exit)

   ;; haskeline
   (System.Console.Haskeline [InputT])
   (qualified System.Console.Haskeline as Haskeline)))

(import Control.Monad.Catch
        ((MonadThrow ..) (MonadCatch ..) (MonadMask ..)))

;;; Type class for command line interface, ... actually, for 'InputT' from the
;;; haskeline package.
(class (=> (MonadIO cl) (CLI cl))
  ;; Show prompt string, and get input line. Return Nothing for EOF
  ;; input.
  (:: getString (-> String (cl (Maybe String))))

  ;; Put output line.
  (:: putString (-> String (cl ())))

  ;; Interrupt signal handler.
  (:: handleInterrupt (-> (cl a) (cl a) (cl a)))

  ;; Perform computation with interrupt handler.
  (:: withInterrupt (-> (cl a) (cl a)))

  ;; Exit with given 'ExitCode'.
  (:: exitWith (-> ExitCode (cl ()))))

(instance (CLI IO)
  (= getString prompt
    (Control.Exception.catch
     (>> (putStr prompt) (fmap Just getLine))
     (\e (if (isEOFError e)
           (return Nothing)
           (throwIO e)))))
  (= putString putStrLn)
  (= handleInterrupt _handler act act)
  (= withInterrupt act act)
  (= exitWith Exit.exitWith))

(instance (=> (MonadIO m) (MonadCatch m) (MonadMask m) (MonadThrow m)
              (CLI (InputT m)))
  (= getString Haskeline.getInputLine)
  %p(INLINE getString)
  (= putString Haskeline.outputStrLn)
  %p(INLINE putString)
  (= handleInterrupt Haskeline.handleInterrupt)
  %p(INLINE handleInterrupt)
  (= withInterrupt Haskeline.withInterrupt)
  %p(INLINE withInterrupt)
  (= exitWith (. liftIO Exit.exitWith))
  %p(INLINE exitWith))


;;; Command data type

;;; Data type to wrap an IO action taking string arguments with name and
;;; description.
(data Command
  (Command {(:: cmd-name String)
            (:: cmd-descr String)
            (:: cmd-act (-> [String] (IO ())))}))

(defn (:: find-command (-> [Command] String (Maybe Command)))
  "Find the command by command name."
  [cmds name]
  (find (. (== name) cmd-name) cmds))


;;; Command line option helper

;;; Note: [Finkel options, ghc options, and RTS options]
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;;
;;; The finkel executable sub commands support options specific to itself, and
;;; some of the sub commands support options to update `DynFlags' in GhcMonad,
;;; and the executable itself support options for RTS. To support these three
;;; kind of options, the command line argument handling work starts with
;;; filtering out the finkel sub command specific options with manually
;;; separating the options defined with `OptDescr' from `System.Console.GetOpts'
;;; module. Then, the rest of the arguments are treated as ghc options and RTS
;;; options.
;;;
;;; Note that, in the C source code of the `rts' package, command line arguments
;;; after `--' are passed to the callee program (see: "rts/RtsFlags.c" in the
;;; ghc source code for detail). Once the command line parser for REPL options
;;; tried to separate ghc options from REPL specific options with `--', however
;;; this approach did not work well when considering RTS options.

(defn (:: others-passed-to-ghc String)
  "  Other options are passed to ghc.")

(defn (:: partition-descrs
        (-> [(OptDescr a)] [String] (, [String] [String])))
  [descrs]
  (lefn [(go [xs0]
           (cond
             [(<- (: x0 x1 rest) xs0)
              (req-arg x0)
              (case (go rest)
                (, as bs) (, (: x0 x1 as) bs))]
             [(<- (: x0 rest) xs0)
              (case (go rest)
                (, as bs) (if (|| (req-arg x0) (no-arg x0) (opt-arg x0)
                                  (req-arg-short-no-space x0))
                            (, (: x0 as) bs)
                            (, as (: x0 bs))))]
             [(<- [] xs0)
              (, [] [])]))
         (no-arg
           [(: #'- #'- cs)] (elem cs long-nos)
           [(: #'- [c])] (elem c short-nos)
           [_] False)
         (req-arg
           [(: #'- #'- cs)] (elem cs long-reqs)
           [(: #'- [c])] (elem c short-reqs)
           [_] False)
         (req-arg-short-no-space
           [(: #'- c _)] (elem c short-reqs)
           [_] False)
         (opt-arg
           [(: #'- #'- cs)] (any (flip isPrefixOf cs) long-eqs)
           [_] False)
         ((, short-nos long-nos short-reqs long-reqs long-eqs)
           (group-descrs descrs))]
    go))

(defn (:: group-descrs
        (-> [(OptDescr a)] (, String [String] String [String] [String])))
  [descrs]
  (lefn [(long-eq [cs]
           (++ cs "="))
         (oflags [cs acc]
           (++ (map long-eq cs) acc))
         (f [(Option ss ls adescr _) (, sns lns srs lrs les)]
           (case adescr
             (NoArg {}) (, (++ ss sns) (++ ls lns) srs lrs les)
             (ReqArg {}) (lefn [(srs' (++ ss srs))
                                (lrs' (++ ls lrs))
                                (les' (oflags ls les))]
                           (, sns lns srs' lrs' les'))
             (OptArg {}) (, sns lns srs lrs (oflags ls les))))]
    (foldr f (, [] [] [] [] []) descrs)))
