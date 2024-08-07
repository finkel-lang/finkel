;;; -*- mode: finkel -*-
%p(LANGUAGE FlexibleInstances
            GeneralizedNewtypeDeriving)

;;;; Types for REPL.

(defmodule Finkel.Tool.Internal.Types
  (export
   ;; repl
   (Repl ..) run-repl put-repl-state get-repl-state

   ;; repl state
   (ReplState ..) (HasReplState ..) initial-repl-state

   ;; input and result
   (Input ..) (InSource ..) Result

   ;; re-export
   (MonadTrans ..))
  (require
   ;; finkel-core
   (Finkel.Tool.Internal.Macro.Ghc))
  (import-when [:compile]
   ;; finkel-core
   (Finkel.Prelude))
  (import
   ;; base
   (Control.Concurrent [MVar])
   (Control.Monad.IO.Class [(MonadIO ..)])
   (Data.Function [on])
   (Data.IORef [IORef atomicWriteIORef newIORef readIORef])


   ;; transformers
   (Control.Monad.Trans.Class [(MonadTrans ..)])

   ;; finkel-kernel
   (Language.Finkel [Code])))

(imports-from-ghc
 (GHC.Data.StringBuffer [StringBuffer appendStringBuffers])
 (GHC.Utils.IO.Unsafe [inlinePerformIO]))

(cond-expand
  [(:min-version "haskeline" 0 8 0)
   ;; exceptions
   (import Control.Monad.Catch
           ((MonadThrow ..) (MonadCatch ..) (MonadMask ..)))]
  [otherwise
   ;; haskeline
   (import System.Console.Haskeline ((MonadException ..) (RunIO ..)))])

;;; REPL, REPL state, input, and result types

;;; Repl state type to hold intermediate line-wise inputs.
(data ReplState
  (ReplState {(:: pending-input (Maybe StringBuffer))
              (:: prompt-string String)})
  (deriving Show))

(defn (:: append-repl-state (-> ReplState ReplState ReplState))
  [r1 r2]
  (where (ReplState {(= pending-input (on unsafeAppendStringBuffers
                                          pending-input r1 r2))
                     (= prompt-string (prompt-string r2))})
    ;; Note the use of `inlinePerformIO'.
    (= unsafeAppendStringBuffers mb-s1 mb-s2
      (| ((<- (Just s1) mb-s1) (<- (Just s2) mb-s2)
          (Just (inlinePerformIO (appendStringBuffers s1 s2))))
         ((<- (Just _) mb-s1) mb-s1)
         ((<- (Just _) mb-s2) mb-s2)
         (otherwise Nothing)))))

(instance (Eq ReplState)
  (defn ==
    (where (on eqStringBuffer pending-input)
      (= eqStringBuffer (on == show)))))

(instance (Monoid ReplState)
  (defn mempty initial-repl-state)
  (cond-expand
    [(not (:min-version "base" 4 11 0))
     (defn mappend append-repl-state)]
    [otherwise
     (:begin)]))

(cond-expand
  [(:min-version "base" 4 11 0)
   (instance (Semigroup ReplState)
     (= <> append-repl-state))]
  [otherwise
   (:begin)])

(defn (:: initial-repl-state ReplState)
  (ReplState {(= pending-input Nothing)
              (= prompt-string "> ")}))

;;; Newtype wrapper for REPL prompt.
(newtype (Repl a)
  (Repl {(:: unRepl (-> (IORef ReplState) (IO a)))}))

(instance (Functor Repl)
  (defn fmap [f (Repl repl)]
    (Repl (. (fmap f) repl)))
  %p(INLINE fmap))

(instance (Applicative Repl)
  (defn pure [x]
    (Repl (\_ (pure x))))
  %p(INLINE pure)

  (defn <*> [(Repl mf) (Repl mx)]
    (Repl (\ref (<*> (mf ref) (mx ref)))))
  %p(INLINE <*>))

(instance (Monad Repl)
  (defn >>= [(Repl repl) k]
    (Repl (\ref (>>= (repl ref) (. (flip unRepl ref) k)))))
  %p(INLINE >>=))

(instance (MonadIO Repl)
  (defn liftIO [io]
    (Repl (\_ io)))
  %p(INLINE liftIO))

;;; In ghc-8.10.1, `haskeline' switched to use `MonadThrow', `MonadCatch', and
;;; `MonadMask' type classes from the `exceptions' package instead of the
;;; internally defined `MonadException' type class. Since the `Repl' data type
;;; is used with codes for the `InputT' from `haskeline' package, defining
;;; instances of type classes from `exceptions'.
(cond-expand
  [(:min-version "haskeline" 0 8 0)
   (:begin
     (instance (MonadThrow Repl)
       (defn throwM (. liftIO throwM))
       %p(INLINE throwM))

     (instance (MonadCatch Repl)
       (defn catch [(Repl repl) f]
         (Repl (\ref
                 (catch (repl ref)
                   (\e (unRepl (f e) ref))))))
       %p(INLINE catch))

     (instance (MonadMask Repl)
       (defn mask [a]
         (lefn [(:: q (-> (-> (IO a) (IO a)) (Repl a) (Repl a)))
                (q [unmask (Repl repl)]
                  (Repl (. unmask repl)))]
           (Repl (\ref
                   (mask
                    (\unmask (unRepl (a (q unmask)) ref)))))))
       %p(INLINE mask)
       (defn uninterruptibleMask [a]
         (lefn [(:: q (-> (-> (IO a) (IO a)) (Repl a) (Repl a)))
                (q [unmask (Repl repl)]
                  (Repl (. unmask repl)))]
           (Repl (\ref
                   (uninterruptibleMask
                    (\unmask
                      (unRepl (a (q unmask)) ref)))))))
       %p(INLINE uninterruptibleMask)
       (defn generalBracket [acquire release use]
         (Repl (\ref
                 (generalBracket
                  (unRepl acquire ref)
                  (\resource exit-case
                    (unRepl (release resource exit-case) ref))
                  (\resource
                    (unRepl (use resource) ref))))))
       %p(INLINE generalBracket)))]

  [otherwise
    (instance (MonadException Repl)
      (defn controlIO [f]
        (Repl (\ref
                (controlIO
                 (\ (RunIO run)
                   (lept [run' (RunIO (. (fmap (. Repl const))
                                         (. run (flip unRepl ref))))]
                     (fmap (flip unRepl ref) (f run'))))))))
      %p(INLINE controlIO))])

(defn (:: run-repl (-> (Repl a) ReplState (IO a)))
  [(Repl repl) st]
  (>>= (newIORef st) repl))

(defn (:: get-repl-state (Repl ReplState))
  (Repl readIORef))

(defn (:: put-repl-state (-> ReplState (Repl ())))
  [st]
  (Repl (flip atomicWriteIORef st)))

;;; Type class for getting and putting 'ReplState'.
(class (HasReplState r)
  (:: getReplState (r ReplState))
  (:: putReplState (-> ReplState (r ()))))

(instance (HasReplState Repl)
  (= getReplState get-repl-state)
  %p(INLINE getReplState)
  (= putReplState put-repl-state)
  %p(INLINE putReplState))

(instance (=> (MonadTrans t) (Monad m) (HasReplState m)
              (HasReplState (t m)))
  (= getReplState (lift getReplState))
  %p(INLINE getReplState)
  (= putReplState (. lift putReplState))
  %p(INLINE putReplState))

;;; Input data type to hold form to evaluate, and MVar to receive
;;; result from evaluation thread.
(data Input
  (Input InSource Code (MVar Result)))

;;; Type for input, to distinguish prompt from network connections to
;;; REPL server.
(data InSource
  Prompt
  Connection)

;;; Synonym for evaluation result.
(type Result
  (Either String String))
