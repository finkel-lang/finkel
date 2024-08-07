;;; -*- mode: finkel -*-

%p(LANGUAGE DeriveDataTypeable
            DeriveGeneric
            OverloadedStrings)

(module P1
  (D ..) define-macro)

(import Language.Finkel)
(import Data.Data)
(import GHC.Generics ((Generic ..)))

(data D D1 D2 D3
  (deriving Bounded Enum Eq Ord Show Read Data Typeable Generic))

(instance (Homoiconic D))

(:: dmac (-> Code (Fnk Code)))
(= dmac form
  (let ((:: make-tsig (-> Code Code))
        (= make-tsig name `(:: ,name Macro))
        (:: macro-decl (-> Code Code Code (Fnk Code)))
        (= macro-decl name arg body
          (do (<- tmp (gensym' (show name)))
              (return `(= ,name
                         (let ((:: ,tmp (-> Code (Fnk Code)))
                               (= ,tmp ,arg ,body))
                           (Macro ,tmp)))))))
    (case (unCode form)
      (List [_ name arg body])
      (do (<- decl (macro-decl name arg body))
          (return `(:begin
                     ,(make-tsig name)
                     ,decl)))

      (List [_ name (@ doc (LForm (L _ (Atom (AString {}))))) arg body])
      (do (<- decl (macro-decl name arg body))
          (return `(:begin
                     ,(make-tsig name)
                     (:doc^ ,doc)
                     ,decl)))

      _ (finkelSrcError form (++ "dmac: malformed macro: " (show form))))))
   %p(INLINABLE dmac)

(:: define-macro Macro)
(= define-macro (Macro dmac))
