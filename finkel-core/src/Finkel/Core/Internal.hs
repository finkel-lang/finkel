;;; -*- mode: finkel -*-
;;;; For internal types and functions

(:doc "Module to re-export internal types and functions.

The intended usage is for internal use only, from test codes and from the
@finkel-tool@ package.")

(module Finkel.Core.Internal
  (:dh1 "For GHC")
  (module Finkel.Core.Internal.Ghc))

(import Finkel.Core.Internal.Ghc)
