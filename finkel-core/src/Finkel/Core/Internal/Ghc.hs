;;; -*- mode: finkel -*-
;;;; Module for ghc related functions

(:doc "Internal module for @ghc@ related functions.")

(module Finkel.Core.Internal.Ghc
  __glasgow_haskell__
  (module Finkel.Core.Internal.Ghc.Compat))

(import Finkel.Core.Internal.Ghc.Compat)
(import Finkel.Core.Internal.Ghc.Version)
