-- | Module re-exporting runtime dependency for SK kernel programs.
--
-- This module exports minimal set of types and functions for writing SK
-- kernel programs, including quotes, quasi-quotes, unquotes,
-- unquote-splicings, and macros.
--
module Language.SK
  (
    -- * Form
    Atom(..)
  , Form(..)
  , LForm(..)
  , Homoiconic(..)
  , Code
  , unCode
  , nil
  , aFractional
  , aSymbol

  -- * Macro
  , Macro(Macro)
  , Skc
  , expand
  , expand1
  , gensym
  , gensym'
  , quoted
  , unquoteSplice
  , skSrcError

  -- * Re-export
  , GenLocated(..)
  ) where

import Language.SK.Expand
import Language.SK.Form
import Language.SK.Homoiconic
import Language.SK.SKC
