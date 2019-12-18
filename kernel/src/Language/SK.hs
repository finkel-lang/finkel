-- | Module re-exporting runtime dependency for SK kernel programs.
--
-- This module exports types and functions for writing SK kernel
-- programs, with quotes, quasi-quotes, unquotes, unquote-splicings, and
-- macros.
--
module Language.SK
  (
    -- * Form
    Atom(..)
  , Form(..)
  , LForm(..)
  , ToCode(..)
  , FromCode(..)
  , Code
  , unCode
  , nil

  , qSymbol
  , qChar
  , qString
  , qInteger
  , qFractional
  , qUnit
  , qList
  , qHsList

  -- * Macro
  , Macro(Macro)
  , Skc
  , expand
  , expand1
  , gensym
  , gensym'
  , unquoteSplice
  , skSrcError

  -- * Re-export from ghc
  , GenLocated(..)
  ) where

-- ghc
import SrcLoc (GenLocated(..))

-- Internal
import Language.SK.Expand
import Language.SK.Form
import Language.SK.Homoiconic
import Language.SK.SKC
