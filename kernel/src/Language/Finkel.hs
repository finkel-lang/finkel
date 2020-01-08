-- | Module re-exporting runtime dependency for Finkel kernel programs.
--
-- This module exports types and functions for writing Finkel kernel
-- programs, with quotes, quasi-quotes, unquotes, unquote-splicings, and
-- macros.
--
module Language.Finkel
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
  , Fnk
  , expand
  , expand1
  , gensym
  , gensym'
  , unquoteSplice
  , finkelSrcError

  -- * Re-export from ghc
  , GenLocated(..)
  ) where

-- ghc
import SrcLoc (GenLocated(..))

-- Internal
import Language.Finkel.Expand
import Language.Finkel.Form
import Language.Finkel.Homoiconic
import Language.Finkel.Fnk
