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

  -- * Fnk
  , Fnk
  , runFnk
  , defaultFnkEnv

  -- * Macro
  , Macro(Macro)
  , expand
  , expand1
  , gensym
  , gensym'
  , unquoteSplice
  , macroFunction
  , finkelSrcError

  -- * Re-export from ghc
  , GenLocated(..)
  ) where

-- ghc
import SrcLoc                       (GenLocated (..))

-- Internal
import Language.Finkel.Expand
import Language.Finkel.Fnk
import Language.Finkel.Form
import Language.Finkel.Homoiconic
import Language.Finkel.SpecialForms
