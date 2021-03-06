{-# LANGUAGE CPP #-}
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

  , Code
  , unCode
  , Homoiconic(..)
  , fromCode

  , QuoteFn
  , qSymbol
  , qChar
  , qString
  , qInteger
  , qFractional
  , qUnit
  , qList
  , qHsList
  , nil
  , asLocOf

  -- * Fnk
  , Fnk
  , runFnk
  , defaultFnkEnv

  -- * Macro
  , Macro(Macro)
  , isMacro
  , expand
  , expands
  , expand1
  , gensym
  , gensym'
  , unquoteSplice
  , macroFunction

  -- * Exceptions
  , FinkelException(..)
  , finkelSrcError

  -- * Re-export from ghc
  , Located
  , GenLocated(..)
  , SrcSpan
  ) where

#include "ghc_modules.h"

-- ghc
import GHC_Types_SrcLoc             (GenLocated (..), Located, SrcSpan)

-- Internal
import Language.Finkel.Exception
import Language.Finkel.Expand
import Language.Finkel.Fnk
import Language.Finkel.Form
import Language.Finkel.Homoiconic
import Language.Finkel.SpecialForms
