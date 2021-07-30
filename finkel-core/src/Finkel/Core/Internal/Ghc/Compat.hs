{-# LANGUAGE CPP #-}
-- Module to re-export functions from ghc

module Finkel.Core.Internal.Ghc.Compat
  (
    -- GHC
    getModuleInfo, lookupModule, lookupName, modInfoExports,

    -- GHC.Data.FastString
    FastString, fsLit, unpackFS, headFS, nullFS,

    -- GHC.Driver.Env
    HscEnv(..),

    -- GHC.Driver.Monad
    GhcMonad(..),

    -- GHC.Driver.Ppr
    showSDoc,

    -- GHC.Types.SourceText
    SourceText(..),

    -- GHC.Types.TyThing
    TyThing(..),

    -- GHC.Types.Var
    varName,

    -- GHC.Unit.Module
    mkModuleName,

    -- GHC.Utils.Lexeme
    isLexCon,

    -- GHC.Utils.Outputable
    ppr
  ) where

import GHC                  (getModuleInfo, lookupModule, lookupName,
                             modInfoExports)

#if MIN_VERSION_ghc(9,2,0)
import GHC.Driver.Env
import GHC.Driver.Ppr
import GHC.Types.SourceText
import GHC.Types.TyThing
#elif MIN_VERSION_ghc(9,0,0)
import GHC.Driver.Types
import GHC.Types.Basic
#endif

#if MIN_VERSION_ghc(9,0,0)
import GHC.Data.FastString
import GHC.Driver.Monad
import GHC.Types.Var
import GHC.Unit.Module
import GHC.Utils.Lexeme
import GHC.Utils.Outputable
#else
import BasicTypes
import FastString
import GhcMonad
import HscTypes
import Lexeme
import Module
import Outputable
import Var
#endif
