{-# LANGUAGE CPP          #-}
{-# LANGUAGE TypeFamilies #-}
-- | Version compatible variant of error message type and functions.

module Language.Finkel.Error
  ( WrappedMsg
  , mkWrappedMsg
  , mkPlainWrappedMsg
  ) where

#include "ghc_modules.h"

#if MIN_VERSION_ghc(9,6,0)
import GHC.Types.Error         (NoDiagnosticOpts (..))
import GHC.Utils.Outputable    (NamePprCtx)
#else
import GHC_Utils_Outputable    (PrintUnqualified)
#endif

#if MIN_VERSION_ghc(9,4,0)
-- base
import Data.Typeable           (Typeable)

-- ghc
-- For "instance Diagnostic GhcMessage"
import GHC.Driver.Errors.Ppr   ()
import GHC.Driver.Errors.Types (GhcMessage, ghcUnknownMessage)
import GHC.Types.Error         (Diagnostic (..), DiagnosticReason (..),
                                mkSimpleDecorated, noHints)
import GHC.Utils.Error         (MsgEnvelope, mkErrorMsgEnvelope,
                                mkPlainErrorMsgEnvelope)
#elif MIN_VERSION_ghc(9,2,0)
import GHC.Types.Error         (DecoratedSDoc, MsgEnvelope, mkMsgEnvelope,
                                mkPlainMsgEnvelope)
#else
import GHC_Utils_Error         (ErrMsg, mkErrMsg, mkPlainErrMsg)
#endif

import GHC_Driver_Session      (DynFlags)
import GHC_Types_SrcLoc        (SrcSpan)
import GHC_Utils_Outputable    (SDoc)

#if MIN_VERSION_ghc(9,4,0)
newtype FnkWrapper = FnkWrapper {unFnkWrapper :: SDoc}
  deriving (Typeable)

instance Diagnostic FnkWrapper where
#if MIN_VERSION_ghc(9,6,0)
  type DiagnosticOpts FnkWrapper = NoDiagnosticOpts
  diagnosticMessage _no_diagnostic_opts = mkSimpleDecorated . unFnkWrapper
  defaultDiagnosticOpts = NoDiagnosticOpts
  -- XXX: May worth adding Finkel specific diagnostic code.
  diagnosticCode _ = Nothing
#else
  diagnosticMessage = mkSimpleDecorated . unFnkWrapper
#endif

  diagnosticReason = const ErrorWithoutFlag
  diagnosticHints = const noHints

wrapSDoc :: SDoc -> GhcMessage
wrapSDoc = ghcUnknownMessage . FnkWrapper
#endif

-- | Synonym for message with 'SDoc'.
#if MIN_VERSION_ghc(9,4,0)
type WrappedMsg = MsgEnvelope GhcMessage
#elif MIN_VERSION_ghc(9,2,0)
type WrappedMsg = MsgEnvelope DecoratedSDoc
#else
type WrappedMsg = ErrMsg
#endif

#if !MIN_VERSION_ghc(9,6,0)
#define NamePprCtx PrintUnqualified
#endif

mkWrappedMsg :: DynFlags -> SrcSpan -> NamePprCtx -> SDoc -> WrappedMsg
{-# INLINABLE mkWrappedMsg #-}

mkPlainWrappedMsg :: DynFlags -> SrcSpan -> SDoc -> WrappedMsg
{-# INLINABLE mkPlainWrappedMsg #-}

#if MIN_VERSION_ghc(9,4,0)
mkWrappedMsg _dflags sp pq sdoc = mkErrorMsgEnvelope sp pq (wrapSDoc sdoc)
mkPlainWrappedMsg _dflags sp sdoc = mkPlainErrorMsgEnvelope sp (wrapSDoc sdoc)
#elif MIN_VERSION_ghc(9,2,0)
mkWrappedMsg = const mkMsgEnvelope
mkPlainWrappedMsg = const mkPlainMsgEnvelope
#else
mkWrappedMsg = mkErrMsg
mkPlainWrappedMsg = mkPlainErrMsg
#endif
