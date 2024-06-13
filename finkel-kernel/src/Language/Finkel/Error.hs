{-# LANGUAGE CPP              #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
-- | Version compatible variant of error message type and functions.

module Language.Finkel.Error
  (
    -- * Simple SDoc error message
    WrappedMsg
  , mkWrappedMsg
  , mkPlainWrappedMsg

    -- * For printing error message
  , HasLogger(..), Logger, WARNINGs
  , printLocatedString
  , printOrThrowDiagnostics'

  ) where

#include "ghc_modules.h"

-- base
import Control.Monad.IO.Class       (MonadIO (..))

#if MIN_VERSION_ghc(9,4,0)
import Data.Typeable                (Typeable)
#endif

-- ghc
import GHC_Data_Bag                 (unitBag)
import GHC_Driver_Session           (DynFlags)
import GHC_Types_SrcLoc             (SrcSpan)
import GHC_Utils_Outputable         (SDoc, neverQualify, text)

#if MIN_VERSION_ghc(9,8,0)
import GHC.Driver.Errors            (printOrThrowDiagnostics)
#elif MIN_VERSION_ghc(9,2,0)
import GHC.Driver.Errors            (handleFlagWarnings)
#else
import GHC_Driver_Types             (handleFlagWarnings)
#endif

#if MIN_VERSION_ghc(9,8,0)
import GHC.Driver.Errors.Types      (DriverMessage)
import GHC.Types.Error              (Messages, defaultDiagnosticOpts)
#elif MIN_VERSION_ghc(9,0,0)
import GHC.Driver.CmdLine           (Warn)
#elif MIN_VERSION_ghc(8,2,0)
import CmdLineParser                (Warn)
#else
import SrcLoc                       (Located)
#endif

#if MIN_VERSION_ghc(9,6,0)
import GHC.Types.Error              (NoDiagnosticOpts (..))
import GHC.Utils.Outputable         (NamePprCtx)
#else
import GHC_Utils_Outputable         (PrintUnqualified)
#endif

#if MIN_VERSION_ghc(9,4,0)
import GHC.Driver.Config.Diagnostic (initDiagOpts)
import GHC.Driver.Errors            (printMessages)
import GHC.Types.Error              (mkMessages)
#else
import GHC_Driver_Errors            (printBagOfErrors)
#endif

#if MIN_VERSION_ghc(9,4,0)
-- For "instance Diagnostic GhcMessage"
import GHC.Driver.Errors.Ppr        ()
import GHC.Driver.Errors.Types      (GhcMessage (..), ghcUnknownMessage)
import GHC.Types.Error              (Diagnostic (..), DiagnosticReason (..),
                                     mkSimpleDecorated, noHints)
import GHC.Utils.Error              (MsgEnvelope, mkErrorMsgEnvelope,
                                     mkPlainErrorMsgEnvelope)
#elif MIN_VERSION_ghc(9,2,0)
import GHC.Types.Error              (DecoratedSDoc, MsgEnvelope, mkMsgEnvelope,
                                     mkPlainMsgEnvelope)
#else
import GHC_Utils_Error              (ErrMsg, mkErrMsg, mkPlainErrMsg)
#endif

#if MIN_VERSION_ghc(9,2,0)
import GHC.Utils.Logger             (HasLogger (..), Logger)
#endif


-- ------------------------------------------------------------------------
-- Wrapper type for SDoc
-- ------------------------------------------------------------------------

#if MIN_VERSION_ghc(9,4,0)
newtype SDocWrapper = SDocWrapper {unSDocWrapper :: SDoc}
  deriving (Typeable)

instance Diagnostic SDocWrapper where
#if MIN_VERSION_ghc(9,6,0)
  type DiagnosticOpts SDocWrapper = NoDiagnosticOpts
  diagnosticMessage _no_diagnostic_opts = mkSimpleDecorated . unSDocWrapper
#  if !MIN_VERSION_ghc(9,8,0)
  defaultDiagnosticOpts = NoDiagnosticOpts
#  endif
  -- XXX: May worth adding Finkel specific diagnostic code.
  diagnosticCode _ = Nothing
#else
  diagnosticMessage = mkSimpleDecorated . unSDocWrapper
#endif

  diagnosticReason = const ErrorWithoutFlag
  diagnosticHints = const noHints

wrapSDoc :: SDoc -> GhcMessage
wrapSDoc = ghcUnknownMessage . SDocWrapper
#endif

-- | Synonym for message with 'SDoc'.
#if MIN_VERSION_ghc(9,4,0)
type WrappedMsg = MsgEnvelope GhcMessage
#elif MIN_VERSION_ghc(9,2,0)
type WrappedMsg = MsgEnvelope DecoratedSDoc
#else
type WrappedMsg = ErrMsg
#endif

#if MIN_VERSION_ghc(9,6,0)
mkWrappedMsg :: DynFlags -> SrcSpan -> NamePprCtx -> SDoc -> WrappedMsg
#else
mkWrappedMsg :: DynFlags -> SrcSpan -> PrintUnqualified -> SDoc -> WrappedMsg
#endif
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


-- ------------------------------------------------------------------------
-- For printing error messages
-- ------------------------------------------------------------------------

printLocatedString
  :: MonadIO m => Logger -> DynFlags -> SrcSpan -> String -> m ()
printLocatedString _logger dflags l str = do
      let em = mkWrappedMsg dflags l neverQualify (text str)
#if MIN_VERSION_ghc(9,6,0)
      let ghc_msg = mkMessages (unitBag em)
          diagnostic_opts = defaultDiagnosticOpts @GhcMessage
          diag_opts = initDiagOpts dflags
      liftIO (printMessages _logger diagnostic_opts diag_opts ghc_msg)
#elif MIN_VERSION_ghc(9,4,0)
      let ghc_msg = mkMessages (unitBag em)
      liftIO (printMessages _logger (initDiagOpts dflags) ghc_msg)
#elif MIN_VERSION_ghc(9,2,0)
      liftIO (printBagOfErrors _logger dflags (unitBag em))
#else
      liftIO (printBagOfErrors dflags (unitBag em))
#endif
{-# INLINABLE printLocatedString #-}

#if MIN_VERSION_ghc(9,8,0)
type WARNINGs = Messages DriverMessage
#elif MIN_VERSION_ghc(8,2,0)
type WARNINGs = [Warn]
#else
type WARNINGs = [Located String]
#endif

-- GHC.Utils.Logger did not exist until ghc 9.2.
#if !MIN_VERSION_ghc(9,2,0)
class HasLogger m where
  getLogger :: m Logger

data Logger -- should never constructed.
#endif

-- | Version compatibility function for 'printOrThrowDiagnostics', former
-- @handleFlagWarnings@ function.
printOrThrowDiagnostics' :: MonadIO m => Logger -> DynFlags -> WARNINGs -> m ()
printOrThrowDiagnostics' _logger dflags warns = do
#if MIN_VERSION_ghc(9,8,0)
  let diagnostic_opts = defaultDiagnosticOpts @GhcMessage
      diag_opts = initDiagOpts dflags
      msg = GhcDriverMessage <$> warns
  liftIO $ printOrThrowDiagnostics _logger diagnostic_opts diag_opts msg
#elif MIN_VERSION_ghc(9,6,0)
  let diagnostic_opts = defaultDiagnosticOpts @GhcMessage
      diag_opts = initDiagOpts dflags
  liftIO $ handleFlagWarnings _logger diagnostic_opts diag_opts warns
#elif MIN_VERSION_ghc(9,4,0)
  liftIO $ handleFlagWarnings _logger (initDiagOpts dflags) warns
#elif MIN_VERSION_ghc(9,2,0)
  liftIO $ handleFlagWarnings _logger dflags warns
#else
  liftIO $ handleFlagWarnings dflags warns
#endif
