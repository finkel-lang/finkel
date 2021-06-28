{-# LANGUAGE CPP #-}
-- | Version compatible variant of error message type and functions.

module Language.Finkel.Error
  ( WrappedMsg
  , mkWrappedMsg
  , mkPlainWrappedMsg
  ) where

#include "ghc_modules.h"

#if MIN_VERSION_ghc(9,2,0)
import GHC.Types.Error      (DecoratedSDoc, MsgEnvelope, mkMsgEnvelope,
                             mkPlainMsgEnvelope)
#else
import GHC_Utils_Error      (ErrMsg, mkErrMsg, mkPlainErrMsg)
#endif

import GHC_Driver_Session   (DynFlags)
import GHC_Types_SrcLoc     (SrcSpan)
import GHC_Utils_Outputable (PrintUnqualified, SDoc)

-- | Synonym for message with 'SDoc'.
#if MIN_VERSION_ghc(9,2,0)
type WrappedMsg = MsgEnvelope DecoratedSDoc
#else
type WrappedMsg = ErrMsg
#endif

mkWrappedMsg :: DynFlags -> SrcSpan -> PrintUnqualified -> SDoc -> WrappedMsg
{-# INLINABLE mkWrappedMsg #-}

mkPlainWrappedMsg :: DynFlags -> SrcSpan -> SDoc -> WrappedMsg
{-# INLINABLE mkPlainWrappedMsg #-}

#if MIN_VERSION_ghc(9,2,0)
mkWrappedMsg = const mkMsgEnvelope
mkPlainWrappedMsg = const mkPlainMsgEnvelope
#else
mkWrappedMsg = mkErrMsg
mkPlainWrappedMsg = mkPlainErrMsg
#endif
