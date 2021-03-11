{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Finkel.Make.Trace
  ( traceMake
  , traceMake'
  , nvc_or_none
  ) where

#include "ghc_modules.h"

-- base
import Control.Monad.IO.Class (MonadIO (..))

-- ghc
import GHC_Driver_Session     (DynFlags, HasDynFlags (..))
import GHC_Utils_Error        (MsgDoc)
import GHC_Utils_Outputable   (Outputable (..), SDoc, hcat, nest, vcat)

-- Internal
import Language.Finkel.Fnk    (FnkDebugFlag (..), FnkEnv, debugWhen')

-- | Trace function for 'make' related modules.
traceMake
  :: (MonadIO m, HasDynFlags m) => FnkEnv -> MsgDoc -> [MsgDoc] -> m ()
traceMake fnk_env fn_name msgs0 =
  getDynFlags >>= \df -> traceMake' df fnk_env fn_name msgs0

-- | Like 'traceMake', but takes 'DynFlags' from argument.
traceMake'
  :: MonadIO m => DynFlags -> FnkEnv -> MsgDoc -> [MsgDoc] -> m ()
traceMake' dflags fnk_env fn_name msgs0 =
  let msgs1 = (hcat [";;; [Language.Finkel.Make.", fn_name, "]:"] : msgs0)
  in  debugWhen' dflags fnk_env Fnk_trace_make msgs1

-- | Nested 'vcat' or text @"none"@.
nvc_or_none :: Outputable a => [a] -> SDoc
nvc_or_none xs = nest 2 sdoc
  where
    sdoc =
       if null xs
         then "none"
         else vcat (map ppr xs)