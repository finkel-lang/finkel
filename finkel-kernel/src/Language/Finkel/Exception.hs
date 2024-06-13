{-# LANGUAGE CPP              #-}
{-# LANGUAGE TypeApplications #-}

-- | Exception related types and functions in @finkel-kernel@.
module Language.Finkel.Exception
  ( FinkelException(..)
  , finkelExceptionLoc
  , readOrFinkelException
  , handleFinkelException
  , printFinkelException
  ) where

#include "ghc_modules.h"

-- base
import Control.Exception            (Exception (..), throw)
import Control.Monad.IO.Class       (MonadIO (..))
import System.IO                    (hPutStrLn, stderr)

-- ghc
import GHC_Data_Bag                 (unitBag)
import GHC_Driver_Session           (HasDynFlags (..))
import GHC_Types_SrcLoc             (GenLocated (..), SrcSpan)
import GHC_Utils_Exception          (ExceptionMonad)
import GHC_Utils_Outputable         (neverQualify, text)

#if MIN_VERSION_ghc(9,6,0)
import GHC.Driver.Errors.Types      (GhcMessage)
import GHC.Types.Error              (defaultDiagnosticOpts)
#endif

#if MIN_VERSION_ghc(9,4,0)
import GHC.Driver.Config.Diagnostic (initDiagOpts)
import GHC.Driver.Errors            (printMessages)
import GHC.Types.Error              (mkMessages)
#else
import GHC_Driver_Errors            (printBagOfErrors)
#endif

#if MIN_VERSION_ghc(9,2,0)
import GHC.Utils.Logger             (HasLogger (..))
#endif

#if MIN_VERSION_ghc(9,0,0)
-- exceptions
import Control.Monad.Catch          (handle)
#else
-- ghc
import GHC_Utils_Exception          (ghandle)
#endif

-- Internal
import Language.Finkel.Error
import Language.Finkel.Form


-- ---------------------------------------------------------------------
--
-- Type
--
-- ---------------------------------------------------------------------

-- | Exception for @finkel-kernel@@ package.
data FinkelException
  = LexicalException SrcSpan Char
  -- ^ Lexical error.
  | InvalidUnquoteSplice Code
  -- ^ Invalid unquote splice with 'Code' value.
  | FinkelSrcError Code String
  -- ^ Error with 'Code' information and additional message.
  | FinkelException String
  -- ^ General exception with message.
  deriving (Eq, Show)

instance Exception FinkelException where
  displayException = displayFinkelException
  {-# INLINE displayException #-}

displayFinkelException :: FinkelException -> String
displayFinkelException e = case e of
  LexicalException _ c   -> "Lexical error near " ++ show c
  InvalidUnquoteSplice c -> "Invalid unquote splice: " ++ show c
  FinkelSrcError _ s     -> s
  FinkelException s      -> s
{-# INLINEABLE displayFinkelException #-}

-- | Get source location information if available.
finkelExceptionLoc :: FinkelException -> Maybe SrcSpan
finkelExceptionLoc fe = case fe of
  LexicalException l _                 -> Just l
  InvalidUnquoteSplice (LForm (L l _)) -> Just l
  FinkelSrcError (LForm (L l _)) _     -> Just l
  _                                    -> Nothing
{-# INLINABLE finkelExceptionLoc #-}

readOrFinkelException :: Read s => String -> String -> String -> s
readOrFinkelException what name str =
  case reads str of
    [(x, "")] -> x
    _ -> throw (FinkelException ("Expecting " ++ what ++
                                 " for " ++ name ++
                                 ", but got " ++ show str))
{-# INLINABLE readOrFinkelException #-}

-- | Print 'FinkelException' with source code information when available.
#if MIN_VERSION_ghc(9,2,0)
printFinkelException
  :: (HasLogger m, HasDynFlags m, MonadIO m) => FinkelException -> m ()
#else
printFinkelException :: (HasDynFlags m, MonadIO m) => FinkelException -> m ()
#endif
printFinkelException e = case finkelExceptionLoc e of
  Just l  -> prLocErr l
  Nothing -> pr msg
  where
    msg = displayException e
    pr = liftIO . hPutStrLn stderr
    prLocErr l = do
      dflags <- getDynFlags
      let em = mkWrappedMsg dflags l neverQualify (text msg)
#if MIN_VERSION_ghc(9,6,0)
      logger <- getLogger
      let ghc_msg = mkMessages (unitBag em)
          diagnostic_opts = defaultDiagnosticOpts @GhcMessage
          diag_opts = initDiagOpts dflags
      liftIO (printMessages logger diagnostic_opts diag_opts ghc_msg)
#elif MIN_VERSION_ghc(9,4,0)
      logger <- getLogger
      let ghc_msg = mkMessages (unitBag em)
      liftIO (printMessages logger (initDiagOpts dflags) ghc_msg)
#elif MIN_VERSION_ghc(9,2,0)
      logger <- getLogger
      liftIO (printBagOfErrors logger dflags (unitBag em))
#else
      liftIO (printBagOfErrors dflags (unitBag em))
#endif
{-# INLINABLE printFinkelException #-}


-- ------------------------------------------------------------------------
--
-- Type fixed variant functions
--
-- ------------------------------------------------------------------------

handleFinkelException :: ExceptionMonad m
                      => (FinkelException -> m a) -> m a -> m a
#if MIN_VERSION_ghc(9,0,0)
handleFinkelException = handle
#else
handleFinkelException = ghandle
#endif
