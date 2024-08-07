{-# LANGUAGE CPP #-}

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
import Control.Exception      (Exception (..), throw)
import Control.Monad.IO.Class (MonadIO (..))
import System.IO              (hPutStrLn, stderr)

-- ghc
import GHC_Driver_Session     (HasDynFlags (..))
import GHC_Types_SrcLoc       (GenLocated (..), SrcSpan)
import GHC_Utils_Exception    (ExceptionMonad)

#if MIN_VERSION_ghc(9,0,0)
-- exceptions
import Control.Monad.Catch    (handle)
#else
-- ghc
import GHC_Utils_Exception    (ghandle)
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
printFinkelException
  :: (HasLogger m, HasDynFlags m, MonadIO m) => FinkelException -> m ()
printFinkelException e = case finkelExceptionLoc e of
  Nothing -> liftIO $ hPutStrLn stderr msg
  Just l  -> do
    logger <- getLogger
    dflags <- getDynFlags
    printLocatedString logger dflags l msg
  where
    msg = displayException e

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

