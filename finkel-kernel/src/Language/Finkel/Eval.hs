{-# LANGUAGE CPP #-}
-- | Module containing functions for code evaluation.
module Language.Finkel.Eval
  ( evalDecls
  , evalExpr
  , evalExprType
  , evalTypeKind
  ) where

#include "ghc_modules.h"

-- base
import Control.Monad.IO.Class  (MonadIO (..))

-- ghc
import GHC_Core_TyCo_Rep       (Kind, Type (..))
import GHC_Core_TyCo_Tidy      (tidyType)
import GHC_Driver_Env_Types    (HscEnv (..))
import GHC_Driver_Main         (hscParsedDecls)
import GHC_Driver_Monad        (GhcMonad (..), withSession)
import GHC_Runtime_Context     (InteractiveContext (..))
import GHC_Runtime_Eval        (compileParsedExprRemote)
import GHC_Tc_Module           (TcRnExprMode (..), tcRnExpr, tcRnType)
import GHC_Types_TyThing       (TyThing (..))
import GHC_Types_Var_Env       (emptyTidyEnv)
import GHC_Utils_Error         (Messages)


#if MIN_VERSION_ghc(9,8,0)
import GHC.Tc.Zonk.Env         (ZonkFlexi (..))
#else
import GHC_Tc_Utils_Zonk       (ZonkFlexi (..))
#endif

#if MIN_VERSION_ghc(9,4,0)
import GHC.Driver.Errors.Types (GhcMessage, hoistTcRnMessage)
import GHC.Tc.Errors.Types     (TcRnMessage)
#elif MIN_VERSION_ghc(9,2,0)
import GHC_Types_Error         (DecoratedSDoc)
#endif

#if MIN_VERSION_ghc(9,2,0)
import GHC_Types_Error         (partitionMessages)
import GHC_Types_SourceError   (throwErrors)
#else
import Control.Exception       (throwIO)
import GHC_Types_SourceError   (mkSrcErr)
#endif

-- ghci
import GHCi.RemoteTypes        (HValue, localRef, withForeignRef)

-- internal
import Language.Finkel.Builder (HDecl, HExpr, HType)


-- ---------------------------------------------------------------------
--
-- Eval functions
--
-- ---------------------------------------------------------------------

-- | Evaluate given expression to haskell value.
evalExpr :: GhcMonad m => HExpr -> m HValue
evalExpr expr = do
  fhv <- compileParsedExprRemote expr
  liftIO (withForeignRef fhv localRef)
{-# INLINABLE evalExpr #-}

-- | Evaluate the type of given expression.
evalExprType :: GhcMonad m => HExpr -> m Type
evalExprType expr = do
  -- See `InteractiveEval.exprType' and `HscMain.hscTcExpr'. As in `evalDecls',
  -- taking HExpr instead of Haskell source code String.
  --
  -- XXX: Currently, `TcRnExprMode' is hard coded as `TM_Inst' in below call to
  -- `tcRnExpr'. In ghci, user can type in and specify the mode from REPL
  -- session.
  --
  hsc_env <- getSession
  ty <- ioMsgMaybe $ hoistTcRnMessage' $ tcRnExpr hsc_env TM_Inst expr
  return $ tidyType emptyTidyEnv ty
{-# INLINABLE evalExprType #-}

-- | Evaluate the kind of given type.  Returned values is a pair of the
-- argument type and the kind of that type.
evalTypeKind :: GhcMonad m => HType -> m (Type, Kind)
evalTypeKind ty = do
  -- See `InteractiveEval.typeKind' and `HscMain.hscKcType'.
  --
  -- XXX: The second argument of `tcRnType' is hard coded as `True' in below
  -- code.
  --
  hsc_env <- getSession
  ioMsgMaybe $ hoistTcRnMessage' $ tcRnType' hsc_env True ty
{-# INLINABLE evalTypeKind #-}

-- | Evaluate given declarations. The returned value is resulting 'TyThing's of
-- declarations and updated interactive context.
evalDecls :: GhcMonad m => [HDecl] -> m ([TyThing], InteractiveContext)
evalDecls decls =
  withSession (\hsc_env -> liftIO (hscParsedDecls hsc_env decls))


-- ---------------------------------------------------------------------
--
-- Auxiliary
--
-- ---------------------------------------------------------------------

-- Separation of TcRnMessage and GhcMessage was introduced in ghc 9.4.
#if MIN_VERSION_ghc(9,4,0)
hoistTcRnMessage' ::
  Monad m => m (Messages TcRnMessage, a) -> m (Messages GhcMessage, a)
hoistTcRnMessage' = hoistTcRnMessage
#else
hoistTcRnMessage' :: a -> a
hoistTcRnMessage' = id
#endif
{-# INLINABLE hoistTcRnMessage' #-}

-- | Like 'HscMain.ioMsgMaybe', but for 'Fnk'.
#if MIN_VERSION_ghc(9,4,0)
ioMsgMaybe :: MonadIO m => IO (Messages GhcMessage, Maybe a) -> m a
#elif MIN_VERSION_ghc(9,2,0)
ioMsgMaybe :: MonadIO m => IO (Messages DecoratedSDoc, Maybe a) -> m a
#else
ioMsgMaybe :: MonadIO m => IO (Messages, Maybe a) -> m a
#endif

#if MIN_VERSION_ghc(9,2,0)
ioMsgMaybe ioA = do
  -- XXX: Log warning messages.
  (msgs, mb_r) <- liftIO ioA
  let (_warns, errs) = partitionMessages msgs
  maybe (throwErrors errs) pure mb_r
#else
ioMsgMaybe ioA = do
  ((_warns, errs), mb_r) <- liftIO ioA
  maybe (liftIO (throwIO (mkSrcErr errs))) return mb_r
#endif
{-# INLINABLE ioMsgMaybe #-}

-- | GHC version compatibility helper for 'tcRnType'.
#if MIN_VERSION_ghc(9,4,0)
tcRnType'
  :: HscEnv -> Bool -> HType -> IO (Messages TcRnMessage, Maybe (Type, Kind))
#elif MIN_VERSION_ghc(9,2,0)
tcRnType'
  :: HscEnv -> Bool -> HType -> IO (Messages DecoratedSDoc, Maybe (Type, Kind))
#else
tcRnType'
  :: HscEnv -> Bool -> HType -> IO (Messages, Maybe (Type, Kind))
#endif

tcRnType' hsc_env = tcRnType hsc_env DefaultFlexi
{-# INLINABLE tcRnType' #-}
