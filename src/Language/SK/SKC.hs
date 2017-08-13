{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Wrapper for SK code compilation monad.

module Language.SK.SKC
  ( Skc(..)
  , SkEnv(..)
  , SkException(..)
  , Macro(..)
  , EnvMacros
  , handleSkException
  , debugIO
  , toGhc
  , fromGhc
  , failS
  , skSrcError
  , getSkEnv
  , putSkEnv
  , insertMacro
  , getEnvMacros
  , putEnvMacros
  , lookupMacro
  , macroNames
  ) where

-- base
import Control.Exception (Exception(..))
import Control.Monad (when)

-- containers
import qualified Data.Map as Map

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

-- Internal
import Language.SK.Form
import Language.SK.GHC


-- ---------------------------------------------------------------------
--
-- Exception
--
-- ---------------------------------------------------------------------

newtype SkException = SkException String
  deriving (Eq, Show)

instance Exception SkException

handleSkException :: ExceptionMonad m
                  => (SkException -> m a) -> m a -> m a
handleSkException = ghandle


-- ---------------------------------------------------------------------
--
-- Skc monad
--
-- ---------------------------------------------------------------------

-- | Macro transformer function.
--
-- A macro in SK is implemented as a function. The function takes a
-- single located code data argument, returns a located code data
-- wrapped in 'Skc'.
data Macro
  = Macro (Code -> Skc Code)
  | SpecialForm (Code -> Skc Code)

type EnvMacros = Map.Map FastString Macro

-- | Environment state in 'Skc'.
data SkEnv = SkEnv
   { -- | Association list of macros.
     envMacros :: EnvMacros
     -- | Flag to hold debug setting.
   , envDebug :: Bool
     -- | Modules to import to context.
   , envContextModules :: [String]
   }

-- | Newtype wrapper for compiling SK code to Haskell AST.
newtype Skc a = Skc {
  unSkc :: StateT SkEnv Ghc a
} deriving (Functor, Applicative, Monad, MonadIO)

instance ExceptionMonad Skc where
  gcatch m h =
    Skc (StateT (\st ->
                   (toGhc m st `gcatch` \e -> toGhc (h e) st)))
  gmask f =
    let g r m = Skc (StateT (r . toGhc m))
    in  Skc (StateT (\st ->
                       (gmask (\r -> toGhc (f (g r)) st))))

instance HasDynFlags Skc where
   getDynFlags = Skc (lift getDynFlags)

instance GhcMonad Skc where
   getSession = Skc (lift getSession)
   setSession s = Skc (lift (setSession s))

toGhc :: Skc a -> SkEnv -> Ghc (a, SkEnv)
toGhc m = runStateT (unSkc m)

fromGhc :: Ghc a -> Skc a
fromGhc m = Skc (lift m)

failS :: String -> Skc a
failS msg = liftIO (throwIO (SkException msg))

-- | Throw a 'SourceError'.
skSrcError :: Code -> String -> Skc a
skSrcError (LForm (L l _)) msg = do
  dflags <- getSessionDynFlags
  let em = mkErrMsg dflags l neverQualify (text msg)
  liftIO (throwIO (mkSrcErr (unitBag em)))

-- | Perform given IO action iff debug flag is turned on.
debugIO :: IO () -> Skc ()
debugIO act = Skc go
  where
    go = do
      sk_env <- get
      when (envDebug sk_env)
           (liftIO act)

getSkEnv :: Skc SkEnv
getSkEnv = Skc get

putSkEnv :: SkEnv -> Skc ()
putSkEnv = Skc . put

getEnvMacros :: Skc EnvMacros
getEnvMacros = envMacros <$> getSkEnv

putEnvMacros :: EnvMacros -> Skc ()
putEnvMacros macros = do
  e <- getSkEnv
  putSkEnv (e {envMacros=macros})

-- | Insert new macro. This function will override existing macro.
insertMacro :: FastString -> Macro -> Skc ()
insertMacro k v = Skc go
  where
    go = modify (\e -> e {envMacros = Map.insert k v (envMacros e)})

-- | Lookup macro by name.
lookupMacro :: FastString -> EnvMacros -> Maybe Macro
lookupMacro = Map.lookup

-- | All macros in given macro environment, filtering out the special
-- forms.
macroNames :: EnvMacros -> [String]
macroNames = Map.foldrWithKey f []
  where
    f k m acc = case m of
                  Macro _ -> unpackFS k : acc
                  _       -> acc
