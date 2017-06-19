{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Wrapper for SK code compilation monad.

module SK.Core.SKC
  ( Skc(..)
  , SkEnv
  , Macro
  , LMacro
  , toGhc
  , fromGhc
  , failS
  , extendMacroEnv
  , getMacroEnv
  ) where

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State

-- Internal
import SK.Core.Form
import SK.Core.GHC

-- | Newtype wrapper for compiling SK code to Haskell AST.
newtype Skc a = Skc {
  unSkc :: StateT SkEnv (ExceptT String Ghc) a
} deriving (Functor, Applicative, Monad, MonadIO)

instance ExceptionMonad Skc where
  gcatch m h =
    Skc (StateT (\st ->
                   (ExceptT
                      (toGhc m st `gcatch` \e -> toGhc (h e) st))))
  gmask f =
    let g r m = Skc (StateT (\st -> ExceptT (r (toGhc m st))))
    in  Skc (StateT (\st ->
                       ExceptT (gmask (\r -> toGhc (f (g r)) st))))

instance HasDynFlags Skc where
   getDynFlags = Skc (lift getDynFlags)

instance GhcMonad Skc where
   getSession = Skc (lift (lift getSession))
   setSession s = Skc (lift (lift (setSession s)))

-- | Macro transformer function.
type Macro = Form Atom -> Skc (Form Atom)

-- | Macro transformer with location information preserved.
type LMacro = LTForm Atom -> Skc (LTForm Atom)

-- | Type of state in 'SKC'.
type SkEnv = [(String, LMacro)]

toGhc :: Skc a -> SkEnv -> Ghc (Either String (a, SkEnv))
toGhc m st = runExceptT (runStateT (unSkc m) st)

fromGhc :: Ghc a -> Skc a
fromGhc m = Skc (lift (lift m))

failS :: String -> Skc a
failS msg = Skc (lift (throwE msg))

getMacroEnv :: Skc [(String, LMacro)]
getMacroEnv = Skc get

extendMacroEnv :: String -> LMacro -> Skc ()
extendMacroEnv name mac = Skc go
  where
    go = do
      env <- get
      put ((name, mac) : env)
