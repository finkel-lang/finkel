-- | Builder functions for Haskell syntax data type.
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.SK.Builder
  ( -- * Builders type and functions
    Builder(..)
  , BState(..)
  , builderError
  , evalBuilder
  , failB
  , formLexer
  , getBState
  , parse
  , putBState
  , runBuilder

  -- * Type synonyms
  , HBind
  , HBinds
  , HCCallConv
  , HConDecl
  , HConDeclDetails
  , HConDeclField
  , HDecl
  , HDeriving
  , HExpr
  , HGRHS
  , HGuardLStmt
  , HIE
  , HIEWrappedName
  , HImportDecl
  , HLocalBinds
  , HMatch
  , HModule
  , HPat
  , HSig
  , HSigWcType
  , HStmt
  , HTyVarBndr
  , HType

  -- * For ghc version compatibility
  , PARSED
  ) where

-- ghc
import Bag (Bag)
import ForeignCall (CCallConv(..))
import HsBinds (HsLocalBinds, LHsBind, LSig)
import HsDecls (HsConDeclDetails, HsDeriving, LConDecl, LHsDecl)
import HsExpr (ExprLStmt, GuardLStmt, LGRHS, LHsExpr, LMatch)
import HsImpExp (LIE, LIEWrappedName, LImportDecl)
import HsPat (LPat)
import HsSyn (HsModule)
import HsTypes (LConDeclField, LHsSigWcType, LHsTyVarBndr, LHsType)
import SrcLoc (Located)

#if MIN_VERSION_ghc (8,4,0)
import HsExtension (GhcPs)
#else
import RdrName (RdrName)
#endif

-- transformers
import Control.Monad.Trans.State (StateT(..), get, put)

-- Internal
import Language.SK.Form


-- -------------------------------------------------------------------
--
-- Builder data type
--
-- -------------------------------------------------------------------

-- | State for 'Builder'.
data BState = BState
    { -- | Input tokens to parse.
      inputs :: [Code]
      -- | Last token, for error message.
    , lastToken :: Maybe Code
    }

-- | Newtype wrapper for parsing form data with Happy.
newtype Builder a = Builder {
    unBuilder :: StateT BState (Either String) a
}

runBuilder :: Builder a
           -> [Code]
           -> Either String (a, [Code])
runBuilder bld toks =
  case runStateT (unBuilder bld) (BState toks Nothing) of
    Right (a, st) -> Right (a, inputs st)
    Left e        -> Left e

evalBuilder :: Builder a -> [Code] -> Either String a
evalBuilder bld toks = fmap fst (runBuilder bld toks)

failB :: String -> Builder a
failB err = Builder (StateT (\_ -> Left err))

instance Functor Builder where
  fmap f (Builder m) = Builder (fmap f m)
  {-# INLINE fmap #-}

instance Applicative Builder where
  pure  = return
  {-# INLINE pure #-}
  Builder m <*> Builder f = Builder (m <*> f)
  {-# INLINE (<*>) #-}

instance Monad Builder where
  return a = Builder (a `seq` return a)
  {-# INLINE return #-}
  m >>= k  =
      Builder
        (StateT (\st ->
                   case runStateT (unBuilder m) st of
                     Right (a,st') ->
                       let m' = a `seq` k a
                       in  m' `seq` runStateT (unBuilder m') st'
                     Left err -> Left err))
  {-# INLINE (>>=) #-}

getBState :: Builder BState
getBState = Builder get
{-# INLINE getBState #-}

putBState :: BState -> Builder ()
putBState = Builder . put
{-# INLINE putBState #-}

-- | Parse with builder using given tokens, continue on successful
-- parse.
parse :: Builder a -> [Code] -> Builder a
parse bld toks =
  case runBuilder bld toks of
    Right (a, _) -> return a
    Left err     -> failB err

-- | Simple lexer to parse forms.
formLexer :: (Code -> Builder a) -> Builder a
formLexer cont = do
    st <- getBState
    case inputs st of
      []   -> cont (LForm (L undefined TEnd))
      x:xs -> do
        putBState (st {inputs = xs, lastToken = Just x})
        cont x
{-# INLINE formLexer #-}

builderError :: Builder a
builderError = do
  st <- getBState
  case lastToken st of
    Nothing -> failB "no location"
    Just x  -> failB (showLoc x ++
                      "syntax error on input `" ++ show x ++ "'")

-- ---------------------------------------------------------------------
--
-- Type synonyms
--
-- ---------------------------------------------------------------------

#if MIN_VERSION_ghc(8,4,0)
type PARSED = GhcPs
#else
type PARSED = RdrName
#endif

type HBind = LHsBind PARSED

type HBinds = Bag (LHsBind PARSED)

type HCCallConv = Located CCallConv

type HConDecl = LConDecl PARSED

type HConDeclDetails = HsConDeclDetails PARSED

type HConDeclField = LConDeclField PARSED

type HDecl = LHsDecl PARSED

type HDeriving = HsDeriving PARSED

type HExpr = LHsExpr PARSED

type HGRHS = LGRHS PARSED HExpr

type HGuardLStmt = GuardLStmt PARSED

type HIE = LIE PARSED

type HIEWrappedName = LIEWrappedName PARSED

type HImportDecl = LImportDecl PARSED

type HLocalBinds = Located (HsLocalBinds PARSED)

type HMatch = LMatch PARSED HExpr

type HModule = HsModule PARSED

type HPat = LPat PARSED

type HSig = LSig PARSED

type HSigWcType = LHsSigWcType PARSED

type HStmt = ExprLStmt PARSED

type HTyVarBndr = LHsTyVarBndr PARSED

type HType = LHsType PARSED
