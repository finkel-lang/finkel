-- | Builder functions for Haskell syntax data type.
--
-- This module contains 'Builder' data type and Haskell AST type
-- synonyms. The 'Builder' data type is used by Happy parser for
-- building various AST types.
--
-- The main purpose of AST type synonyms defined in this module are
-- for managing ghc version compatibility.
--
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.SK.Builder
  ( -- * Builders type and functions
    Builder(..)
  , BState(..)
  , SyntaxError(..)
  , syntaxErrMsg
  , syntaxErrCode
  , builderError
  , evalBuilder
  , failB
  , formLexer
  , getBState
  , parse
  , putBState
  , setLastToken
  , runBuilder

  -- * Type synonyms for ghc version compatibility
  -- $typesynonym
  , PARSED
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
import SrcLoc (Located, GenLocated(..), noLoc)

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

-- | Wrapper data for syntax error.
data SyntaxError = SyntaxError Code String
  deriving (Eq, Show)

-- | Newtype wrapper for parsing form data with Happy.
newtype Builder a = Builder {
    unBuilder :: StateT BState (Either SyntaxError) a
}

instance Functor Builder where
  fmap f (Builder m) = Builder (fmap f m)
  {-# INLINE fmap #-}

instance Applicative Builder where
  pure = return
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

-- | Run given 'Builder' with using given list of 'Code' as input.
runBuilder :: Builder a
           -> [Code]
           -> Either SyntaxError (a, [Code])
runBuilder bld toks =
  case runStateT (unBuilder bld) (BState toks Nothing) of
    Right (a, st) -> Right (a, inputs st)
    Left e        -> Left e

-- | Like 'runBuilder', but discards left over 'Code's.
evalBuilder :: Builder a -> [Code] -> Either SyntaxError a
evalBuilder bld toks = fmap fst (runBuilder bld toks)

-- | Fail builder computation with given message.
failB :: String -> Builder a
failB err = do
  mb_tok <- fmap lastToken getBState
  let be = SyntaxError tok err
      tok = case mb_tok of
              Just t  -> t
              Nothing -> LForm (noLoc (Atom AUnit))
  Builder (StateT (\_ -> Left be))

-- | Extract message from 'SyntaxError'.
syntaxErrMsg :: SyntaxError -> String
syntaxErrMsg (SyntaxError _ msg) = msg

-- | Extract code from 'SyntaxError'.
syntaxErrCode :: SyntaxError -> Code
syntaxErrCode (SyntaxError code _) = code

-- | Get current 'BState'.
getBState :: Builder BState
getBState = Builder get
{-# INLINE getBState #-}

-- | Put current 'BState'.
putBState :: BState -> Builder ()
putBState = Builder . put
{-# INLINE putBState #-}

-- | Set last token to given 'Code'.
setLastToken :: Code -> Builder ()
setLastToken code = do
  st <- getBState
  putBState (st {lastToken = Just code})
{-# INLINE setLastToken #-}

-- | Parse with builder using given tokens, continue on successful
-- parse.
parse :: Builder a -> [Code] -> Builder a
parse bld toks =
  case runBuilder bld toks of
    Right (a, _) -> return a
    Left err     -> Builder (StateT (\_ -> Left err))

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

-- | Show simple syntax error message with current 'Code'.
builderError :: Builder a
builderError = do
  st <- getBState
  case lastToken st of
    Nothing -> failB "syntax error"
    Just x  -> failB ("syntax error on input `" ++ show x ++ "'")


-- ---------------------------------------------------------------------
--
-- Type synonyms
--
-- ---------------------------------------------------------------------

-- $typesynonym
--
-- Type synonym for managing version compatibility.
--
-- This 'PARSED' type synonym is wrapped with CPP macro detecting the
-- ghc package version at compilation time.  At the time of initial
-- development of sk-kernel package, ghc source codes were not under the
-- /Trees that Grow/ modifications.  When updating from ghc 8.2.x to
-- 8.4.x, 'PARSED' were added to handle the AST argument type
-- modification.
--
-- See <https://ghc.haskell.org/trac/ghc/wiki/ImplementingTreesThatGrow>
-- for more information of \"Trees that Grow\".
--

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
