{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Builder functions for Haskell syntax data type.
--
-- This module contains 'Builder' data type and Haskell AST type synonyms. The
-- 'Builder' data type is used by Happy parser for building various AST types.
--
-- The main purpose of AST type synonyms defined in this module are for managing
-- ghc version compatibility.
--
module Language.Finkel.Builder
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
  , HConDeclGADTDetails
  , HConDeclH98Details
  , HConDeclField
  , HDecl
  , HDeriving
  , HDerivStrategy
  , HExpr
  , HGRHS
  , HGuardLStmt
  , HIE
  , HIEWrappedName
  , HImportDecl
  , HKind
  , HLocalBinds
  , HMatch
  , HModule
  , HPat
  , HSig
  , HSigType
  , HSigWcType
  , HStmt
  , HTyVarBndr
  , HTyVarBndrSpecific
  , HTyVarBndrVis
  , HType

  -- * Function names for @:quote@
  , Quote
  , qListS
  , qHsListS
  , qSymbolS
  , qCharS
  , qStringS
  , qIntegerS
  , qFractionalS
  , qUnitS
  , quoteWith

  ) where

#include "ghc_modules.h"

-- ghc
import GHC_Data_Bag             (Bag)
import GHC_Data_FastString      (FastString, appendFS)
import GHC_Driver_Session       (DynFlags)
import GHC_Hs                   (HsModule)
import GHC_Hs_Binds             (HsLocalBinds, LHsBind, LSig)
import GHC_Hs_Decls             (HsDeriving, LConDecl, LDerivStrategy, LHsDecl)
import GHC_Hs_Expr              (ExprLStmt, GuardLStmt, LGRHS, LHsExpr, LMatch)
import GHC_Hs_Extension         (GhcPs)
import GHC_Hs_ImpExp            (LIE, LIEWrappedName, LImportDecl)
import GHC_Hs_Pat               (LPat)
import GHC_Hs_Type              (LConDeclField, LHsSigType, LHsSigWcType,
                                 LHsTyVarBndr, LHsType)
import GHC_Parser_Lexer         (PState (..))
import GHC_Types_ForeignCall    (CCallConv (..))
import GHC_Types_SrcLoc         (Located, noLoc)

#if MIN_VERSION_ghc(9,8,0)
import GHC.Hs.Type              (HsBndrVis (..))
#endif

#if MIN_VERSION_ghc(9,4,0)
import GHC.Driver.Config.Parser (initParserOpts)
#elif MIN_VERSION_ghc(9,2,0)
import GHC.Driver.Config        (initParserOpts)
#endif

#if MIN_VERSION_ghc(9,2,0)
import GHC.Hs.Decls             (HsConDeclGADTDetails, HsConDeclH98Details)
import GHC_Parser_Lexer         (initParserState)
#else
import GHC_Hs_Decls             (HsConDeclDetails)
import GHC_Parser_Lexer         (mkPState)
#endif

#if MIN_VERSION_ghc(9,0,0)
import GHC_Types_Var            (Specificity (..))
#endif

-- Internal
import Language.Finkel.Form


-- -------------------------------------------------------------------
--
-- Builder data type
--
-- -------------------------------------------------------------------

-- | State for 'Builder'.
data BState = BState
    { -- | Input tokens to parse.
      inputs       :: [Code]
      -- | The 'PState' used for parser from GHC.
    , ghcPState    :: PState
      -- | Last token, for error message.
    , lastToken    :: Maybe Code
      -- | Whether to use qualified functions when quoting.
    , qualifyQuote :: Bool
    }

-- | Wrapper data for syntax error.
data SyntaxError = SyntaxError Code String
  deriving (Eq, Show)

-- | Newtype wrapper for parsing list of 'Code' with Happy.
--
-- Implements simple state monad with result value wrapped with 'Either', to
-- terminate parsing computation with 'SyntaxError'.
newtype Builder a =
  Builder { unBuilder :: BState -> Either SyntaxError (a, BState) }

instance Functor Builder where
  fmap f (Builder m) =
    Builder (\st0 -> do (a, st1) <- m st0
                        return (f a, st1))
  {-# INLINE fmap #-}

instance Applicative Builder where
  pure x = Builder (\st -> pure (x, st))
  {-# INLINE pure #-}
  Builder f <*> Builder m =
    Builder (\st0 -> do (g, st1) <- f st0
                        (v, st2) <- m st1
                        return (g v, st2))
  {-# INLINE (<*>) #-}

instance Monad Builder where
  Builder m >>= k =
    Builder (\st0 -> do (a, st1) <- m st0
                        unBuilder (k a) st1)
  {-# INLINE (>>=) #-}

-- | Run given 'Builder' with using given list of 'Code' as input.
runBuilder :: DynFlags
           -> Bool
           -> Builder a
           -> [Code]
           -> Either SyntaxError (a, [Code])
runBuilder dflags qualify bld toks =
  let buf = error "PState StringBuffer is empty"
      rl  = error "PState RealSrcLoc is empty"
#if MIN_VERSION_ghc(9,2,0)
      ps  = initParserState (initParserOpts dflags) buf rl
#else
      ps  = mkPState dflags buf rl
#endif
  in  case unBuilder bld (BState toks ps Nothing qualify) of
        Right (a, st) -> Right (a, inputs st)
        Left err      -> Left err

-- | Like 'runBuilder', but discards left over 'Code's.
evalBuilder :: DynFlags -> Bool -> Builder a -> [Code] -> Either SyntaxError a
evalBuilder dflags qualify bld toks =
  fmap fst (runBuilder dflags qualify bld toks)

-- | Fail builder computation with given message.
failB :: String -> Builder a
failB err = do
  mb_tok <- fmap lastToken getBState
  let tok = case mb_tok of
              Just t  -> t
              Nothing -> LForm (noLoc (Atom AUnit))
  Builder (const (Left (SyntaxError tok err)))

-- | Extract message from 'SyntaxError'.
syntaxErrMsg :: SyntaxError -> String
syntaxErrMsg (SyntaxError _ msg) = msg

-- | Extract code from 'SyntaxError'.
syntaxErrCode :: SyntaxError -> Code
syntaxErrCode (SyntaxError code _) = code

-- | Get current 'BState'.
getBState :: Builder BState
getBState = Builder (\st -> Right (st,st))
{-# INLINABLE getBState #-}

-- | Put current 'BState'.
putBState :: BState -> Builder ()
putBState st = Builder (\_ -> Right ((), st))
{-# INLINABLE putBState #-}

-- | Set last token to given 'Code'.
setLastToken :: Code -> Builder ()
setLastToken code = do
  st <- getBState
  putBState (st {lastToken = Just code})
{-# INLINABLE setLastToken #-}

-- | Parse with builder using given tokens, continue on successful parse.
parse :: Builder a -> [Code] -> Builder a
parse bld toks =
  do bstate <- getBState
     let pstate = ghcPState bstate
         qualify = qualifyQuote bstate
     case unBuilder bld (BState toks pstate Nothing qualify) of
       Right (a, _) -> return a
       Left err     -> Builder (const (Left err))

-- | Simple lexer to parse forms.
formLexer :: (Code -> Builder a) -> Builder a
formLexer cont = do
    st <- getBState
    case inputs st of
      []   -> cont (LForm (noLoc TEnd))
      x:xs -> do
        putBState (st {inputs = xs, lastToken = Just x})
        cont x
{-# INLINABLE formLexer #-}

-- | Show simple syntax error message with current 'Code'.
builderError :: Builder a
builderError = do
  st <- getBState
  case lastToken st of
    Nothing -> failB "Syntax error"
    Just x  -> failB ("Syntax error on input `" ++ show x ++ "'")


-- ---------------------------------------------------------------------
--
-- Type synonyms
--
-- ---------------------------------------------------------------------

-- $typesynonym
--
-- Type synonyms for managing GHC version compatibility.
--
-- This 'PARSED' type synonym is wrapped with CPP macro detecting the ghc
-- package version at compilation time.  At the time of initial development of
-- finkel-kernel package, ghc source codes were not under the /Trees that Grow/
-- modifications.  When updating from ghc 8.2.x to 8.4.x, 'PARSED' were added to
-- handle the AST argument type modification.
--
-- See
-- <https://gitlab.haskell.org/ghc/ghc/wikis/implementing-trees-that-grow>
-- for more information of \"Trees that Grow\".
--

type PARSED = GhcPs

type HBind = LHsBind PARSED

type HBinds = Bag (LHsBind PARSED)

type HCCallConv = Located CCallConv

type HConDecl = LConDecl PARSED

#if MIN_VERSION_ghc(9,2,0)
type HConDeclH98Details = HsConDeclH98Details PARSED
type HConDeclGADTDetails = HsConDeclGADTDetails PARSED
#else
-- In ghc < 9.2, constructor details were not saparated, internal
-- representations are same.
type HConDeclH98Details = HsConDeclDetails PARSED
type HConDeclGADTDetails = HsConDeclDetails PARSED
#endif

type HConDeclField = LConDeclField PARSED

type HDecl = LHsDecl PARSED

type HDeriving = HsDeriving PARSED

type HDerivStrategy = LDerivStrategy PARSED

type HExpr = LHsExpr PARSED

type HGRHS = LGRHS PARSED HExpr

type HGuardLStmt = GuardLStmt PARSED

type HIE = LIE PARSED

type HIEWrappedName = LIEWrappedName PARSED

type HImportDecl = LImportDecl PARSED

type HKind = HType

#if MIN_VERSION_ghc(9,2,0)
type HLocalBinds = HsLocalBinds PARSED
#else
type HLocalBinds = Located (HsLocalBinds PARSED)
#endif

type HMatch = LMatch PARSED HExpr

#if MIN_VERSION_ghc(9,6,0)
type HModule = HsModule PARSED
#elif MIN_VERSION_ghc(9,0,0)
type HModule = HsModule
#else
type HModule = HsModule PARSED
#endif

type HPat = LPat PARSED

type HSig = LSig PARSED

type HSigType = LHsSigType PARSED

type HSigWcType = LHsSigWcType PARSED

type HStmt = ExprLStmt PARSED

#if MIN_VERSION_ghc(9,8,0)
type HTyVarBndr = LHsTyVarBndr () PARSED
type HTyVarBndrSpecific = LHsTyVarBndr Specificity PARSED
type HTyVarBndrVis = LHsTyVarBndr (HsBndrVis PARSED) PARSED
#elif MIN_VERSION_ghc(9,0,0)
type HTyVarBndr = LHsTyVarBndr () PARSED
type HTyVarBndrSpecific = LHsTyVarBndr Specificity PARSED
type HTyVarBndrVis = HTyVarBndr
#else
type HTyVarBndr = LHsTyVarBndr PARSED
type HTyVarBndrSpecific = HTyVarBndr
type HTyVarBndrVis = HTyVarBndr
#endif

type HType = LHsType PARSED

-- ---------------------------------------------------------------------
--
-- Function names for ":quote"
--
-- ---------------------------------------------------------------------


-- Note: [Qualified names for quoting functions]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Quoting functions can use qualified name after expansion, to support quote in
-- REPL without importing the "Language.Finkel" module.  See how
-- "Opt_ImplicitImportQualified" flag is set in initialization code of Finkel
-- REPL in "finkel-tool" package.

type Quote = Bool -> FastString

quoteWith :: FastString -> Quote
quoteWith name qualify =
  if qualify
     then appendFS "Language.Finkel."  name
     else name
{-# INLINABLE quoteWith #-}

qListS :: Quote
qListS = quoteWith "qList"
{-# INLINABLE qListS #-}

qHsListS :: Quote
qHsListS = quoteWith "qHsList"
{-# INLINABLE qHsListS #-}

qSymbolS :: Quote
qSymbolS = quoteWith "qSymbol"
{-# INLINABLE qSymbolS #-}

qCharS :: Quote
qCharS = quoteWith "qChar"
{-# INLINABLE qCharS #-}

qStringS :: Quote
qStringS = quoteWith "qString"
{-# INLINABLE qStringS #-}

qIntegerS :: Quote
qIntegerS = quoteWith "qInteger"
{-# INLINABLE qIntegerS #-}

qFractionalS :: Quote
qFractionalS = quoteWith "qFractional"
{-# INLINABLE qFractionalS #-}

qUnitS :: Quote
qUnitS = quoteWith "qUnit"
{-# INLINABLE qUnitS #-}
