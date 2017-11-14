-- | Builder functions for Haskell syntax data type.
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
  , mkRdrName
  , splitQualName

  -- * Type synonyms
  , HBind
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
  , HSigWcType
  , HStmt
  , HTyVarBndr
  , HType
  ) where

-- base
import Control.Monad (ap, liftM)
import Data.Char (isUpper)

-- qualified import of OccName.varName from ghc package. The entity
-- `varName' conflicts with `Var.varName'.
import qualified OccName (varName)

-- transformers
import Control.Monad.Trans.State (StateT(..), get, put)

-- Internal
import Language.SK.GHC
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
      Left e -> Left e
      Right (a, st) -> Right (a, inputs st)

evalBuilder :: Builder a -> [Code] -> Either String a
evalBuilder bld toks = fmap fst (runBuilder bld toks)

failB :: String -> Builder a
failB err = Builder (StateT (\_ -> Left err))

instance Functor Builder where
    fmap = liftM

instance Applicative Builder where
    pure = return
    (<*>) = ap

instance Monad Builder where
  return a = Builder (return a)
  m >>= k  =
      Builder
        (StateT (\st ->
                   case runStateT (unBuilder m) st of
                     Right (a,st') -> runStateT (unBuilder (k a)) st'
                     Left err -> Left err))

getBState :: Builder BState
getBState = Builder get

putBState :: BState -> Builder ()
putBState = Builder . put

-- | Parse with builder using given tokens, continue on successful
-- parse.
parse :: Builder a -> [Code] -> Builder a
parse bld toks =
  case runBuilder bld toks of
    Right (a, _) -> return a
    Left err -> failB err

-- | Simple lexer to parse forms.
formLexer :: (Code -> Builder a) -> Builder a
formLexer cont = do
    st <- getBState
    case inputs st of
      [] -> cont (LForm (L undefined TEnd))
      x:xs -> do
        putBState (st {inputs = xs, lastToken = Just x})
        cont x

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

type HBind = LHsBind RdrName

type HCCallConv = Located CCallConv

type HConDecl = LConDecl RdrName

type HConDeclDetails = HsConDeclDetails RdrName

type HConDeclField = LConDeclField RdrName

type HDecl = LHsDecl RdrName

type HDeriving = HsDeriving RdrName

type HExpr = LHsExpr RdrName

type HGRHS = LGRHS RdrName HExpr

type HGuardLStmt = GuardLStmt RdrName

type HIE = LIE RdrName

type HIEWrappedName = LIEWrappedName RdrName

type HImportDecl = LImportDecl RdrName

type HLocalBinds = Located (HsLocalBinds RdrName)

type HMatch = LMatch RdrName HExpr

type HModule = HsModule RdrName

type HPat = LPat RdrName

type HSigWcType = LHsSigWcType RdrName

type HStmt = ExprLStmt RdrName

type HTyVarBndr = LHsTyVarBndr RdrName

type HType = LHsType RdrName


-- ---------------------------------------------------------------------
--
-- Auxiliary
--
-- ---------------------------------------------------------------------

mkRdrName :: FastString -> RdrName
mkRdrName name
  -- ':' is special syntax. It is defined in module "GHC.Types" in
  -- package "ghc-prim", but not exported.
  | name == ":" = nameRdrName consDataConName

  -- Name starting with ':' is data constructor.
  | x == ':' = mkUnqual srcDataName name

  -- Name starting with capital letters may qualified var name or data
  -- constructor name.
  | isUpper x =
    case splitQualName name of
      Nothing -> mkUnqual srcDataName name
      Just q@(_, name')
         | isUpper y || y == ':' -> mkQual tcName q
         | otherwise             -> mkQual OccName.varName q
         where
           y = headFS name'

  -- Variable.
  | otherwise = mkVarUnqual name
  where
    x = headFS name

splitQualName :: FastString -> Maybe (FastString, FastString)
splitQualName fstr = go (unpackFS fstr) "" []
  where
    go str0 tmp acc =
      case str0 of
        [] | null acc  -> Nothing
           | otherwise ->
             let mdl = reverse (tail (concat acc))
                 var = (reverse tmp)
             in  Just (fsLit mdl , fsLit var)
        c:str1
           | c == '.' ->
             case str1 of
               [] -> go str1 (c:tmp) acc
               _  -> go str1 [] ((c:tmp) : acc)
           | otherwise -> go str1 (c:tmp) acc
