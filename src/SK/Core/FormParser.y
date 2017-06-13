-- Happy parser for S-expression forms.
{
-- | Module for parsing form data.
--
-- Unlike the lexer for reading source code, parser defined in this
-- module expects list of 'Form' data as input and converting to Haskell
-- syntax defined in GHC.
--
module SK.Core.FormParser
  ( Builder(..)
  , runBuilder
  , evalBuilder
  , parse_module
  , p_decl
  , p_expr
  , showLoc
  ) where

import Control.Monad (foldM, liftM, ap)
import Data.Char (isUpper)
import Data.List (foldl1')

import Control.Monad.Trans.Class
import Control.Monad.Trans.State

import SK.Core.Form
import SK.Core.GHC
}

%name parse_module module
%name p_mod_header mod_header
%name p_import import
%name p_decl decl
%name p_pats pats
%name p_pats0 pats0
%name p_expr expr
%name p_exprs exprs
%name p_do_stmt1 do_stmt1
%name p_type type
%name p_types types

%tokentype { LTForm Atom }
%monad { Builder }
%lexer { formLexer } { L _ TEnd }

%token

'module' { L _ (TAtom (ASymbol "module")) }
'import' { L _ (TAtom (ASymbol "import")) }
'if'     { L _ (TAtom (ASymbol "if")) }
'lambda' { L _ (TAtom (ASymbol "\\")) }
'do'     { L _ (TAtom (ASymbol "do")) }

'='  { L _ (TAtom (ASymbol "=")) }
'<-' { L _ (TAtom (ASymbol "<-")) }
'->' { L _ (TAtom (ASymbol "->")) }
'::' { L _ (TAtom (ASymbol "::")) }

'symbol'  { L _ (TAtom (ASymbol _)) }
'string'  { L _ (TAtom (AString _)) }
'integer' { L _ (TAtom (AInteger _)) }
'comment' { L _ (TAtom (AComment _)) }
'unit'    { L _ (TAtom AUnit) }

'import_form' { L _ (TList $$@((L _ (TAtom (ASymbol "import"))):_)) }
'list'        { L _ (TList $$) }
'hslist'      { L _ (THsList _) }


%%

--- -------------
--- Documentation

mbdoc :: { Maybe LHsDocString }
      : {- empty -} { Nothing }
      | 'comment' { Just (b_commentString $1) }


--- ------
--- Module

module :: { HsModule RdrName }
       : mbdoc 'list' imports decls
         {% parse p_mod_header $2 <*> pure $1 <*> pure $3 <*> pure $4 }
       | mbdoc 'list' decls
         {% parse p_mod_header $2 <*> pure $1 <*> pure [] <*> pure $3 }
       | mbdoc 'list' imports
         {% parse p_mod_header $2 <*> pure $1 <*> pure $3 <*> pure [] }

mod_header :: { Maybe LHsDocString -> [HImportDecl] -> [HDecl]
               -> HsModule RdrName }
           : 'module' 'symbol' { b_module $2 }

imports :: { [HImportDecl] }
        : rimports { reverse $1 }

rimports :: { [HImportDecl] }
         : import_form          { [$1] }
         | rimports import_form { $2 : $1 }

import_form :: { HImportDecl }
            : 'import_form' {% parse p_import $1 }

import :: { HImportDecl }
       : 'import' 'symbol' { b_import $2 }


--- ------------
--- Declarations

decls :: { [HDecl] }
      : rdecls { reverse $1 }

rdecls :: { [HDecl] }
      : decl_with_doc        { [$1] }
      | rdecls decl_with_doc { $2 : $1 }

decl_with_doc :: { HDecl }
              : mbdoc 'list' {% parse p_decl $2 }

decl :: { HDecl }
     : '=' decl_lhs expr  { b_funD $1 $2 $3 }
     | '::' 'symbol' type { b_typeSig $2 $3 }

decl_lhs :: { HExpr -> HsBind RdrName }
         : 'list'   {% b_declLhsB $1 }
         | 'symbol' {% b_declLhsB [$1] }

--- ----
--- Type

type :: { HType }
     : 'symbol' { b_symType $1 }
     | 'unit'   { b_unitType $1 }
     | 'hslist' {% b_listT $1 }
     | 'list'   {% parse p_types $1 }

types :: { HType }
      : '->' type type { b_funT $1 $2 $3 }
      | appty          { b_appT $1 }

appty :: { [HType] }
      : rappty { reverse $1 }

rappty :: { [HType] }
       : type       { [$1] }
       | rappty type { $2 : $1 }


--- --------
--- Patterns

pats :: { [HPat] }
     : 'unit' { [] }
     | 'list' {% parse p_pats0 $1 }

pats0 :: { [HPat] }
      : rpats0 { reverse $1 }

rpats0 :: { [HPat] }
       : {- empty -} { [] }
       | rpats0 pat  { $2 : $1 }

pat :: { HPat }
    : 'integer' { b_intPat $1 }
    | 'symbol'  { b_symPat $1 }
    | 'list'    {% b_listPat $1 }


--- -----------
--- Expressions

expr :: { HExpr }
     : atom     { $1 }
     | 'hslist' {% b_hsList $1 }
     | 'list'   {% parse p_exprs $1 }

atom :: { HExpr }
     : 'string'  { b_string $1 }
     | 'integer' { b_integer $1 }
     | 'symbol'  { b_symbol $1 }
     | 'unit'    { b_unit $1 }

exprs :: { HExpr }
      : 'if' expr expr expr { b_if $1 $2 $3 $4 }
      | 'do' do_stmts       { b_do $1 $2 }
      | 'lambda' pats expr  { b_lambda $1 $2 $3 }
      | app                 { b_app $1 }

app :: { [HExpr] }
    : rapp { reverse $1 }

rapp :: { [HExpr] }
     : expr { [$1] }
     | rapp expr { $2 : $1 }

-- do expression

do_stmts :: { [HExprLStmt] }
         : rdo_stmts { reverse $1 }

rdo_stmts :: { [HExprLStmt] }
          : do_stmt           { [$1] }
          | rdo_stmts do_stmt { $2 : $1 }

do_stmt :: { HExprLStmt }
        : atom   { b_bodyStmt $1 }
        | 'list' {% parse p_do_stmt1 $1 }

do_stmt1 :: { HExprLStmt }
         : '<-' pat expr { b_bindStmt $1 $2 $3 }
         | exprs         { b_bodyStmt $1 }


{
-- | State for 'Builder'.
data BState = BState
    { -- | Input tokens to parse.
      inputs :: [LTForm Atom]
      -- | Last token, for error message.
    , lastToken :: Maybe (LTForm Atom)
    }

-- | Newtype wrapper for parsing form data with Happy.
newtype Builder a = Builder {
    unBuilder :: StateT BState (Either String) a
}

runBuilder :: Builder a -> [LTForm Atom]
           -> Either String (a, [LTForm Atom])
runBuilder bld toks =
    case runStateT (unBuilder bld) (BState toks Nothing) of
      Left e -> Left e
      Right (a, st) -> Right (a, inputs st)

evalBuilder :: Builder a -> [LTForm Atom] -> Either String a
evalBuilder bld toks  = fmap fst (runBuilder bld toks)

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
parse :: Builder a -> [LTForm Atom] -> Builder a
parse bld toks = do
  case runBuilder bld toks of
    Right (a, _) -> return a
    Left err -> failB err

showLoc :: Located a -> String
showLoc x = case getLoc x of
      RealSrcSpan r ->
        "line " ++ show (srcSpanStartLine r) ++ ", " ++
        "column " ++ show (srcSpanStartCol r)
      UnhelpfulSpan _ -> "unknown location"

happyError :: Builder a
happyError = do
  st <- getBState
  case lastToken st of
    Nothing -> failB "no location"
    Just x -> failB ("parse error at " ++ showLoc x ++ ": " ++
                       (show (lTFormToForm x)))

-- | Simple lexer to parse forms.
formLexer :: (LTForm Atom -> Builder a) -> Builder a
formLexer cont = do
    st <- getBState
    case inputs st of
      [] -> cont (L undefined TEnd)
      x:xs -> putBState (BState xs (Just x)) >> cont x


---
--- Auxiliary
---

type HExpr = LHsExpr RdrName

type HDecl = LHsDecl RdrName

type HBind = LHsBind RdrName

type HSigWcType = LHsSigWcType RdrName

type HType = LHsType RdrName

type HPat = LPat RdrName

type HExprLStmt = ExprLStmt RdrName

type HImportDecl = LImportDecl RdrName

-- XXX: Currently, cannot tell the difference between 'Qualified.fun'
-- and 'DataConstr'.
mkRdrName :: String -> RdrName
mkRdrName name@(x:_)
  | isUpper x = mkUnqual srcDataName (fsLit name)
  | otherwise = mkVarUnqual (fsLit name)


---
--- Builder functions
---

-- In GHC source code, there is a file "compiler/hsSyn/Convert.hs".
-- This module contains codes converting Template Haskell data types to
-- GHC's internal data type, which is a helpful resource for
-- understanding the values and types for constructing Haskell AST data.

b_module :: LTForm Atom -> Maybe LHsDocString -> [HImportDecl]
         -> [HDecl] -> HsModule RdrName
b_module (L l (TAtom (ASymbol name))) mbdoc imports decls =
    HsModule { hsmodName = Just (L l (mkModuleName name))
             , hsmodExports = Nothing
             , hsmodImports = imports
             -- Function `cvTopDecls' is used for mergeing multiple
             -- top-level FunBinds, which possibly taking different
             -- patterns in its arguments.
             , hsmodDecls = cvTopDecls (toOL decls)
             , hsmodDeprecMessage = Nothing
             , hsmodHaddockModHeader = mbdoc }

b_import :: LTForm Atom -> HImportDecl
b_import (L l (TAtom (ASymbol m))) =
    L l (simpleImportDecl (mkModuleName m))

b_funD :: Located a -> (HExpr -> HsBind RdrName) -> HExpr -> HDecl
b_funD (L l _) f e = L l (ValD (f e))

b_declLhsB :: [LTForm Atom] -> Builder (HExpr -> HsBind RdrName)
b_declLhsB ((L l (TAtom (ASymbol name))):forms) = do
   args <- parse p_pats0 forms
   return (\body ->
             let match = mkMatch args body (L l emptyLocalBinds)
             in  mkFunBind (L l (mkRdrName name)) [match])

-- Types

b_typeSig :: LTForm Atom -> HType -> HDecl
b_typeSig (L l (TAtom (ASymbol name))) typ =
  let typ' = mkLHsSigWcType typ
  in  L l (SigD (TypeSig [L l (mkRdrName name)] typ'))

b_symType :: LTForm Atom -> HType
b_symType (L l (TAtom (ASymbol name))) =
    L l (HsTyVar (L l (mkUnqual tcClsName (fsLit name))))

b_unitType :: LTForm Atom -> HType
b_unitType (L l _) = L l (HsTupleTy HsBoxedTuple [])

b_funT :: Located a -> HType -> HType -> HType
b_funT (L l _) a b = L l (HsFunTy a b)

b_appT :: [HType] -> HType
b_appT (x:xs) = foldl f x xs
  where f b a = L (getLoc b) (HsAppTy b a)

b_listT :: LTForm Atom -> Builder HType
b_listT (L l (THsList ty)) = do
  ty' <- parse p_type [L l (TList ty)]
  return (L l (HsListTy ty'))

-- Pattern

b_intPat :: LTForm Atom -> HPat
b_intPat (L l (TAtom (AInteger n))) =
    let lit = (mkHsIntegral (show n) n placeHolderType)
    in  L l (mkNPat (L l lit) Nothing)

b_symPat :: LTForm Atom -> HPat
b_symPat (L l (TAtom (ASymbol name@(x:xs))))
   | name == "_" = L l (WildPat placeHolderType)
   | isUpper x = L l (ConPatIn (L l (mkRdrName name)) (PrefixCon []))
   | otherwise = L l (VarPat (L l (mkRdrName name)))

b_listPat :: [LTForm Atom] -> Builder HPat
b_listPat (L l x:rest) =
    case x of
      TAtom (ASymbol name)
        | isUpper (head name) -> do
            pats <- parse p_pats0 rest
            let ps = PrefixCon pats
            return (L l (ConPatIn (L l (mkRdrName name)) ps))
        | otherwise -> failB "b_listPat: not yet supported"
      _ -> failB "b_listPat: non-symbol in head of list."

-- Expression

b_if :: LTForm Atom -> HExpr -> HExpr -> HExpr -> HExpr
b_if (L l (TAtom _)) p t f = L l (mkHsIf p t f)

b_lambda :: Located a -> [HPat] -> HExpr -> HExpr
b_lambda ref pats body = mkHsLam pats body

b_do :: Located a -> [HExprLStmt] -> HExpr
b_do l exprs = L (getLoc l) (mkHsDo DoExpr exprs)

b_app :: [HExpr] -> HExpr
b_app = foldl1' (\a b -> L (getLoc a) (HsApp a b))

b_bodyStmt :: HExpr -> HExprLStmt
b_bodyStmt expr = L (getLoc expr) (mkBodyStmt expr)

b_bindStmt :: Located a -> HPat -> HExpr -> HExprLStmt
b_bindStmt ref pat expr = L (getLoc ref) (mkBindStmt pat expr)

b_hsList :: LTForm Atom -> Builder HExpr
b_hsList (L l (THsList xs)) = do
    xs' <- mapM (\x -> parse p_expr [x]) xs
    return (L l (ExplicitList placeHolderType Nothing xs'))

b_string :: LTForm Atom -> HExpr
b_string (L l (TAtom (AString x))) = L l (HsLit (HsString x (fsLit x)))

b_integer :: LTForm Atom -> HExpr
b_integer (L l (TAtom (AInteger x))) =
    L l (HsOverLit (mkHsIntegral (show x) x placeHolderType))

b_symbol :: LTForm Atom -> HExpr
b_symbol (L l (TAtom (ASymbol x))) = L l (HsVar (L l (mkRdrName x)))

b_unit :: Located a -> HExpr
b_unit (L l _) = L l (ExplicitTuple [] Boxed)

b_commentString :: LTForm Atom -> Located HsDocString
b_commentString (L l (TAtom (AComment x))) = L l (HsDocString (fsLit x))
}
