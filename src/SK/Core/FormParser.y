-- Happy parser for S-expression forms.
{
-- | Module for parsing form data.
--
-- Unlike the lexer for reading source code, parser defined in this
-- module expects list of 'Form' data as input, converts to Haskell
-- AST defined in GHC.
--
module SK.Core.FormParser
  ( Builder(..)
  , runBuilder
  , evalBuilder
  , evalBuilder'
  , parse_module
  , p_decl
  , p_expr
  , showLoc
  ) where

import Control.Monad (foldM, liftM, ap)
import Data.Char (isUpper)
import Data.List (foldl1')
import Data.Maybe (fromMaybe)

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State

import SK.Core.Form
import SK.Core.GHC
}

%name parse_module module
%name p_mod_header mod_header
%name p_import import
%name p_top_decl top_decl
%name p_decl decl
%name p_type type
%name p_types0 types0
%name p_lconst lconst

%name p_pats pats
%name p_pats0 pats0
%name p_pats1 pats1

%name p_expr expr
%name p_exprs exprs
%name p_match match
%name p_guards0 guards0
%name p_guards1 guards1
%name p_guard guard
%name p_lbinds0 lbinds0

%name p_stmt1 stmt1

%tokentype { LTForm Atom }
%monad { Builder }
%lexer { formLexer } { L _ TEnd }

%token

'module' { L _ (TAtom (ASymbol "module")) }
'import' { L _ (TAtom (ASymbol "import")) }
'if'     { L _ (TAtom (ASymbol "if")) }
'data'   { L _ (TAtom (ASymbol "data")) }
'do'     { L _ (TAtom (ASymbol "do")) }
'\\'     { L _ (TAtom (ASymbol "\\")) }
'let'    { L _ (TAtom (ASymbol "let")) }
'case'   { L _ (TAtom (ASymbol "case")) }

'='  { L _ (TAtom (ASymbol "=")) }
'<-' { L _ (TAtom (ASymbol "<-")) }
'->' { L _ (TAtom (ASymbol "->")) }
'::' { L _ (TAtom (ASymbol "::")) }
','  { L _ (TAtom (ASymbol ",")) }
'|'  { L _ (TAtom (ASymbol "|")) }

'symbol'  { L _ (TAtom (ASymbol _)) }
'char'    { L _ (TAtom (AChar _)) }
'string'  { L _ (TAtom (AString _)) }
'integer' { L _ (TAtom (AInteger _)) }
'frac'    { L _ (TAtom (AFractional _)) }
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
      | 'comment' { Just (b_commentStringE $1) }


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
       : 'import' 'symbol' { b_importD $2 }


--- ------------
--- Declarations

decls :: { [HDecl] }
      : rdecls { reverse $1 }

rdecls :: { [HDecl] }
      : decl_with_doc        { [$1] }
      | rdecls decl_with_doc { $2 : $1 }

decl_with_doc :: { HDecl }
              : mbdoc 'list' {% parse p_top_decl $2 }

top_decl :: { HDecl }
         : 'data' 'symbol' consts { b_dataD $2 $3 }
         | decl { $1 }

decl :: { HDecl }
     : '=' decl_lhs expr  { b_funD $1 $2 $3 }
     | '::' 'symbol' type { b_tsigD $2 $3 }

decl_lhs :: { HExpr -> HsBind RdrName }
         : 'list'   {% b_declLhsB $1 }
         | 'symbol' {% b_declLhsB [$1] }

consts :: { [HConDecl] }
       : rconsts { reverse $1 }

rconsts :: { [HConDecl] }
        : {- empty -}   { [] }
        | rconsts const { $2 : $1 }

const :: { HConDecl }
      : 'list' {% parse p_lconst $1 }

lconst :: { HConDecl }
       : 'symbol' types { b_conD $1 $2 }

--- ----
--- Type

type :: { HType }
     : 'symbol' { b_symT $1 }
     | 'unit'   { b_unitT $1 }
     | 'hslist' {% b_listT $1 }
     | 'list'   {% parse p_types0 $1 }

types0 :: { HType }
       : '->' type type { b_funT $1 $2 $3 }
       | ',' types      { b_tupT $1 $2 }
       | types          { b_appT $1 }

types :: { [HType] }
      : rtypes { reverse $1 }

rtypes :: { [HType] }
       : type        { [$1] }
       | rtypes type { $2 : $1 }


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
    : 'integer' { b_intP $1 }
    | 'symbol'  { b_symP $1 }
    | 'hslist'  {% b_hsListP $1 }
    | 'list'    {% parse p_pats1 $1 }

pats1 :: { HPat }
      : ',' pats0      { b_tupP $1 $2 }
      | 'symbol' pats0 { b_conP $1 $2 }


--- -----------
--- Expressions

expr :: { HExpr }
     : atom     { $1 }
     | 'hslist' {% b_hsListB $1 }
     | 'list'   {% parse p_exprs $1 }

atom :: { HExpr }
     : 'symbol'  { b_varE $1 }
     | 'char'    { b_charE $1 }
     | 'string'  { b_stringE $1 }
     | 'integer' { b_integerE $1 }
     | 'frac'    { b_floatE $1 }
     | 'unit'    { b_unitE $1 }

exprs :: { HExpr }
      : '\\' pats expr      { b_lamE $1 $2 $3 }
      | ',' app             { b_tupE $1 $2 }
      | 'let' lbinds expr   { b_letE $1 $2 $3 }
      | 'if' expr expr expr { b_ifE $1 $2 $3 $4 }
      | 'case' expr alts    { b_caseE $1 $2 $3 }
      | 'do' do_stmts       { b_doE $1 $2 }
      | '::' expr type      { b_tsigE $1 $2 $3 }
      | app                 { b_appE $1 }

lbinds :: { [HDecl] }
       : 'unit' { [] }
       | 'list' {% parse p_lbinds0 $1 }

lbinds0 :: { [HDecl] }
        : rlbinds0 { reverse $1 }

rlbinds0 :: { [HDecl] }
         : 'list'          {% fmap (:[]) (parse p_decl $1) }
         | rlbinds0 'list' {% fmap (:$1) (parse p_decl $2) }

app :: { [HExpr] }
    : rapp { reverse $1 }

rapp :: { [HExpr] }
     : expr      { [$1] }
     | rapp expr { $2 : $1 }

alts :: { [HMatch] }
     : ralts { reverse $1 }

ralts :: { [HMatch] }
      : 'list'       {% fmap (:[]) (parse p_match $1)  }
      | ralts 'list' {% fmap (:$1) (parse p_match $2) }

match :: { HMatch }
      : pat guards { b_match $1 $2 }

-- Parsing list form for guards
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Separating the rule for 'list' and atom, so that the 'guards0' rule
-- can try matching the symbol '|' before 'expr' rule, to differentiate
-- the entire form from function application of reserved symbol '|'.

guards :: { [HGRHS] }
       : 'list' {% parse p_guards0 $1 }
       | atom   { [noLoc (GRHS [] $1)] }

guards0 :: { [HGRHS] }
      : '|' guards1 { $2 }
      | exprs       { [noLoc (GRHS [] $1)] }

guards1 :: { [HGRHS] }
        : 'list'         {% b_hgrhs $1 [] }
        | 'list' guards1 {% b_hgrhs $1 $2 }

guard :: { (HExpr, [GuardLStmt RdrName]) }
      : expr       { ($1, []) }
      | stmt guard { fmap ($1:) $2 }


--- ------------
--- Do statement

do_stmts :: { [HExprLStmt] }
         : rdo_stmts { reverse $1 }

rdo_stmts :: { [HExprLStmt] }
          : stmt           { [$1] }
          | rdo_stmts stmt { $2 : $1 }

stmt :: { HExprLStmt }
        : atom   { b_bodyS $1 }
        | 'list' {% parse p_stmt1 $1 }

stmt1 :: { HExprLStmt }
         : '<-' pat expr { b_bindS $1 $2 $3 }
         | 'let' lbinds  { b_letS $1 $2 }
         | exprs         { b_bodyS $1 }


{
-- | State for 'Builder'.
data BState = BState
    { -- | Input tokens to parse.
      inputs :: [LTForm Atom]
      -- | Last token, for error message.
    , lastToken :: Maybe (LTForm Atom)
      -- | File path of input, if any.
    , inputPath :: Maybe FilePath
    }

-- | Newtype wrapper for parsing form data with Happy.
newtype Builder a = Builder {
    unBuilder :: StateT BState (Either String) a
}

runBuilder :: Builder a
           -> Maybe FilePath
           -> [LTForm Atom]
           -> Either String (a, [LTForm Atom])
runBuilder bld mbpath toks =
    case runStateT (unBuilder bld) (BState toks Nothing mbpath) of
      Left e -> Left e
      Right (a, st) -> Right (a, inputs st)

evalBuilder :: Builder a -> Maybe FilePath
            -> [LTForm Atom] -> Either String a
evalBuilder bld mbpath toks = fmap fst (runBuilder bld mbpath toks)

evalBuilder' :: Monad m => Builder a
             -> Maybe FilePath -> [LTForm Atom]
             -> ExceptT String m a
evalBuilder' bld mbpath toks = case evalBuilder bld mbpath toks of
  Right a -> return a
  Left err -> throwE err

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
  st <- getBState
  case runBuilder bld (inputPath st) toks of
    Right (a, _) -> return a
    Left err -> failB err

showLoc :: Located a -> String
showLoc x = case getLoc x of
      RealSrcSpan r ->
        show (srcSpanFile r) ++ ":" ++
        "line " ++ show (srcSpanStartLine r) ++ ", " ++
        "column " ++ show (srcSpanStartCol r)
      UnhelpfulSpan _ -> "unknown location"

happyError :: Builder a
happyError = do
  st <- getBState
  case lastToken st of
    Nothing -> failB "no location"
    Just x  ->
      -- XXX: 'x' is a 'Located' data, it should contain source file
      -- path information, so 'inputPath' field in 'st' is not
      -- necessary.
      let path = fromMaybe "unknown input" (inputPath st)
      in  failB (path ++ ": parse error at " ++ showLoc x ++ ": " ++
                       (show (lTFormToForm x)))

-- | Simple lexer to parse forms.
formLexer :: (LTForm Atom -> Builder a) -> Builder a
formLexer cont = do
    st <- getBState
    case inputs st of
      [] -> cont (L undefined TEnd)
      x:xs -> do
        putBState (st {inputs = xs, lastToken = Just x})
        cont x


---
--- Auxiliary
---

type HExpr = LHsExpr RdrName

type HDecl = LHsDecl RdrName

type HConDecl = LConDecl RdrName

type HBind = LHsBind RdrName

type HSigWcType = LHsSigWcType RdrName

type HType = LHsType RdrName

type HPat = LPat RdrName

type HExprLStmt = ExprLStmt RdrName

type HLocalBinds = Located (HsLocalBinds RdrName)

type HMatch = LMatch RdrName HExpr

type HGRHS = LGRHS RdrName HExpr

type HGuardLStmt = GuardLStmt RdrName

type HImportDecl = LImportDecl RdrName

-- XXX: Currently, cannot tell the difference between 'Qualified.fun'
-- and 'DataConstr'.
mkRdrName :: String -> RdrName
mkRdrName name@(x:_)
  -- ':' is special syntax. It is defined in module "GHC.Types" in
  -- package "ghc-prim", but not exported.
  | name == [':'] = nameRdrName consDataConName

  -- Data constructor starts from capital letter or ':'.
  | isUpper x || x == ':' = mkUnqual srcDataName (fsLit name)

  -- Variable.
  | otherwise = mkVarUnqual (fsLit name)


-- | Build 'HLocalBinds' from list of 'HDecl's.
declsToBinds :: Located a -> [HDecl] -> HLocalBinds
declsToBinds (L l _) decls = L l binds'
  where
    binds' = case decls of
      [] -> emptyLocalBinds
      _  -> HsValBinds (ValBindsIn (listToBag binds) sigs)
    (binds, sigs) = go ([],[]) decls
    go (bs,ss) ds =
      case ds of
        [] -> (bs, ss)
        d:ds' -> case d of
          L ld (ValD b) -> go (L ld b:bs,ss) ds'
          L ld (SigD s) -> go (bs,L ld s:ss) ds'

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

b_importD :: LTForm Atom -> HImportDecl
b_importD (L l (TAtom (ASymbol m))) =
    L l (simpleImportDecl (mkModuleName m))

b_dataD :: LTForm Atom -> [HConDecl] -> HDecl
b_dataD (L l (TAtom (ASymbol name))) cs = L l (TyClD decl)
  where
    decl = DataDecl { tcdLName = L l (mkUnqual tcName (fsLit name))
                    , tcdTyVars = tvs
                    , tcdDataDefn = defn
                    , tcdDataCusk = PlaceHolder
                    , tcdFVs = placeHolderNames }
    tvs = mkHsQTvs []  -- need more info.
    defn = HsDataDefn { dd_ND = DataType
                      , dd_ctxt = noLoc []
                      , dd_cType = Nothing
                      , dd_kindSig = Nothing
                      , dd_cons = cs
                      -- The field `dd_derivs' changed from ghc-8.0.2.
                      , dd_derivs = Nothing
                      }

b_conD :: LTForm Atom -> [HType] -> HConDecl
b_conD (L l1 (TAtom (ASymbol s1))) types =
    L l1 (ConDeclH98 { con_name = L l1 (mkUnqual srcDataName (fsLit s1))
                     , con_qvars = Nothing
                     , con_cxt = Nothing
                     , con_details = details
                     , con_doc = Nothing })
  where
    details = PrefixCon types

b_funD :: Located a -> (HExpr -> HsBind RdrName) -> HExpr -> HDecl
b_funD (L l _) f e = L l (ValD (f e))

b_declLhsB :: [LTForm Atom] -> Builder (HExpr -> HsBind RdrName)
b_declLhsB ((L l (TAtom (ASymbol name))):forms) = do
   args <- parse p_pats0 forms
   return (\body ->
             let match = mkMatch args body (L l emptyLocalBinds)
             in  mkFunBind (L l (mkRdrName name)) [match])

b_tsigD :: LTForm Atom -> HType -> HDecl
b_tsigD (L l (TAtom (ASymbol name))) typ =
  let typ' = mkLHsSigWcType typ
  in  L l (SigD (TypeSig [L l (mkRdrName name)] typ'))

-- Types

b_symT :: LTForm Atom -> HType
b_symT (L l (TAtom (ASymbol name))) = L l (HsTyVar (L l ty))
  where
    ty = mkUnqual namespace (fsLit name)
    namespace =
      case name of
        (x:_) | isUpper x || ':' == x -> tcName
        _ -> tvName

b_unitT :: LTForm Atom -> HType
b_unitT (L l _) = L l (HsTupleTy HsBoxedTuple [])

b_funT :: Located a -> HType -> HType -> HType
b_funT (L l _) a b = L l (HsFunTy a b)

b_appT :: [HType] -> HType
b_appT (x:xs) = foldl f x xs
  where f b a = L (getLoc b) (HsAppTy b a)

b_listT :: LTForm Atom -> Builder HType
b_listT (L l (THsList ty)) = do
  ty' <- parse p_type [L l (TList ty)]
  return (L l (HsListTy ty'))

b_tupT :: Located a -> [HType] -> HType
b_tupT (L l _) ts = L l (HsTupleTy HsBoxedTuple ts)

-- Pattern

b_intP :: LTForm Atom -> HPat
b_intP (L l (TAtom (AInteger n))) =
    let lit = (mkHsIntegral (show n) n placeHolderType)
    in  L l (mkNPat (L l lit) Nothing)

b_symP :: LTForm Atom -> HPat
b_symP (L l (TAtom (ASymbol name@(x:xs))))
   | name == "_" = L l (WildPat placeHolderType)
   | isUpper x || x == ':'
    = L l (ConPatIn (L l (mkRdrName name)) (PrefixCon []))
   | otherwise = L l (VarPat (L l (mkRdrName name)))

b_hsListP :: LTForm Atom -> Builder HPat
b_hsListP (L l (THsList xs)) = do
    pats <- parse p_pats0 xs
    return (L l (ListPat pats placeHolderType Nothing))

b_tupP :: Located a -> [HPat] -> HPat
b_tupP (L l _) ps = L l (TuplePat ps Boxed [])

b_conP :: LTForm Atom -> [HPat] -> HPat
b_conP (L l (TAtom (ASymbol con))) rest =
    L l (ConPatIn (L l (mkRdrName con)) (PrefixCon rest))

-- Expression

b_ifE :: LTForm Atom -> HExpr -> HExpr -> HExpr -> HExpr
b_ifE (L l (TAtom _)) p t f = L l (mkHsIf p t f)

b_lamE :: Located a -> [HPat] -> HExpr -> HExpr
b_lamE ref pats body = mkHsLam pats body

b_tupE :: Located a -> [HExpr] -> HExpr
b_tupE (L l _) args = L l (ExplicitTuple (map mkArg args) Boxed)
  where mkArg x@(L l n) = L l (Present x)

b_letE :: Located a -> [HDecl] -> HExpr -> HExpr
b_letE ref@(L l _) decls body =
    L l (HsLet (declsToBinds ref decls) body)

b_caseE :: Located a -> HExpr -> [HMatch] -> HExpr
b_caseE (L l _) expr matches = L l (HsCase expr mg)
  where mg = mkMatchGroup FromSource matches

b_match :: HPat -> [HGRHS] -> HMatch
b_match pat@(L l _) grhss =
    L l (Match NonFunBindMatch [pat] Nothing grhss')
  where
    grhss' = GRHSs grhss (noLoc emptyLocalBinds)

b_hgrhs :: [LTForm Atom] -> [HGRHS] -> Builder [HGRHS]
b_hgrhs forms rhss = do
  (body, gs) <- parse p_guard forms
  let rhs = GRHS gs body
      lrhs = case gs of
        [] -> noLoc rhs
        _  -> L (combineLocs (head gs) (last gs)) rhs
  return (lrhs:rhss)

b_grhs :: HExpr -> HExpr -> HGRHS
b_grhs guard@(L l _) body = L l (GRHS [L l (mkBodyStmt guard)] body)

b_tsigE :: Located a -> HExpr -> HType -> HExpr
b_tsigE (L l _) e t = L l (ExprWithTySig e (mkLHsSigWcType t))

b_doE :: Located a -> [HExprLStmt] -> HExpr
b_doE l exprs = L (getLoc l) (mkHsDo DoExpr exprs)

b_appE :: [HExpr] -> HExpr
b_appE = foldl1' (\a b -> L (getLoc a) (HsApp a b))

b_lbindB :: (HExpr -> HsBind RdrName) -> HExpr -> HBind
b_lbindB f e = L (getLoc e) (f e)

b_charE :: LTForm Atom -> HExpr
b_charE (L l (TAtom (AChar x))) = L l (HsLit (HsChar (show x) x))

b_stringE :: LTForm Atom -> HExpr
b_stringE (L l (TAtom (AString x))) = L l (HsLit (HsString x (fsLit x)))

b_integerE :: LTForm Atom -> HExpr
b_integerE (L l (TAtom (AInteger x))) =
    L l (HsOverLit $! (mkHsIntegral (show x) x placeHolderType))

b_floatE :: LTForm Atom -> HExpr
b_floatE (L l (TAtom (AFractional x))) =
   L l (HsOverLit $! (mkHsFractional x placeHolderType))

b_varE :: LTForm Atom -> HExpr
b_varE (L l (TAtom (ASymbol x))) = L l (HsVar (L l (mkRdrName x)))

b_unitE :: Located a -> HExpr
b_unitE (L l _) = L l (ExplicitTuple [] Boxed)

b_commentStringE :: LTForm Atom -> Located HsDocString
b_commentStringE (L l (TAtom (AComment x))) = L l (HsDocString (fsLit x))

--- Statement

b_bindS :: Located a -> HPat -> HExpr -> HExprLStmt
b_bindS ref pat expr = L (getLoc ref) (mkBindStmt pat expr)

b_letS :: Located a -> [HDecl] -> HExprLStmt
b_letS lref@(L l _) decls = L l (LetStmt (declsToBinds lref decls))

b_bodyS :: HExpr -> HExprLStmt
b_bodyS expr = L (getLoc expr) (mkBodyStmt expr)

b_hsListB :: LTForm Atom -> Builder HExpr
b_hsListB (L l (THsList xs)) = do
    xs' <- mapM (\x -> parse p_expr [x]) xs
    return (L l (ExplicitList placeHolderType Nothing xs'))

}
