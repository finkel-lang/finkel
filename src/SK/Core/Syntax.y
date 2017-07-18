-- Happy parser for S-expression forms.
{
-- | Module for parsing form data.
--
-- Unlike the lexer for reading source code, parser defined in this
-- module expects list of 'Form' data as input, converts to Haskell
-- AST defined in GHC.
--
module SK.Core.Syntax
  ( Builder(..)
  , runBuilder
  , evalBuilder
  , evalBuilder'
  , parseModule
  , parseImports
  , parseStmt
  , parseDecls
  , parseExpr
  ) where

import SK.Core.Builder
import SK.Core.Form
import SK.Core.GHC
}

%name parse_module module
%name p_mod_header mod_header

%name p_imports imports
%name p_import_form import_form
%name p_import import

%name p_top_decls top_decls
%name p_top_decl top_decl
%name p_decl decl
%name p_decls decls
%name p_lqtycl lqtycl

%name p_type type
%name p_types types
%name p_types0 types0
%name p_lconstr lconstr

%name p_pat pat
%name p_pats pats
%name p_pats0 pats0
%name p_pats1 pats1

%name p_expr expr
%name p_exprs exprs
%name p_hlist hlist
%name p_match match
%name p_guards0 guards0
%name p_guards1 guards1
%name p_guard guard
%name p_lbinds0 lbinds0
%name p_app app

%name p_stmt stmt
%name p_stmt1 stmt1

%name p_symbols1 symbols1

%tokentype { Code }
%monad { Builder }
%lexer { formLexer } { L _ TEnd }

%token

'case'     { L _ (Atom (ASymbol "case")) }
'data'     { L _ (Atom (ASymbol "data")) }
'do'       { L _ (Atom (ASymbol "do")) }
'if'       { L _ (Atom (ASymbol "if")) }
'import'   { L _ (Atom (ASymbol "import")) }
'instance' { L _ (Atom (ASymbol "instance")) }
'let'      { L _ (Atom (ASymbol "let")) }
'module'   { L _ (Atom (ASymbol "module")) }
'newtype'  { L _ (Atom (ASymbol "newtype")) }
'type'     { L _ (Atom (ASymbol "type")) }

','  { L _ (Atom (ASymbol ",")) }
'->' { L _ (Atom (ASymbol "->")) }
'::' { L _ (Atom (ASymbol "::")) }
'<-' { L _ (Atom (ASymbol "<-")) }
'='  { L _ (Atom (ASymbol "=")) }
'=>' { L _ (Atom (ASymbol "=>")) }
'\\' { L _ (Atom (ASymbol "\\")) }
'{'  { L _ (Atom (ASymbol "{")) }
'|'  { L _ (Atom (ASymbol "|")) }
'}'  { L _ (Atom (ASymbol "}")) }

-- For `as' pattern
-- '@'  { L _ (Atom (ASymbol "@")) }

-- For `irrefutable' pattern
-- '~'  { L _ (Atom (ASymbol "~")) }

'symbol'  { L _ (Atom (ASymbol _)) }
'char'    { L _ (Atom (AChar _)) }
'string'  { L _ (Atom (AString _)) }
'integer' { L _ (Atom (AInteger _)) }
'frac'    { L _ (Atom (AFractional _)) }
'comment' { L _ (Atom (AComment _)) }
'unit'    { L _ (Atom AUnit) }

'f_import'
    { L _ (List $$@((L _ (Atom (ASymbol "import"))):_)) }
'f_module'
    { L _ (List $$@((L _ (Atom (ASymbol "module"))):_)) }
'deriving'
    { L _ (List [L _ (Atom (ASymbol "deriving")), L _ (List $$)])}

'list'       { L _ (List $$) }
'hslist'     { L _ (HsList _) }


%%

-- ---------------------------------------------------------------------
--
-- Documentation
--
-- ---------------------------------------------------------------------

mbdoc :: { Maybe LHsDocString }
    : {- empty -} { Nothing }
    | 'comment' { Just (b_commentStringE $1) }


-- ---------------------------------------------------------------------
--
-- Module
--
-- ---------------------------------------------------------------------

module :: { HsModule RdrName }
    : mhead imports top_decls {% $1 `fmap` pure $2 <*> pure $3 }
    | mhead imports           {% $1 `fmap` pure $2 <*> pure [] }
    | mhead top_decls         {% $1 `fmap` pure [] <*> pure $2 }

mhead :: { [HImportDecl] -> [HDecl] -> HsModule RdrName }
    : {- empty -}      { b_implicitMainModule }
    | mbdoc 'f_module' {% parse p_mod_header $2 <*> pure $1 }

mod_header :: { Maybe LHsDocString -> [HImportDecl] -> [HDecl]
                -> HsModule RdrName }
    : 'module' 'symbol' { b_module $2 }

imports :: { [HImportDecl] }
    : rimports { reverse $1 }

rimports :: { [HImportDecl] }
    : import_form          { [$1] }
    | rimports import_form { $2 : $1 }

import_form :: { HImportDecl }
    : 'f_import' {% parse p_import $1 }

import :: { HImportDecl }
    : 'import' 'symbol' { b_importD $2 }


-- ---------------------------------------------------------------------
--
-- Declarations
--
-- ---------------------------------------------------------------------

top_decls :: { [HDecl] }
    : rtop_decls { reverse $1 }

rtop_decls :: { [HDecl] }
    : top_decl_with_doc            { [$1] }
    | rtop_decls top_decl_with_doc { $2 : $1 }

top_decl_with_doc :: { HDecl }
    : mbdoc 'list' {% parse p_top_decl $2 }

top_decl :: { HDecl }
    : 'data' simpletype constrs    { b_dataD $1 $2 $3 }
    | 'type' simpletype type       { b_typeD $1 $2 $3 }
    | 'newtype' simpletype constrs { b_newtypeD $1 $2 $3 }
    | 'instance' qtycl idecls      { b_instD $2 $3 }
    | decl                         { $1 }

simpletype :: { (String, [HTyVarBndr])}
    : 'symbol' { b_simpletypeD [$1] }
    | 'list'   { b_simpletypeD $1 }

constrs :: { (HsDeriving RdrName, [HConDecl]) }
    : rconstrs { let (m,d) = $1 in (m, reverse d) }

rconstrs :: { (HsDeriving RdrName, [HConDecl]) }
    : {- empty -}            { (Nothing, []) }
    | rconstrs deriving      { b_derivD $1 $2 }
    | rconstrs constr        { fmap ($2:) $1 }

constr :: { HConDecl }
    : 'symbol' { b_conOnlyD $1 }
    | 'list'   {% parse p_lconstr $1 }

deriving :: { [HType] }
    : 'deriving' {% parse p_types $1 }

lconstr :: { HConDecl }
    : 'symbol' condetails         { b_conD $1 $2 }
    | 'symbol' '{' fielddecls '}' { b_conD $1 $3 }

condetails :: { HsConDeclDetails RdrName }
    : types { b_conDeclDetails $1 }

fielddecls :: { HsConDeclDetails RdrName }
    : fielddecls1 { b_recFieldsD $1 }

fielddecls1 :: { [HConDeclField] }
    : rfielddecls { reverse $1 }

rfielddecls :: { [HConDeclField] }
    : fielddecl             { [$1] }
    | rfielddecls fielddecl { $2:$1 }

fielddecl :: { HConDeclField }
    : 'symbol' type { b_recFieldD [$1] $2 }
    | symbols  type { b_recFieldD $1 $2 }

qtycl :: { ([HType], HType) }
    : 'list' {% parse p_lqtycl $1 }

lqtycl :: { ([HType], HType) }
    : '=>' 'unit' type  { ([], $3) }
    | '=>' 'list' types {% b_qtyclC . (:$3) =<< parse p_types0 $2 }
    | types0            { ([], $1) }

idecls :: { [HDecl] }
    : decls { $1 }

decl :: { HDecl }
    : '=' 'symbol' aguards { b_funBindD $2 $3 }
    | '=' 'list' guards    {% b_patBindD $3 `fmap` parse p_pats1 $2 }
    | '=' 'hslist' guards
      {% do { lp <- parse p_pats0 (unwrapListL $2)
            ; return (b_patBindD $3 (b_hsListP lp)) }}
    | '::' 'symbol' dtype { b_tsigD [$2] $3 }
    | '::' symbols dtype  { b_tsigD $2 $3 }

aguards :: { ([HGRHS], [HPat]) }
        : guards      { ($1, []) }
        | pat aguards { ($1:) `fmap` $2 }

dtype :: { ([HType], HType) }
    : 'symbol' { ([], b_symT $1) }
    | 'unit'   { ([], b_unitT $1) }
    | 'hslist'
      {% do { t <- parse p_type [toListL $1]
            ; return ([], b_listT t) }}
    | qtycl { $1 }

decls :: { [HDecl] }
   : rdecls { reverse $1 }

rdecls :: { [HDecl] }
   : {- empty -}   { [] }
   | rdecls 'list' {% (:$1) `fmap` parse p_decl $2 }


-- ---------------------------------------------------------------------
--
-- Type
--
-- ---------------------------------------------------------------------

type :: { HType }
    : 'symbol' { b_symT $1 }
    | 'unit'   { b_unitT $1 }
    | 'hslist' {% b_listT `fmap` parse p_type [toListL $1] }
    | 'list'   {% parse p_types0 $1 }

types0 :: { HType }
    : '->' types {% b_funT $2 }
    | ',' types  { b_tupT $1 $2 }
    | types      { b_appT $1 }

types :: { [HType] }
    : rtypes { reverse $1 }

rtypes :: { [HType] }
    : type        { [$1] }
    | rtypes type { $2 : $1 }


-- ---------------------------------------------------------------------
--
-- Patterns
--
-- ---------------------------------------------------------------------

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
    | 'string'  { b_stringP $1 }
    | 'char'    { b_charP $1 }
    | 'symbol'  { b_symP $1 }
    | 'hslist'  {% b_hsListP `fmap` parse p_pats0 (unwrapListL $1) }
    | 'list'    {% parse p_pats1 $1 }

pats1 :: { HPat }
    : ',' pats0      { b_tupP $1 $2 }
    | 'symbol' pats0 {% b_conP $1 $2 }


-- ---------------------------------------------------------------------
--
-- Expressions
--
-- ---------------------------------------------------------------------

expr :: { HExpr }
    : atom     { $1 }
    | 'list'   {% parse p_exprs $1 }

atom :: { HExpr }
    : 'symbol'  { b_varE $1 }
    | 'char'    { b_charE $1 }
    | 'string'  { b_stringE $1 }
    | 'integer' { b_integerE $1 }
    | 'frac'    { b_floatE $1 }
    | 'unit'    { b_unitE $1 }
    | 'hslist'  {% b_hsListE `fmap` parse p_hlist (unwrapListL $1) }

exprs :: { HExpr }
    : '\\' lambda             { b_lamE $2 }
    | ',' app                 { b_tupE $1 $2 }
    | 'let' lbinds expr       { b_letE $1 $2 $3 }
    | 'if' expr expr expr     { b_ifE $1 $2 $3 $4 }
    | 'case' expr alts        { b_caseE $1 $2 $3 }
    | 'do' do_stmts           { b_doE $1 $2 }
    | '::' expr type          { b_tsigE $1 $2 $3 }
    | 'symbol' '{' fbinds '}' { b_recConOrUpdE $1 $3 }
    | 'list' '{' fbinds '}'   {% b_recUpdE (parse p_exprs $1) $3 }
    | app                     { b_appE $1 }

lambda :: { (HExpr,[HPat]) }
     : expr       { ($1,[]) }
     | pat lambda { fmap ($1:) $2 }

lbinds :: { [HDecl] }
    : 'unit' { [] }
    | 'list' {% parse p_lbinds0 $1 }

lbinds0 :: { [HDecl] }
    : rlbinds0 { reverse $1 }

rlbinds0 :: { [HDecl] }
    : 'list'          {% fmap (:[]) (parse p_decl $1) }
    | rlbinds0 'list' {% fmap (:$1) (parse p_decl $2) }

fbinds :: { [(String, HExpr)] }
    : rfbinds { reverse $1 }

rfbinds :: { [(String, HExpr)] }
    : {- empty -}           { [] }
    | rfbinds 'symbol' expr { (symbolNameL $2, $3):$1 }

app :: { [HExpr] }
    : rapp { reverse $1 }

rapp :: { [HExpr] }
    : expr      { [$1] }
    | rapp expr { $2 : $1 }

alts :: { [HMatch] }
    : ralts { reverse $1 }

ralts :: { [HMatch] }
    : pat guards       { [b_match $1 $2] }
    | ralts pat guards { b_match $2 $3 : $1 }

match :: { HMatch }
    : pat guards { b_match $1 $2 }

hlist :: { [HExpr] }
    : rhlist { reverse $1 }

rhlist :: { [HExpr] }
    : {- empty -} { [] }
    | rhlist expr { $2:$1 }

-- Parsing form for guards
-- ~~~~~~~~~~~~~~~~~~~~~~~
--
-- Separating the rule for 'list' and atom, so that the 'guards0' rule
-- can try matching the symbol '|' before 'expr' rule, to differentiate
-- the entire form from function application of reserved symbol '|'.

guards :: { [HGRHS] }
    : 'list' {% parse p_guards0 $1 }
    | atom   { [L (getLoc $1) (GRHS [] $1)] }

guards0 :: { [HGRHS] }
    : '|' guards1 { $2 }
    | exprs       { [L (getLoc $1) (GRHS [] $1)] }

guards1 :: { [HGRHS] }
    : 'list'         {% b_hgrhs [] `fmap` parse p_guard $1 }
    | 'list' guards1 {% b_hgrhs $2 `fmap` parse p_guard $1 }

guard :: { (HExpr, [GuardLStmt RdrName]) }
    : expr       { ($1, []) }
    | stmt guard { fmap ($1:) $2 }


-- ---------------------------------------------------------------------
--
-- Do statement
--
-- ---------------------------------------------------------------------

do_stmts :: { [HStmt] }
    : rdo_stmts { reverse $1 }

rdo_stmts :: { [HStmt] }
    : stmt           { [$1] }
    | rdo_stmts stmt { $2 : $1 }

stmt :: { HStmt }
    : atom     { b_bodyS $1 }
    | 'list'   {% parse p_stmt1 $1 }

stmt1 :: { HStmt }
    : '<-' pat expr { b_bindS $1 $2 $3 }
    | 'let' lbinds  { b_letS $1 $2 }
    | exprs         { b_bodyS $1 }


-- ---------------------------------------------------------------------
--
-- Auxiliary
--
-- ---------------------------------------------------------------------

symbols :: { [Code] }
    : 'list' {% parse p_symbols1 $1 }

symbols1 :: { [Code] }
    : rsymbols { reverse $1 }

rsymbols :: { [Code] }
    : 'symbol'          { [$1] }
    | rsymbols 'symbol' { $2:$1 }


{
happyError :: Builder a
happyError = builderError

parseModule :: Builder (HsModule RdrName)
parseModule = parse_module

parseImports :: Builder [HImportDecl]
parseImports = p_imports

parseStmt :: Builder HStmt
parseStmt = p_stmt

parseDecls :: Builder [HDecl]
parseDecls = p_decls

parseExpr :: Builder HExpr
parseExpr = p_expr
}
