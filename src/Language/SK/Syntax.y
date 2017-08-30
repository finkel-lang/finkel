-- Happy parser for S-expression forms.
{
{-# LANGUAGE OverloadedStrings #-}
-- | Module for parsing form data.
--
-- Unlike the lexer for reading source code, parser defined in this
-- module expects list of 'Form' data as input, converts to Haskell
-- AST defined in GHC.
--
module Language.SK.Syntax
  ( Builder(..)
  , runBuilder
  , evalBuilder
  , parseModule
  , parseImports
  , parseStmt
  , parseDecls
  , parseTopDecls
  , parseExpr
  ) where

import Language.SK.Builder
import Language.SK.Form
import Language.SK.GHC
}

%name parse_module module
%name p_mod_header mod_header

%name p_entity entity
%name p_entities entities

%name p_imports imports
%name p_import import
%name p_limport limport
%name p_impdecl0 impdecl0

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
%name p_where where
%name p_lbinds0 lbinds0
%name p_app app

%name p_stmt stmt
%name p_stmt1 stmt1

%name p_symbols1 symbols1

%tokentype { Code }
%monad { Builder }
%lexer { formLexer } { LForm (L _ TEnd) }

%token

-- Haskell 2010
'as'        { LForm (L _ (Atom (ASymbol "as"))) }
'case'      { LForm (L _ (Atom (ASymbol "case"))) }
'class'     { LForm (L _ (Atom (ASymbol "class"))) }
'data'      { LForm (L _ (Atom (ASymbol "data"))) }
'default'   { LForm (L _ (Atom (ASymbol "default"))) }
'do'        { LForm (L _ (Atom (ASymbol "do"))) }
'hiding'    { LForm (L _ (Atom (ASymbol "hiding"))) }
'if'        { LForm (L _ (Atom (ASymbol "if"))) }
'infix'     { LForm (L _ (Atom (ASymbol "infix"))) }
'infixl'    { LForm (L _ (Atom (ASymbol "infixl"))) }
'infixr'    { LForm (L _ (Atom (ASymbol "infixr"))) }
'instance'  { LForm (L _ (Atom (ASymbol "instance"))) }
'let'       { LForm (L _ (Atom (ASymbol "let"))) }
'newtype'   { LForm (L _ (Atom (ASymbol "newtype"))) }
'qualified' { LForm (L _ (Atom (ASymbol "qualified"))) }
'type'      { LForm (L _ (Atom (ASymbol "type"))) }

'!'  { LForm (L _ (Atom (ASymbol "!"))) }
','  { LForm (L _ (Atom (ASymbol ","))) }
'->' { LForm (L _ (Atom (ASymbol "->"))) }
'..' { LForm (L _ (Atom (ASymbol ".."))) }
'::' { LForm (L _ (Atom (ASymbol "::"))) }
'<-' { LForm (L _ (Atom (ASymbol "<-"))) }
'='  { LForm (L _ (Atom (ASymbol "="))) }
'=>' { LForm (L _ (Atom (ASymbol "=>"))) }
'@'  { LForm (L _ (Atom (ASymbol "@"))) }
'\\' { LForm (L _ (Atom (ASymbol "\\"))) }
'{'  { LForm (L _ (Atom (ASymbol "{"))) }
'|'  { LForm (L _ (Atom (ASymbol "|"))) }
'}'  { LForm (L _ (Atom (ASymbol "}"))) }
'~'  { LForm (L _ (Atom (ASymbol "~"))) }

-- Pragmas
'unpack'    { LForm (L _ (Atom (ASymbol "UNPACK"))) }
'inline'    { LForm (L _ (Atom (ASymbol "INLINE"))) }
'noinline'  { LForm (L _ (Atom (ASymbol "NOINLINE"))) }
'inlinable' { LForm (L _ (Atom (ASymbol "INLINABLE"))) }

'symbol'  { LForm (L _ (Atom (ASymbol _))) }
'char'    { LForm (L _ (Atom (AChar _))) }
'string'  { LForm (L _ (Atom (AString _))) }
'integer' { LForm (L _ (Atom (AInteger _))) }
'frac'    { LForm (L _ (Atom (AFractional _))) }
'comment' { LForm (L _ (Atom (AComment _))) }
'unit'    { LForm (L _ (Atom AUnit)) }

'deriving'
    { LForm (L _ (List [ LForm (L _ (Atom (ASymbol "deriving")))
                       , LForm (L _ (List $$))])) }

'import'
    { LForm (L _ (List ((LForm (L _ (Atom (ASymbol "import")))):$$))) }

'module'
    { LForm (L _ (List ((LForm (L _ (Atom (ASymbol "module")))):$$))) }

'where'
    { LForm (L _ (List ((LForm (L _ (Atom (ASymbol "where")))):$$))) }

'list'       { LForm (L _ (List $$)) }
'hslist'     { LForm (L _ (HsList _)) }


%%

-- ---------------------------------------------------------------------
--
-- Documentation
--
-- ---------------------------------------------------------------------

mbdoc :: { Maybe LHsDocString }
    : {- empty -} { Nothing }
    | 'comment'   { Just (b_commentStringE $1) }


-- ---------------------------------------------------------------------
--
-- Module
--
-- ---------------------------------------------------------------------

module :: { HModule }
    : mhead imports top_decls {% $1 `fmap` pure $2 <*> pure $3 }
    | mhead imports           {% $1 `fmap` pure $2 <*> pure [] }
    | mhead top_decls         {% $1 `fmap` pure [] <*> pure $2 }

mhead :: { [HImportDecl] -> [HDecl] -> HModule }
    : mbdoc          { b_implicitMainModule }
    | mbdoc 'module' {% parse p_mod_header $2 <*> pure $1 }

mod_header :: { Maybe LHsDocString -> [HImportDecl] -> [HDecl]
                -> HModule }
    : 'symbol' exports { b_module $1 $2 }

exports :: { [HIE] }
    : rexports { reverse $1 }

rexports :: { [HIE] }
    : {- empty -}     { [] }
    | rexports export { $2 : $1 }

export :: { HIE }
    : 'symbol' { b_ieSym $1 }
    | 'module' { b_ieMdl $1 }
    | 'list'   {% parse p_entity $1 }

entity :: { HIE }
    : 'symbol'          { b_ieAbs $1 }
    | 'symbol' '..'     { b_ieAll $1 }
    | 'symbol' symbols1 { b_ieWith $1 $2 }

entities :: { [HIE] }
    : rentities { reverse $1 }

rentities :: { [HIE] }
    : {- empty -}        { [] }
    | rentities 'symbol' { b_ieSym $2 : $1 }
    | rentities 'list'   {% fmap (:$1) (parse p_entity $2) }

imports :: { [HImportDecl] }
    : rimports { reverse $1 }

rimports :: { [HImportDecl] }
    : import          { [$1] }
    | rimports import { $2 : $1 }

import :: { HImportDecl }
    : 'import' {% parse p_limport $1 }

limport :: { HImportDecl }
    : impdecl                 { b_importD $1 False Nothing }
    | impdecl 'unit'          { b_importD $1 False (Just []) }
    | impdecl 'list'          {% do { es <- parse p_entities $2
                                    ; let es' = Just es
                                    ; return (b_importD $1 False es') }}
    | impdecl 'hiding' 'list' {% do { es <- parse p_entities $3
                                    ; let es' = Just es
                                    ; return (b_importD $1 True es') }}

impdecl :: { (Code, Bool, Maybe Code ) }
    : 'symbol' { ($1, False, Nothing) }
    | 'list'   {% parse p_impdecl0 $1 }

impdecl0 :: { (Code, Bool, Maybe Code) }
    : 'qualified' 'symbol'               { ($2, True, Nothing) }
    | 'qualified' 'symbol' 'as' 'symbol' { ($2, True, Just $4) }
    | 'symbol'                           { ($1, False, Nothing) }
    | 'symbol' 'as' 'symbol'             { ($1, False, Just $3) }


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
    | 'class' qtycl cdecls         {% b_classD $2 $3 }
    | 'instance' qtycl idecls      { b_instD $2 $3 }
    | 'default' zero_or_more_types { b_defaultD $2 }
    | fixity 'integer' symbols1    { b_fixityD $1 $2 $3 }
    | decl                         { $1 }

simpletype :: { (FastString, [HTyVarBndr])}
    : 'symbol' { b_simpletypeD [$1] }
    | 'list'   { b_simpletypeD $1 }

constrs :: { (HDeriving, [HConDecl]) }
    : rconstrs { let (m,d) = $1 in (m, reverse d) }

rconstrs :: { (HDeriving, [HConDecl]) }
    : {- empty -}            { (noLoc [], []) }
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

condetails :: { HConDeclDetails }
    : dctypes { b_conDeclDetails $1 }

dctypes :: { [HType] }
    : {- empty -}    { [] }
    | dctype dctypes { $1 : $2 }

dctype :: { HType }
    : 'unpack' type { b_unpackT $1 $2 }
    | type          { $1 }

fielddecls :: { HConDeclDetails }
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

cdecls :: { [HDecl] }
    : decls { $1 }

idecls :: { [HDecl] }
    : decls { $1 }

fixity :: { FixityDirection }
    : 'infixl' { InfixL }
    | 'infixr' { InfixR }
    | 'infix'  { InfixN }

decl :: { HDecl }
    : '=' 'symbol' aguards { b_funBindD $2 $3 }
    | '=' 'list' guards    {% b_patBindD $3 `fmap` parse p_pats1 $2 }
    | '=' 'hslist' guards  {% do { lp <- parse p_pats0 (unwrapListL $2)
                                 ; let lp' = b_hsListP lp
                                 ; return (b_patBindD $3 lp') }}
    | '::' 'symbol' dtype  { b_tsigD [$2] $3 }
    | '::' symbols dtype   { b_tsigD $2 $3 }
    | 'inline' 'symbol'    { b_inlineD Inline $2 }
    | 'noinline' 'symbol'  { b_inlineD NoInline $2 }
    | 'inlinable' 'symbol' { b_inlineD Inlinable $2 }

aguards :: { (([HGRHS],[HDecl]), [HPat]) }
        : guards      { ($1, []) }
        | pat aguards { ($1:) `fmap` $2 }

dtype :: { ([HType], HType) }
    : 'symbol' { ([], b_symT $1) }
    | 'unit'   { ([], b_unitT $1) }
    | 'hslist' {% do { t <- parse p_type [toListL $1]
                     ; return ([], b_listT t) }}
    | qtycl    { $1 }

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
    | '!' type   { b_bangT $1 $2 }
    | types      { b_appT $1 }

types :: { [HType] }
    : rtypes { reverse $1 }

rtypes :: { [HType] }
    : type        { [$1] }
    | rtypes type { $2 : $1 }

zero_or_more_types :: { [HType] }
    : {- empty -} { [] }
    | types       { $1 }

qtype :: { ([HType], HType) }
    : 'symbol' { ([],b_symT $1) }
    | 'unit'   { ([],b_unitT $1) }
    | 'hslist' {% do { typ <- parse p_type [toListL $1]
                     ; return ([], b_listT typ) } }
    | qtycl    { $1 }


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
    : ',' pats0             { b_tupP $1 $2 }
    | '@' 'symbol' pat      { b_asP $2 $3 }
    | '~' pat               { b_lazyP $2 }
    | 'symbol' '{' lblp '}' {% b_labeledP $1 $3 }
    | 'symbol' pats0        {% b_conP $1 $2 }

lblp :: { [(Code, HPat)] }
    : rlblp { reverse $1 }

rlblp :: { [(Code, HPat)] }
    : {- empty -}       { [] }
    | rlblp 'symbol' pat { ($2, $3):$1 }


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
    | '::' expr qtype         { b_tsigE $1 $2 $3 }
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

fbinds :: { [(FastString, HExpr)] }
    : rfbinds { reverse $1 }

rfbinds :: { [(FastString, HExpr)] }
    : {- empty -}           { [] }
    | rfbinds 'symbol' expr { (symbolNameFS $2, $3):$1 }

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

hlist :: { Either HExpr [HExpr] }
    : expr expr '..' expr { Left (b_arithSeqE $1 (Just $2) (Just $4)) }
    | expr expr '..'      { Left (b_arithSeqE $1 (Just $2) Nothing) }
    | expr '..' expr      { Left (b_arithSeqE $1 Nothing (Just $3)) }
    | expr '..'           { Left (b_arithSeqE $1 Nothing Nothing) }
    | hlist0              { Right $1 }

hlist0 :: { [HExpr] }
    : {- empty -} { [] }
    | expr hlist0 { $1:$2 }


-- Parsing form for guards
-- ~~~~~~~~~~~~~~~~~~~~~~~
--
-- Separating the rule for 'where', 'list' and atom, so that the
-- 'guards0' rule can try matching the symbol '|' before 'expr' rule, to
-- differentiate the entire form from function application of reserved
-- symbol '|'.

guards :: { ([HGRHS],[HDecl]) }
    : 'where' {% parse p_where $1 }
    | 'list'  {% parse p_guards0 $1 >>= \gs -> return (gs,[]) }
    | atom    { ([L (getLoc $1) (GRHS [] $1)], []) }

guards0 :: { [HGRHS] }
    : '|' guards1 { $2 }
    | exprs       { [L (getLoc $1) (GRHS [] $1)] }

guards1 :: { [HGRHS] }
    : 'list'         {% b_hgrhs [] `fmap` parse p_guard $1 }
    | 'list' guards1 {% b_hgrhs $2 `fmap` parse p_guard $1 }

guard :: { (HExpr, [HGuardLStmt]) }
    : expr       { ($1, []) }
    | stmt guard { fmap ($1:) $2 }

where :: { [HGRHS],[HDecl] }
    : 'list' lbinds0 {% parse p_guards0 $1 >>= \gs -> return (gs,$2) }
    | atom lbinds0   { ([L (getLoc $1) (GRHS [] $1)], $2) }


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

parseModule :: Builder HModule
parseModule = parse_module

parseImports :: Builder [HImportDecl]
parseImports = p_imports

parseStmt :: Builder HStmt
parseStmt = p_stmt

parseDecls :: Builder [HDecl]
parseDecls = p_decls

parseTopDecls :: Builder [HDecl]
parseTopDecls = p_top_decls

parseExpr :: Builder HExpr
parseExpr = p_expr
}
