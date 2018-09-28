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
  , parseLImport
  , parseStmt
  , parseDecls
  , parseTopDecls
  , parseExpr
  , parseType
  ) where

-- ghc
import BasicTypes (FixityDirection(..), InlineSpec(..))
import FastString (FastString)
import ForeignCall (Safety)
import HsDoc (LHsDocString)
import HsExpr (GRHS(..))
import SrcLoc (Located, noLoc)

-- Internal
import Language.SK.Builder
import Language.SK.Form
import Language.SK.Syntax.Internal
}

%name parse_module module
%name p_mod_header mod_header

%name p_entity entity
%name p_entities entities

%name p_imports imports
%name p_import import
%name p_limport limport

%name p_top_decls top_decls
%name p_top_decl top_decl
%name p_decl decl
%name p_decls decls
%name p_lqtycl lqtycl
%name p_sfsig sfsig
%name p_lsname lsname

%name p_type type
%name p_types types
%name p_types0 types0
%name p_lconstr lconstr
%name p_lqtycon lqtycon
%name p_lh98constr lh98constr

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

%name p_varids1 varids1
%name p_symbols1 symbols1

%tokentype { Code }
%monad { Builder }
%lexer { formLexer } { LForm (L _ TEnd) }

%token

-- Haskell 2010 reserved ids
'case'     { LForm (L _ (Atom (ASymbol "case"))) }
'class'    { LForm (L _ (Atom (ASymbol "class"))) }
'data'     { LForm (L _ (Atom (ASymbol "data"))) }
'default'  { LForm (L _ (Atom (ASymbol "default"))) }
'deriving' { LForm (L _ (List [LForm (L _ (Atom (ASymbol "deriving")))
                              ,LForm (L _ (List $$))])) }
'do'       { LForm (L _ (Atom (ASymbol "do"))) }
'foreign'  { LForm (L _ (Atom (ASymbol "foreign"))) }
'if'       { LForm (L _ (Atom (ASymbol "if"))) }
'import'   { LForm (L _ (List ((LForm (L _ (Atom (ASymbol "import"))))
                               :$$))) }
'infix'    { LForm (L _ (Atom (ASymbol "infix"))) }
'infixl'   { LForm (L _ (Atom (ASymbol "infixl"))) }
'infixr'   { LForm (L _ (Atom (ASymbol "infixr"))) }
'instance' { LForm (L _ (Atom (ASymbol "instance"))) }
'let'      { LForm (L _ (Atom (ASymbol "let"))) }
'module'   { LForm (L _ (List ((LForm (L _ (Atom (ASymbol "module"))))
                               :$$))) }
'newtype'  { LForm (L _ (Atom (ASymbol "newtype"))) }
'type'     { LForm (L _ (Atom (ASymbol "type"))) }
'where'    { LForm (L _ (List ((LForm (L _ (Atom (ASymbol "where"))))
                               :$$))) }

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

-- Non Haskell 2010 reserved id, but treated specially
'as'        { LForm (L _ (Atom (ASymbol "as"))) }
'hiding'    { LForm (L _ (Atom (ASymbol "hiding"))) }
'qualified' { LForm (L _ (Atom (ASymbol "qualified"))) }

-- GHC Extensions
'forall' { LForm (L _ (Atom (ASymbol "forall"))) }

-- Pragmas
'inlinable'  { LForm (L _ (Atom (ASymbol "INLINABLE"))) }
'inline'     { LForm (L _ (Atom (ASymbol "INLINE"))) }
'noinline'   { LForm (L _ (Atom (ASymbol "NOINLINE"))) }
'specialize' { LForm (L _ (Atom (ASymbol "SPECIALIZE"))) }
'unpack'     { LForm (L _ (List [LForm
                                 (L _ (Atom (ASymbol "UNPACK")))])) }

'symbol'  { LForm (L _ (Atom (ASymbol _))) }
'char'    { LForm (L _ (Atom (AChar _))) }
'string'  { LForm (L _ (Atom (AString _))) }
'integer' { LForm (L _ (Atom (AInteger _))) }
'frac'    { LForm (L _ (Atom (AFractional _))) }
'comment' { LForm (L _ (Atom (AComment _))) }
'unit'    { LForm (L _ (Atom AUnit)) }

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
    : varid    { b_ieSym $1 }
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
    | rentities varid  { b_ieSym $2 : $1 }
    | rentities 'list' {% fmap (:$1) (parse p_entity $2) }

imports :: { [HImportDecl] }
    : rimports { reverse $1 }

rimports :: { [HImportDecl] }
    : import          { [$1] }
    | rimports import { $2 : $1 }

import :: { HImportDecl }
    : 'import' {% parse p_limport $1 }

limport :: { HImportDecl }
    : 'qualified' 'symbol' 'as' 'symbol' impspec
      { b_importD ($2, True, Just $4) $5 }
    | 'qualified' 'symbol' impspec
      { b_importD ($2, True, Nothing) $3 }
    | 'symbol' 'as' 'symbol' impspec
      { b_importD ($1, False, Just $3) $4 }
    | 'symbol' impspec
      { b_importD ($1, False, Nothing) $2 }

impspec :: { (Bool, Maybe [HIE]) }
    : 'hiding' 'list' {% do { es <- parse p_entities $2
                            ; return (True, Just es) } }
    | 'list'          {% do { es <- parse p_entities $1
                            ; return (False, Just es) } }
    | 'unit'          { (False, Just []) }
    | {- empty -}     { (False, Nothing) }


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
    | 'foreign' 'symbol' ccnv sname 'list'
      {% parse p_sfsig $5 >>= b_ffiD $1 $2 $3 $4 }
    | decl                         { $1 }

sfsig :: { (Code, HType) }
    : '::' 'symbol' type { ($2, $3) }

simpletype :: { (FastString, [HTyVarBndr])}
    : 'symbol' { b_simpletypeD [$1] }
    | 'list'   { b_simpletypeD $1 }

constrs :: { (HDeriving, [HConDecl]) }
    : rconstrs { let (m,d) = $1 in (m,reverse d) }

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
    : forall qtycon       { b_forallD (snd $1) $2 }
    | '::' 'symbol' qtype { b_gadtD $2 $3 }
    | lh98constr          { $1 }

forall :: { (Code, [Code]) }
    : 'forall' lforall { ($1, $2) }

lforall :: { [Code] }
    : 'symbol' { [$1] }
    | 'symbol' lforall { $1:$2 }

qtycon :: { (HConDecl, [HType]) }
    : 'list' {% parse p_lqtycon $1 }

lqtycon :: { (HConDecl, [HType]) }
    : '=>' 'unit' lh98constr   { ($3, []) }
    | '=>' type tys_h98constr  { let (c,ts) = $3 in (c,$2:reverse ts) }
    | lh98constr               { ($1, []) }

tys_h98constr :: { (HConDecl, [HType]) }
    : h98constr          { ($1, []) }
    | type tys_h98constr { let (c,ts) = $2 in (c,$1:ts) }

h98constr :: { HConDecl }
    : 'list' {% parse p_lh98constr $1 }

lh98constr :: { HConDecl }
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
    : 'symbol' dctype { b_recFieldD [$1] $2 }
    | symbols  dctype { b_recFieldD $1 $2 }

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

ccnv :: { HCCallConv }
    : 'symbol' {% b_callConv $1 }

sname :: { (Maybe (Located Safety), Code) }
    : 'string' { (Nothing, $1) }
    | 'list'   {% parse p_lsname $1 }

lsname :: { (Maybe (Located Safety), Code) }
    : 'symbol' 'string' {% b_safety $1 >>= \s -> return (Just s, $2) }

decl :: { HDecl }
    : '=' varid aguards    { b_funBindD $2 $3 }
    | '=' 'list' guards    {% b_patBindD $3 `fmap` parse p_pats1 $2 }
    | '=' 'hslist' guards  {% do { lp <- parse p_pats0 (unwrapListL $2)
                                 ; let lp' = b_hsListP lp
                                 ; return (b_patBindD $3 lp') }}
    | '::' varid dtype     { b_tsigD [$2] $3 }
    | '::' varids dtype    { b_tsigD $2 $3 }
    | 'inline' varid       { b_inlineD Inline $2 }
    | 'noinline' varid     { b_inlineD NoInline $2 }
    | 'inlinable' varid    { b_inlineD Inlinable $2 }
    | 'specialize' 'list'  {% parse p_sfsig $2 >>= b_specializeD $1 }

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
    : varid    { b_symT $1 }
    | 'unit'   { b_unitT $1 }
    | 'hslist' {% case toListL $1 of
                    LForm (L _ (List [])) -> return (b_nilT $1)
                    xs -> b_listT `fmap` parse p_type [xs] }
    | 'list'   {% parse p_types0 $1 }

types0 :: { HType }
    : '->' types   {% b_funT $2 }
    | ',' types    { b_tupT $1 $2 }
    | '!' type     { b_bangT $1 $2 }
    | forall qtycl { b_forallT $1 $2 }
    | types        { b_appT $1 }

types :: { [HType] }
    : rtypes { reverse $1 }

rtypes :: { [HType] }
    : type            { [$1] }
    | rtypes type     { $2 : $1 }

zero_or_more_types :: { [HType] }
    : {- empty -} { [] }
    | types       { $1 }

qtype :: { ([HType], HType) }
    : 'symbol' { ([], b_symT $1) }
    | 'unit'   { ([], b_unitT $1) }
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
    | 'unit'    { b_unitP $1 }
    | varid     { b_symP $1 }
    | 'hslist'  {% b_hsListP `fmap` parse p_pats0 (unwrapListL $1) }
    | 'list'    {% parse p_pats1 $1 }

pats1 :: { HPat }
    : ',' pats0             { b_tupP $1 $2 }
    | '@' varid pat         { b_asP $2 $3 }
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
    : varid     { b_varE $1 }
    | 'char'    { b_charE $1 }
    | 'string'  { b_stringE $1 }
    | 'integer' { b_integerE $1 }
    | 'frac'    { b_floatE $1 }
    | 'unit'    { b_unitE $1 }
    | 'hslist'  {% b_hsListE `fmap` parse p_hlist (unwrapListL $1) }

varid :: { Code }
    : 'symbol'   { $1 }
    | special_id { $1 }

special_id :: { Code }
    : '!'         { $1 }
    | 'as'        { $1 }
    | 'forall'    { $1 }
    | 'hiding'    { $1 }
    | 'qualified' { $1 }

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

varids :: { [Code] }
    : 'list' {% parse p_varids1 $1 }

varids1 :: { [Code] }
    : rvarids { reverse $1 }

rvarids :: { [Code] }
    : varid         { [$1] }
    | rvarids varid { $2 : $1 }

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

parseLImport :: Builder HImportDecl
parseLImport = p_limport

parseStmt :: Builder HStmt
parseStmt = p_stmt

parseDecls :: Builder [HDecl]
parseDecls = p_decls

parseTopDecls :: Builder [HDecl]
parseTopDecls = p_top_decls

parseExpr :: Builder HExpr
parseExpr = p_expr

parseType :: Builder HType
parseType = p_type

-- | Unwrap the element of 'List' and 'HsList', otherwise returns '[]'.
unwrapListL :: Code -> [Code]
unwrapListL (LForm (L _ form)) =
    case form of
      List xs -> xs
      HsList xs -> xs
      _ -> []
}
