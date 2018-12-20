{
{-# LANGUAGE OverloadedStrings #-}
-- | Module for parsing form data.
--
-- This module contains Happy parser for S-expression forms. Unlike the
-- lexer for reading source code, parser defined in this module takes a
-- list of 'Code' data as input, and converts to internal abstract
-- syntax tree data defined in GHC.
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
import BasicTypes (FixityDirection(..), InlineSpec(..), OverlapMode(..))
import FastString (FastString)
import ForeignCall (Safety)
import HaddockUtils (addConDoc)
import HsDoc (LHsDocString)
import HsExpr (GRHS(..))
import SrcLoc (Located, noLoc)

-- Internal
import Language.SK.Builder
import Language.SK.Form
import Language.SK.Syntax.HDecl
import Language.SK.Syntax.HExpr
import Language.SK.Syntax.HIE
import Language.SK.Syntax.HPat
import Language.SK.Syntax.HType
import Language.SK.Syntax.SynUtils
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

%name p_idsyms1 idsyms1

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
'overlappable' { LForm
                 (L _ (List [LForm
                              (L _ (Atom (ASymbol "OVERLAPPABLE")))])) }
'overlapping' { LForm
                (L _ (List [LForm
                             (L _ (Atom (ASymbol "OVERLAPPING")))])) }
'overlaps' { LForm
             (L _ (List [LForm
                          (L _ (Atom (ASymbol "OVERLAPS")))])) }
'incoherent' { LForm
               (L _ (List [LForm
                            (L _ (Atom (ASymbol "INCOHERENT")))])) }

-- Documentation
'docn' { LForm (L _ (List [LForm (L _ (Atom (ASymbol ":docn"))), $$])) }
'docp' { LForm (L _ (List [LForm (L _ (Atom (ASymbol ":docp"))), $$])) }

-- Plain constructors
'symbol'  { LForm (L _ (Atom (ASymbol _))) }
'char'    { LForm (L _ (Atom (AChar _))) }
'string'  { LForm (L _ (Atom (AString _))) }
'integer' { LForm (L _ (Atom (AInteger _))) }
'frac'    { LForm (L _ (Atom (AFractional _))) }
'unit'    { LForm (L _ (Atom AUnit)) }
'list'    { LForm (L _ (List $$)) }
'hslist'  { LForm (L _ (HsList _)) }


%%

-- ---------------------------------------------------------------------
--
-- Documentation
--
-- ---------------------------------------------------------------------

mbdocnext :: { Maybe LHsDocString }
    : 'docn'      {% fmap Just (b_docnextE $1) }
    | {- empty -} { Nothing }

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
    : mbdocnext          {% b_implicitMainModule }
    | mbdocnext 'module' {% parse p_mod_header $2 <*> pure $1 }

mod_header :: { Maybe LHsDocString -> [HImportDecl] -> [HDecl]
                -> HModule }
    : 'symbol' exports {% b_module $1 $2 }

exports :: { [HIE] }
    : rexports { reverse $1 }

rexports :: { [HIE] }
    : {- empty -}     { [] }
    | rexports export { $2 : $1 }

export :: { HIE }
    : idsym    {% b_ieSym $1 }
    | 'module' {% b_ieMdl $1 }
    | 'list'   {% parse p_entity $1 }

entity :: { HIE }
    : conid         {% b_ieAbs $1 }
    | conid '..'    {% b_ieAll $1 }
    | conid idsyms1 {% b_ieWith $1 $2 }

entities :: { [HIE] }
    : rentities { reverse $1 }

rentities :: { [HIE] }
    : {- empty -}      { [] }
    | rentities idsym  {% b_ieSym $2 >>= \es -> return (es:$1) }
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
      {% b_importD ($2, True, Just $4) $5 }
    | 'qualified' 'symbol' impspec
      {% b_importD ($2, True, Nothing) $3 }
    | 'symbol' 'as' 'symbol' impspec
      {% b_importD ($1, False, Just $3) $4 }
    | 'symbol' impspec
      {% b_importD ($1, False, Nothing) $2 }

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
    : 'list'    {% parse p_top_decl $1 }
    | 'docn'    {% b_docnextD $1 }
    | 'docp'    {% b_docprevD $1 }

top_decl :: { HDecl }
    : 'data' simpletype constrs            { b_dataD $1 $2 $3 }
    | 'type' simpletype type               { b_typeD $1 $2 $3 }
    | 'newtype' simpletype constrs         { b_newtypeD $1 $2 $3 }
    | 'class' qtycl cdecls                 {% b_classD $2 $3 }
    | 'instance' overlap qtycl idecls      { b_instD $2 $3 $4 }
    | 'default' zero_or_more_types         { b_defaultD $2 }
    | fixity 'integer' idsyms1             {% b_fixityD $1 $2 $3 }
    | 'foreign' 'symbol' ccnv sname 'list' {% parse p_sfsig $5 >>=
                                              b_ffiD $1 $2 $3 $4 }
    | decl                                 { $1 }

overlap :: { Maybe (Located OverlapMode) }
    : 'overlappable' { b_overlapP $1 }
    | 'overlapping'  { b_overlapP $1 }
    | 'overlaps'     { b_overlapP $1 }
    | 'incoherent'   { b_overlapP $1 }
    | {- empty -}    { Nothing }

sfsig :: { (Code, HType) }
    : '::' idsym type { ($2, $3) }

simpletype :: { (FastString, [HTyVarBndr])}
    : conid  {% b_simpletypeD [$1] }
    | 'list' {% b_simpletypeD $1 }

constrs :: { (HDeriving, [HConDecl]) }
    : rconstrs { let (m,d) = $1 in (m,reverse d) }

rconstrs :: { (HDeriving, [HConDecl]) }
    : {- empty -}            { (noLoc [], []) }
    | rconstrs deriving      { b_derivD $1 $2 }
    | rconstrs constr        { fmap ($2:) $1 }

constr :: { HConDecl }
    : mbdocnext conid  {% fmap (flip addConDoc $1) (b_conOnlyD $2) }
    | mbdocnext 'list' {% fmap (flip addConDoc $1) (parse p_lconstr $2) }

deriving :: { [HType] }
    : 'deriving' {% parse p_types $1 }

lconstr :: { HConDecl }
    : forall qtycon    { b_forallD (snd $1) $2 }
    | '::' conid dtype {% b_gadtD $2 $3 }
    | lh98constr       { $1 }

forall :: { (Code, [Code]) }
    : 'forall' lforall { ($1, $2) }

lforall :: { [Code] }
    : idsym         { [$1] }
    | idsym lforall { $1:$2 }

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
    : conid condetails         {% b_conD $1 $2 }
    | conid '{' fielddecls '}' {% b_conD $1 $3 }

condetails :: { HConDeclDetails }
    : zero_or_more_types { b_conDeclDetails $1 }

fielddecls :: { HConDeclDetails }
    : fielddecls1 { b_recFieldsD $1 }

fielddecls1 :: { [HConDeclField] }
    : rfielddecls { reverse $1 }

rfielddecls :: { [HConDeclField] }
    : fielddecl             { [$1] }
    | rfielddecls fielddecl { $2:$1 }

fielddecl :: { HConDeclField }
    : idsym  type {% b_recFieldD [$1] $2 }
    | idsyms type {% b_recFieldD $1 $2 }

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
    : 'symbol' 'string' {% (\s -> (Just s, $2)) `fmap` b_safety $1 }

decl :: { HDecl }
    : '=' idsym aguards     {% b_funBindD $2 $3 }
    | '=' pat guards        { b_patBindD $3 $2 }
    | '::' idsym dtype      {% b_tsigD [$2] $3 }
    | '::' idsyms dtype     {% b_tsigD $2 $3 }
    | 'inline' idsym        {% b_inlineD Inline $2 }
    | 'noinline' idsym      {% b_inlineD NoInline $2 }
    | 'inlinable' idsym     {% b_inlineD Inlinable $2 }
    | 'specialize' 'list'   {% parse p_sfsig $2 >>= b_specializeD $1 }

aguards :: { (([HGRHS],[HDecl]), [HPat]) }
    : guards      { ($1, []) }
    | pat aguards { ($1:) `fmap` $2 }

dtype :: { ([HType], HType) }
    : 'symbol' {% (\t -> ([], t)) `fmap` b_symT $1}
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
    : 'unpack' type { b_unpackT $1 $2 }
    | '!' type      { b_bangT $1 $2 }
    | idsym         {% b_symT $1 }
    | 'unit'        { b_unitT $1 }
    | 'hslist'      {% case toListL $1 of
                         LForm (L _ (List [])) -> return (b_nilT $1)
                         xs -> b_listT `fmap` parse p_type [xs] }
    | 'list'        {% parse p_types0 $1 }

types0 :: { HType }
    : '->' types             {% b_funT $2 }
    | ',' zero_or_more_types { b_tupT $1 $2 }
    | forall qtycl           { b_forallT $1 $2 }
    | types                  {% b_appT $1 }

types :: { [HType] }
    : rtypes { reverse $1 }

rtypes :: { [HType] }
    : type            { [$1] }
    | rtypes type     { $2 : $1 }

zero_or_more_types :: { [HType] }
    : {- empty -} { [] }
    | types       { $1 }


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
    : '~' pat_ { b_lazyP $2 }
    | pat_     { $1 }

pat_ :: { HPat }
    : 'integer' {% b_intP $1 }
    | 'string'  {% b_stringP $1 }
    | 'char'    {% b_charP $1 }
    | 'unit'    {% b_unitP $1 }
    | idsym     {% b_symP $1 }
    | 'hslist'  {% b_hsListP `fmap` parse p_pats0 (unListL $1) }
    | 'list'    {% parse p_pats1 $1 }

pats1 :: { HPat }
    : ',' pats0            { b_tupP $1 $2 }
    | '@' idsym pat        {% b_asP $2 $3 }
    | conid '{' labelp '}' {% b_labeledP $1 $3 }
    | conid pats0          {% b_conP $1 $2 }

labelp :: { [(Code, HPat)] }
    : rlabelp { reverse $1 }

rlabelp :: { [(Code, HPat)] }
    : {- empty -}       { [] }
    | rlabelp idsym pat { ($2, $3):$1 }


-- ---------------------------------------------------------------------
--
-- Expressions
--
-- ---------------------------------------------------------------------

expr :: { HExpr }
    : atom     { $1 }
    | 'list'   {% parse p_exprs $1 }

atom :: { HExpr }
    : idsym     {% b_varE $1 }
    | 'char'    {% b_charE $1 }
    | 'string'  {% b_stringE $1 }
    | 'integer' {% b_integerE $1 }
    | 'frac'    {% b_floatE $1 }
    | 'unit'    { b_unitE $1 }
    | 'hslist'  {% b_hsListE `fmap` parse p_hlist (unListL $1) }

exprs :: { HExpr }
    : '\\' lambda           { b_lamE $2 }
    | ',' app               { b_tupE $1 $2 }
    | 'let' lbinds expr     { b_letE $1 $2 $3 }
    | 'if' expr expr expr   { b_ifE $1 $2 $3 $4 }
    | 'case' expr matches   { b_caseE $1 $2 $3 }
    | 'do' do_stmts         { b_doE $1 $2 }
    | '::' expr dtype       { b_tsigE $1 $2 $3 }
    | idsym '{' fbinds '}'  {% b_recConOrUpdE $1 $3 }
    | 'list' '{' fbinds '}' {% b_recUpdE (parse p_exprs $1) $3 }
    | app                   { b_appE $1 }

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

fbinds :: { [(Located FastString, HExpr)] }
    : rfbinds { reverse $1 }

rfbinds :: { [(Located FastString, HExpr)] }
    : {- empty -}           { [] }
    | rfbinds 'symbol' expr { case $2 of
                                LForm (L l _) ->
                                    (L l (symbolNameFS $2), $3):$1 }

app :: { [HExpr] }
    : rapp { reverse $1 }

rapp :: { [HExpr] }
    : expr      { [$1] }
    | rapp expr { $2 : $1 }

matches :: { [HMatch] }
    : rmatches { reverse $1 }

rmatches :: { [HMatch] }
    : match          { [$1] }
    | rmatches match { $2 : $1 }

match :: { HMatch }
    : pat guards     { b_match $1 $2 }

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
    | atom    { ([L (getLoc $1) (b_GRHS [] $1)], []) }

guards0 :: { [HGRHS] }
    : '|' guards1 { $2 }
    | exprs       { [L (getLoc $1) (b_GRHS [] $1)] }

guards1 :: { [HGRHS] }
    : 'list'         {% b_hgrhs [] `fmap` parse p_guard $1 }
    | 'list' guards1 {% b_hgrhs $2 `fmap` parse p_guard $1 }

guard :: { (HExpr, [HGuardLStmt]) }
    : expr       { ($1, []) }
    | stmt guard { fmap ($1:) $2 }

where :: { ([HGRHS],[HDecl]) }
    : 'list' lbinds0 {% parse p_guards0 $1 >>= \gs -> return (gs,$2) }
    | atom lbinds0   { ([L (getLoc $1) (b_GRHS [] $1)], $2) }


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
-- Identifier
--
-- ---------------------------------------------------------------------

idsym :: { Code }
    : 'symbol'   { $1 }
    | special_id { $1 }

special_id :: { Code }
    : '!'         { $1 }
    | 'as'        { $1 }
    | 'forall'    { $1 }
    | 'hiding'    { $1 }
    | 'qualified' { $1 }

idsyms :: { [Code] }
    : 'list' {% parse p_idsyms1 $1 }

idsyms1 :: { [Code] }
    : ridsyms { reverse $1 }

ridsyms :: { [Code] }
    : idsym         { [$1] }
    | ridsyms idsym { $2 : $1 }

conid :: { Code }
    : 'symbol' { $1 }


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
unListL :: Code -> [Code]
unListL (LForm (L _ form)) =
    case form of
      List xs   -> xs
      HsList xs -> xs
      _         -> []
}
