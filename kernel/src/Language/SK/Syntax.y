-- -*- mode: haskell; -*-
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
  ( -- * Haskell AST parsers
    parseModule
  , parseImports
  , parseLImport
  , parseStmt
  , parseDecls
  , parseTopDecls
  , parseExpr
  , parseType

    -- * Forms for documentation comment
    -- $docforms
  ) where

-- ghc
import BasicTypes ( Activation(..), FixityDirection(..)
                  , InlineSpec(..), OverlapMode(..) )
import FastString (FastString)
import ForeignCall (Safety)
import HsDoc (LHsDocString)
import HsExpr (GRHS(..))
import SrcLoc (GenLocated(..), Located, getLoc, noLoc)

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
%name p_lcdecl lcdecl
%name p_lidecl lidecl
%name p_lqtycl lqtycl
%name p_sfsig sfsig
%name p_lsimpletype lsimpletype
%name p_ldatafamcon ldatafamcon
%name p_famconhd famconhd
%name p_lfinsthd lfinsthd
%name p_lfameq lfameq
%name p_phase phase

%name p_type type
%name p_types types
%name p_types0 types0
%name p_lconstr lconstr
%name p_lqtycon lqtycon
%name p_lkindtv lkindtv
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
'_'  { LForm (L _ (Atom (ASymbol "_"))) }

-- Non Haskell 2010 reserved id, but treated specially
'as'        { LForm (L _ (Atom (ASymbol "as"))) }
'hiding'    { LForm (L _ (Atom (ASymbol "hiding"))) }
'qualified' { LForm (L _ (Atom (ASymbol "qualified"))) }

-- GHC Extensions
'family' { LForm (L _ (Atom (ASymbol "family"))) }
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

-- Documentation forms

'docn' { LForm (L _ (List [LForm (L _ (Atom (ASymbol ":docn"))), $$])) }
'docp' { LForm (L _ (List [LForm (L _ (Atom (ASymbol ":docp"))), $$])) }
'dock' { LForm (L _ (List (LForm (L _ (Atom (ASymbol ":dock"))) : _))) }
'dh1'  { LForm (L _ (List (LForm (L _ (Atom (ASymbol ":dh1"))) : _))) }
'dh2'  { LForm (L _ (List (LForm (L _ (Atom (ASymbol ":dh2"))) : _))) }
'dh3'  { LForm (L _ (List (LForm (L _ (Atom (ASymbol ":dh3"))) : _))) }
'dh4'  { LForm (L _ (List (LForm (L _ (Atom (ASymbol ":dh4"))) : _))) }

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

docnext :: { LHsDocString }
    : 'docn' {% b_docString $1}

docprev :: { LHsDocString }
    : 'docp' {% b_docString $1 }

mbdocnext :: { Maybe LHsDocString }
    : docnext     { Just $1 }
    | {- empty -} { Nothing }

mbdocprev :: { Maybe LHsDocString }
    : docprev     { Just $1 }
    | {- empty -} { Nothing}


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
    | 'dh1'    {% b_ieGroup 1 $1 }
    | 'dh2'    {% b_ieGroup 2 $1 }
    | 'dh3'    {% b_ieGroup 3 $1 }
    | 'dh4'    {% b_ieGroup 4 $1 }
    | 'docn'   {% b_ieDoc $1 }
    | 'dock'   {% b_ieDocNamed $1 }
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
    | 'dh1'     {% b_docGroupD 1 $1 }
    | 'dh2'     {% b_docGroupD 2 $1 }
    | 'dh3'     {% b_docGroupD 3 $1 }
    | 'dh4'     {% b_docGroupD 4 $1 }
    | 'dock'    {% b_docNamed $1 }

top_decl :: { HDecl }
    : 'data' simpletype constrs            { b_dataD $1 $2 $3 }
    | 'data' 'family' datafamcon           { b_datafamD $1 $3 }
    | 'data' 'instance' finsthd constrs    { b_datainstD $1 $3 $4 }
    | 'type' simpletype type               { b_typeD $1 $2 $3 }
    | 'type' 'family' datafamcon fameqs    { b_tyfamD $4 $1 $3 }
    | 'type' 'instance' finsthd type       { b_tyinstD $1 $3 $4 }
    | 'newtype' simpletype constrs         { b_newtypeD $1 $2 $3 }
    | 'newtype' 'instance' finsthd constrs { b_newtypeinstD $1 $3 $4 }
    | 'class' qtycl cdecls                 {% b_classD $2 $3 }
    | 'instance' overlap qtycl idecls      {% b_instD $2 $3 $4 }
    | 'default' zero_or_more_types         { b_defaultD $2 }
    | fixity 'integer' idsyms1             {% b_fixityD $1 $2 $3 }
    | foreign                              { $1 }
    | decl                                 { $1 }

overlap :: { Maybe (Located OverlapMode) }
    : {- empty -}    { Nothing }
    | 'overlappable' { b_overlapP $1 }
    | 'overlapping'  { b_overlapP $1 }
    | 'overlaps'     { b_overlapP $1 }
    | 'incoherent'   { b_overlapP $1 }

sfsig :: { (Code, HType) }
    : '::' idsym type { ($2, $3) }

simpletype :: { (FastString, [HTyVarBndr], Maybe HKind)}
    : conid  {% getConId $1 >>= \n -> return (n, [], Nothing) }
    | 'list' {% parse p_lsimpletype $1 }

lsimpletype :: { (FastString, [HTyVarBndr], Maybe HKind) }
    : '::' conid type {% getConId $2 >>= \n -> return (n, [], Just $3) }
    | conid tvbndrs   {% getConId $1 >>= \n -> return (n, $2, Nothing) }

constrs :: { (HDeriving, [HConDecl]) }
    : rconstrs { let (m,d) = $1 in (m,reverse d) }

rconstrs :: { (HDeriving, [HConDecl]) }
    : {- empty -}            { (noLoc [], []) }
    | rconstrs deriving      { b_derivD $1 $2 }
    | rconstrs constr        { fmap ($2:) $1 }

constr :: { HConDecl }
    : conid mbdocprev  {% addConDoc' $2 `fmap` b_conOnlyD $1 }
    | 'list' mbdocprev {% addConDoc' $2 `fmap` parse p_lconstr $1 }
    | docnext conid    {% addConDoc'' $1 `fmap` b_conOnlyD $2 }
    | docnext 'list'   {% addConDoc'' $1 `fmap` parse p_lconstr $2 }

deriving :: { [HType] }
    : 'deriving' {% parse p_types $1 }

lconstr :: { HConDecl }
    : '::' conid dtype   {% b_gadtD $2 $3 }
    | 'forall' forallcon { b_forallD (fst $2) (snd $2) }
    | lqtycon            { b_qtyconD $1 }

forallcon :: { ([HTyVarBndr], (HConDecl, [HType])) }
    : qtycon           { ([], $1) }
    | tvbndr forallcon { case $2 of (vs,con) -> ($1:vs,con) }

tvbndrs :: { [HTyVarBndr] }
    : rtvbndrs { reverse $1 }

rtvbndrs :: { [HTyVarBndr] }
    : {- empty -}     { [] }
    | rtvbndrs tvbndr { $2:$1 }

tvbndr :: { HTyVarBndr }
    : idsym  { codeToUserTyVar $1 }
    | 'list' {% parse p_lkindtv $1 }

lkindtv :: { HTyVarBndr }
    : '::' idsym type {% kindedTyVar $1 $2 $3 }

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
    : conid  {% b_conOnlyD $1 }
    | 'list' {% parse p_lh98constr $1 }

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
    : idsym  type_without_doc mbdocprev {% b_recFieldD [$1] $2 $3 }
    | idsyms type_without_doc mbdocprev {% b_recFieldD $1 $2 $3 }
    | docnext idsym type_without_doc    {% b_recFieldD [$2] $3 (Just $1) }
    | docnext idsyms type_without_doc   {% b_recFieldD $2 $3 (Just $1) }

qtycl :: { ([HType], HType) }
    : 'list' {% parse p_lqtycl $1 }

lqtycl :: { ([HType], HType) }
    : '=>' 'unit' type  { ([], $3) }
    | '=>' 'list' types {% parse p_types0 $2 >>= b_qtyclC . (:$3) }
    | types0            { ([], $1) }

cdecls :: { [HDecl] }
    : rcdecls { reverse $1 }

rcdecls :: { [HDecl] }
    : {- empty -}   { [] }
    | rcdecls cdecl { $2:$1 }

cdecl :: { HDecl }
    : 'docp' {% b_docprevD $1 }
    | 'docn' {% b_docnextD $1 }
    | 'list' {% parse p_lcdecl $1 }

lcdecl :: { HDecl }
    : 'type' datafamcon              { b_tyfamD [] $1 $2 }
    | 'type' 'instance' finsthd type { b_tyinstD $1 $3 $4 }
    | 'data' datafamcon              { b_datafamD $1 $2 }
    | decl                           { $1 }

idecls :: { [HDecl] }
    : ridecls { reverse $1 }

ridecls :: { [HDecl] }
    : {- empty -}   { [] }
    | ridecls idecl { $2:$1 }

idecl :: { HDecl }
    : 'list' {% parse p_lidecl $1 }

lidecl :: { HDecl }
    : 'type' finsthd type    { b_tyinstD $1 $2 $3 }
    | 'data' finsthd constrs { b_datainstD $1 $2 $3 }
    | decl                   { $1 }

datafamcon :: { (FastString, [HTyVarBndr], Maybe HType) }
    : 'list' {% parse p_ldatafamcon $1 }

ldatafamcon :: { (FastString, [HTyVarBndr], Maybe HType)  }
    : '::' 'list' type {% do { (n,tv) <- parse p_famconhd $2
                             ; return (n,tv,Just $3)} }
    | famconhd         { case $1 of (n,tv) -> (n,tv,Nothing) }

famconhd :: { (FastString, [HTyVarBndr]) }
    : conid tvbndrs {% getConId $1 >>= \n -> return (n,$2) }

finsthd :: { (Located FastString, [HType]) }
    : 'list' {% parse p_lfinsthd $1 }

lfinsthd :: { (Located FastString, [HType]) }
    : conid types {% do {n <- getConId $1
                        ;case $1 of LForm (L l _) -> return (L l n,$2)} }

fameqs :: { [(Located FastString, [HType], HType)] }
    : rfameqs { reverse $1 }

rfameqs :: { [(Located FastString, [HType], HType)] }
    : {- empty -}     { [] }
    | rfameqs fameq { $2:$1 }

fameq :: { (Located FastString, [HType], HType) }
    : 'list' {% parse p_lfameq $1 }

lfameq :: { (Located FastString, [HType], HType) }
    : '=' finsthd type { case $2 of (c,ts) -> (c,ts,$3) }

fixity :: { FixityDirection }
    : 'infixl' { InfixL }
    | 'infixr' { InfixR }
    | 'infix'  { InfixN }

foreign :: { HDecl }
    : 'foreign' 'symbol' ccnv {- safety -} {- "" -} 'list'
      {% do { (name, ty) <- parse p_sfsig $4
            ; let entity = LForm (noLoc (Atom (AString "")))
            ; b_ffiD $1 $2 $3 Nothing entity (name, ty) } }
    | 'foreign' 'symbol' ccnv {- safety -} fentity  'list'
      {% parse p_sfsig $5 >>= b_ffiD $1 $2 $3 Nothing $4 }
    | 'foreign' 'symbol' ccnv safety       fentity  'list'
      {% parse p_sfsig $6 >>= b_ffiD $1 $2 $3 (Just $4) $5 }

ccnv :: { HCCallConv }
    : 'symbol' {% b_callConv $1 }

safety :: { Located Safety }
    : 'symbol' {% b_safety $1 }

fentity :: { Code }
    : 'string' { $1 }

decl :: { HDecl }
    : '=' pats_and_guards    {% case $2 of (g,p) -> b_funOrPatD $1 p g }
    | '::' idsym dtype       {% b_tsigD [$2] $3 }
    | '::' idsyms dtype      {% b_tsigD $2 $3 }
    | 'inline' actv idsym    {% b_inlineD Inline $2 $3 }
    | 'noinline' actv idsym  {% b_inlineD NoInline $2 $3 }
    | 'inlinable' actv idsym {% b_inlineD Inlinable $2 $3 }
    | 'specialize' actv 'list'
      {% do { sig <- parse p_sfsig $3
            ; b_specializeD $1 $2 sig }}
    | 'specialize' 'inline' actv 'list'
      {% do { sig <- parse p_sfsig $4
            ; b_specializeInlineD $1 $3 sig }}

pats_and_guards :: { (([HGRHS],[HDecl]), [HPat]) }
    : guards              { ($1, []) }
    | pat pats_and_guards { ($1:) `fmap` $2 }

dtype :: { ([HType], HType) }
    : 'symbol' {% (\t -> ([], t)) `fmap` b_symT $1}
    | 'unit'   { ([], b_unitT $1) }
    | 'hslist' {% do { t <- parse p_type [toListL $1]
                     ; return ([], b_listT t) }}
    | qtycl    { $1 }

actv :: { Maybe Activation }
    : {- empty -} { Nothing }
    | 'hslist'    {% fmap Just (parse p_phase (unListL $1)) }

phase :: { Activation }
    : 'integer'     {% b_activation ActiveAfter $1 }
    | '~' 'integer' {% b_activation ActiveBefore $2 }
    | idsym         {% b_activation ActiveBefore $1 }

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
    : type_without_doc mbdocprev { maybe $1 (b_docT $1) $2 }

type_without_doc :: { HType }
    : 'symbol'       {% b_symT $1 }
    | type_no_symbol { $1 }

type_no_symbol :: { HType }
    : special_id_no_bang {% b_symT $1 }
    | 'unpack' type      { b_unpackT $1 $2 }
    | '!' type           { b_bangT $1 $2 }
    | '_'                { b_anonWildT $1 }
    | 'unit'             { b_unitT $1 }
    | '~'                { b_tildeT $1 }
    | 'string'           {% b_tyLitT $1 }
    | 'integer'          {% b_tyLitT $1 }
    | 'hslist'           {% case toListL $1 of
                              LForm (L _ (List [])) -> return (b_nilT $1)
                              xs -> b_listT `fmap` parse p_type [xs] }
    | 'list'             {% parse p_types0 $1 }

types0 :: { HType }
    : '->' zero_or_more_types           {% b_funT $1 $2 }
    | ',' zero_or_more_types            { b_tupT $1 $2 }
    | 'forall' forallty                 { b_forallT $1 $2 }
    | '::' type type                    { b_kindedType $1 $2 $3 }
    | 'symbol' zero_or_more_types       {% b_opOrAppT $1 $2 }
    | type_no_symbol zero_or_more_types {% b_appT ($1:$2) }

forallty :: { ([HTyVarBndr], ([HType], HType)) }
    : qtycl           { ([], $1) }
    | tvbndr forallty { case $2 of (vs,ty) -> ($1:vs,ty) }

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
    | '!' pat_ { b_bangP $2 }
    | pat_     { $1 }

pat_ :: { HPat }
    : 'integer'          {% b_intP $1 }
    | 'string'           {% b_stringP $1 }
    | 'char'             {% b_charP $1 }
    | 'unit'             {% b_unitP $1 }
    | '_'                { b_wildP $1 }
    | special_id_no_bang {% b_symP $1 }
    | 'symbol'           {% b_symP $1 }
    | 'hslist'           {% b_hsListP `fmap` parse p_pats0 (unListL $1) }
    | 'list'             {% parse p_pats1 $1 }

pats1 :: { HPat }
    : ',' pats0            { b_tupP $1 $2 }
    | '@' idsym pat        {% b_asP $2 $3 }
    | conid '{' labelp '}' {% b_labeledP $1 $3 }
    | conid pats0          {% b_conP [$1] False $2 }
    | 'list' pats0         {% b_conP $1 True $2 }
    | '::' pat type        { b_sigP $1 $2 $3 }

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

expr_no_idsym :: { HExpr }
    : atom_no_idsym { $1 }
    | 'list'        {% parse p_exprs $1 }

atom :: { HExpr }
    : idsym         {% b_varE $1 }
    | atom_no_idsym { $1 }

atom_no_idsym :: { HExpr }
    : 'char'    {% b_charE $1 }
    | 'string'  {% b_stringE $1 }
    | 'integer' {% b_integerE $1 }
    | 'frac'    {% b_floatE $1 }
    | 'unit'    { b_unitE $1 }
    | 'hslist'  {% b_hsListE `fmap` parse p_hlist (unListL $1) }

exprs :: { HExpr }
    : '\\' lambda           { b_lamE $2 }
    | ',' app               { b_tupE $1 $2 }
    | ','                   { b_tupConE $1 }
    | 'let' lbinds expr     {% b_letE $1 $2 $3 }
    | 'if' expr expr expr   { b_ifE $1 $2 $3 $4 }
    | 'case' expr matches   { b_caseE $1 $2 $3 }
    | 'do' stmts            { b_doE $1 $2 }
    | '::' expr dtype       { b_tsigE $1 $2 $3 }
    | idsym '{' fbinds '}'  {% b_recConOrUpdE $1 $3 }
    | 'list' '{' fbinds '}' {% b_recUpdE (parse p_exprs $1) $3 }
    | expr                  { $1 }
    | idsym app             {% b_opOrAppE $1 $2 }
    | expr_no_idsym app     { b_appE ($1:$2) }

lambda :: { (HExpr,[HPat]) }
     : expr       { ($1,[]) }
     | pat lambda { fmap ($1:) $2 }

lbinds :: { [HDecl] }
    : 'unit' { [] }
    | 'list' {% parse p_lbinds0 $1 }

lbinds0 :: { [HDecl] }
    : rlbinds0 { reverse $1 }

rlbinds0 :: { [HDecl] }
    : {- empty -}     { [] }
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
    | expr '|' stmts      { Left (b_lcompE $1 $3) }
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

stmts :: { [HStmt] }
    : rstmts { reverse $1 }

rstmts :: { [HStmt] }
    : stmt           { [$1] }
    | rstmts stmt { $2 : $1 }

stmt :: { HStmt }
    : atom     { b_bodyS $1 }
    | 'list'   {% parse p_stmt1 $1 }

stmt1 :: { HStmt }
    : '<-' pat expr { b_bindS $1 $2 $3 }
    | 'let' lbinds  {% b_letS $1 $2 }
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
    : '!'                { $1 }
    | special_id_no_bang { $1 }

special_id_no_bang :: { Code }
    : 'as'        { $1 }
    | 'family'    { $1 }
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

-- | Parser for Haskell module.
parseModule :: Builder HModule
parseModule = parse_module

-- | Parser for import declarations.
parseImports :: Builder [HImportDecl]
parseImports = p_imports

-- | Parser for single import declaration.
parseLImport :: Builder HImportDecl
parseLImport = p_limport

-- | Parser for statement.
parseStmt :: Builder HStmt
parseStmt = p_stmt

-- | Parser for declarations.
parseDecls :: Builder [HDecl]
parseDecls = p_decls

-- | Parser for top level declarations.
parseTopDecls :: Builder [HDecl]
parseTopDecls = p_top_decls

-- | Parser for Haskell expression.
parseExpr :: Builder HExpr
parseExpr = p_expr

-- | Parser for Haskell type.
parseType :: Builder HType
parseType = p_type

-- | Unwrap the element of 'List' and 'HsList', otherwise returns '[]'.
unListL :: Code -> [Code]
unListL (LForm (L _ form)) =
    case form of
      List xs   -> xs
      HsList xs -> xs
      _         -> []

-- $docforms
--
-- There are four kinds of forms for documentation comments.
--
-- [@:docn@]: The @(:docn "comment")@ form is for writing
-- documentation with @comment@ for the next element. It can appear in
-- export entities list, or in top level declarations. It is analogous
-- to Haskell comments starting with @|@.
--
-- [@:docp@]: The @(:docp "comment")@ form is like /:docn/, but for
-- previous form. Unlike /:docn/, it cannot appear in export entities
-- list. It is analogous to Haskell comments starting with @^@.
--
-- [@:dock@]: The @(:dock name)@ and @(:dock name "comment")@ form is
-- for referencing documentation. @(:dock name)@ is used in export
-- entities list to refer other documentation comment, and @(:dock name
-- "comment")@ is for top level to contain the documentation contents.
-- It is analogous to Haskell comment starting with @$name@.
--
-- [@:dh1, :dh2, :dh3, and :dh4@]: The @(:dh1 "comment")@ is for level
-- 1 documentation section header. There are four levels of section
-- headers: @:dh1@, @:dh2@, @:dh3@, and @:dh4@. It could be used in
-- export entities list, or in top level declaration when the module
-- does not contain explicit export entities. It is analogous to Haskell
-- comments starting with @*@s.
}
