-- -*- mode: haskell; -*-
{
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Module for parsing form data.
--
-- This module contains Happy parser for S-expression forms. Unlike the lexer
-- for reading source code, parser defined in this module takes a list of 'Code'
-- data as input, and converts to internal abstract syntax tree data defined in
-- GHC.
--
module Language.Finkel.Syntax
  (
    -- * Forms for documentation comment
    -- $docforms

    -- * Haskell AST parsers
    parseModule
  , parseImports
  , parseLImport
  , parseStmt
  , parseDecls
  , parseTopDecls
  , parseExpr
  , parseType
  ) where

#include "ghc_modules.h"

-- ghc
import GHC_Types_Basic ( Activation(..), FixityDirection(..), InlineSpec(..)
                       , OverlapMode(..), SourceText(..) )
import GHC_Data_FastString (FastString)
import GHC_Types_ForeignCall (Safety)
import GHC_Hs_Doc (LHsDocString)
import GHC_Hs_Expr (GRHS(..))
import GHC_Types_SrcLoc (GenLocated(..), Located, getLoc, noLoc)

#if MIN_VERSION_ghc(8,6,0)
import GHC_Hs_Decls (DerivStrategy(..))
#else
import BasicTypes (DerivStrategy(..))
#endif

-- Internal
import Language.Finkel.Builder
import Language.Finkel.Form
import Language.Finkel.Syntax.HDecl
import Language.Finkel.Syntax.HExpr
import Language.Finkel.Syntax.HIE
import Language.Finkel.Syntax.HPat
import Language.Finkel.Syntax.HType
import Language.Finkel.Syntax.SynUtils
}

%name parse_module module
%name p_mod_header mod_header

%name p_entity entity
%name p_entities entities

%name p_imports imports
%name p_limport limport

%name p_top_decls top_decls
%name p_top_decl top_decl
%name p_decl decl
%name p_decls decls
%name p_decl_tsig decl_tsig
%name p_lcdecl lcdecl
%name p_lidecl lidecl
%name p_field_detail field_detail
%name p_lqtycl lqtycl
%name p_sfsig sfsig
%name p_lsimpletype lsimpletype
%name p_ldconhead ldconhead
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
%name p_lkindtv_specific lkindtv_specific
%name p_lh98constr lh98constr
%name p_deriving_clause deriving_clause
%name p_standalone_deriv standalone_deriv

%name p_pat pat
%name p_pats pats
%name p_pats0 pats0
%name p_pats1 pats1
%name p_label1p label1p

%name p_expr expr
%name p_exprs exprs
%name p_hlist hlist
%name p_match match
%name p_guards0 guards0
%name p_guards1 guards1
%name p_guard guard
%name p_where where
%name p_lbinds0 lbinds0
%name p_rfbind rfbind
%name p_app app

%name p_stmt stmt
%name p_stmt1 stmt1

%tokentype { Code }
%monad { Builder }
%lexer { formLexer } { LForm (L _ TEnd) }

%token

-- Haskell 2010 reserved ids
'case'     { LForm (L _ (Atom (ASymbol "case"))) }
'class'    { LForm (L _ (Atom (ASymbol "class"))) }
'data'     { LForm (L _ (Atom (ASymbol "data"))) }
'default'  { LForm (L _ (Atom (ASymbol "default"))) }
'deriving' { LForm (L _ (List (LForm (L _ (Atom (ASymbol "deriving"))):$$))) }
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
'anyclass' { LForm (L _ (Atom (ASymbol "anyclass"))) }
'family'   { LForm (L _ (Atom (ASymbol "family"))) }
'forall'   { LForm (L _ (Atom (ASymbol "forall"))) }
'stock'    { LForm (L _ (Atom (ASymbol "stock"))) }
'via'      { LForm (L _ (Atom (ASymbol "via"))) }

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

'doc'  { LForm (L _ (List [LForm (L _ (Atom (ASymbol ":doc"))), $$])) }
'doc^' { LForm (L _ (List [LForm (L _ (Atom (ASymbol ":doc^"))), $$])) }
'doc$' { LForm (L _ (List (LForm (L _ (Atom (ASymbol ":doc$"))) : _))) }
'dh1'  { LForm (L _ (List (LForm (L _ (Atom (ASymbol ":dh1"))) : _))) }
'dh2'  { LForm (L _ (List (LForm (L _ (Atom (ASymbol ":dh2"))) : _))) }
'dh3'  { LForm (L _ (List (LForm (L _ (Atom (ASymbol ":dh3"))) : _))) }
'dh4'  { LForm (L _ (List (LForm (L _ (Atom (ASymbol ":dh4"))) : _))) }

-- Finkel specific quote primitive
':quote' { LForm (L _ (Atom (ASymbol ":quote"))) }

-- Plain constructors
'symbol'  { LForm (L _ (Atom (ASymbol _))) }
'char'    { LForm (L _ (Atom (AChar _ _))) }
'string'  { LForm (L _ (Atom (AString _ _))) }
'integer' { LForm (L _ (Atom (AInteger _))) }
'frac'    { LForm (L _ (Atom (AFractional _))) }
'unit'    { LForm (L _ (Atom AUnit)) }
'list'    { LForm (L _ (List _)) }
'hslist'  { LForm (L _ (HsList _)) }


%%

-- ---------------------------------------------------------------------
--
-- Documentation
--
-- ---------------------------------------------------------------------

list_es :: { [Code] }
    : 'list' {% case unCode $1 of List xs -> pure xs; _ -> builderError }

docnext :: { LHsDocString }
    : 'doc' {% b_docString $1}

docprev :: { LHsDocString }
    : 'doc^' {% b_docString $1 }

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

mod_header :: { Maybe LHsDocString -> [HImportDecl] -> [HDecl] -> HModule }
    : 'symbol' exports {% b_module (Just $1) $2 }

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
    | 'doc'    {% b_ieDoc $1 }
    | 'doc$'   {% b_ieDocNamed $1 }
    | list_es  {% parse p_entity $1 }

entity :: { HIE }
    : conid {- empty -} {% b_ieAbs $1 }
    | conid '..'        {% b_ieAll $1 }
    | conid idsyms1     {% b_ieWith $1 $2 }

entities :: { [HIE] }
    : rentities { reverse $1 }

rentities :: { [HIE] }
    : {- empty -}       { [] }
    | rentities idsym   {% b_ieSym $2 >>= \es -> return (es:$1) }
    | rentities list_es {% fmap (:$1) (parse p_entity $2) }

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
    : 'hiding' list_es {% do { es <- parse p_entities $2
                            ; return (True, Just es) } }
    | list_es          {% do { es <- parse p_entities $1
                            ; return (False, Just es) } }
    | 'unit'           { (False, Just []) }
    | {- empty -}      { (False, Nothing) }


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
    : list_es    {% parse p_top_decl $1 }
    | 'deriving' {% parse p_standalone_deriv $1 }
    | 'doc'      {% b_docnextD $1 }
    | 'doc^'     {% b_docprevD $1 }
    | 'dh1'      {% b_docGroupD 1 $1 }
    | 'dh2'      {% b_docGroupD 2 $1 }
    | 'dh3'      {% b_docGroupD 3 $1 }
    | 'dh4'      {% b_docGroupD 4 $1 }
    | 'doc$'     {% b_docNamed $1 }

top_decl :: { HDecl }
    : 'data' simpletype constrs            { b_dataD $1 $2 $3 }
    | 'data' 'family' dconhead             { b_datafamD $1 $3 }
    | 'data' 'instance' finsthd constrs    { b_datainstD $1 $3 $4 }
    | 'type' simpletype type               { b_typeD $1 $2 $3 }
    | 'type' 'family' dconhead fameqs      { b_tyfamD $4 $1 $3 }
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
    : conid   {% getConId $1 >>= \n -> return (n, [], Nothing) }
    | list_es {% parse p_lsimpletype $1 }

lsimpletype :: { (FastString, [HTyVarBndr], Maybe HKind) }
    : '::' conid type {% getConId $2 >>= \n -> return (n, [], Just $3) }
    | ldconhead     { $1 }

constrs :: { (HDeriving, [HConDecl]) }
    : rconstrs deriving { ($2,reverse $1) }

rconstrs :: { [HConDecl] }
    : {- empty -}     { [] }
    | rconstrs constr { $2 : $1 }

constr :: { HConDecl }
    : conid mbdocprev   {% addConDoc' $2 `fmap` b_conOnlyD $1 }
    | list_es mbdocprev {% addConDoc' $2 `fmap` parse p_lconstr $1 }
    | docnext conid     {% addConDoc'' $1 `fmap` b_conOnlyD $2 }
    | docnext list_es   {% addConDoc'' $1 `fmap` parse p_lconstr $2 }

deriving :: { HDeriving }
    : {- empty -}         { noLoc [] }
    | 'deriving' deriving {% do { ds1 <- parse p_deriving_clause $1
                                ; return (b_derivsD ds1 $2) } }

deriving_clause :: { HDeriving }
    : 'anyclass' types { b_derivD (Just (uL $1 AnyclassStrategy)) $2 }
    | 'newtype' types  { b_derivD (Just (uL $1 NewtypeStrategy)) $2 }
    | 'stock' types    { b_derivD (Just (uL $1 StockStrategy)) $2 }
    | types mb_via     { b_derivD $2 $1 }

mb_via :: { Maybe HDerivStrategy }
    : {- empty -} { Nothing }
    | 'via' type  {% b_viaD $2 }

standalone_deriv :: { HDecl }
    : 'anyclass' 'instance' overlap type
      { b_standaloneD (Just (uL $1 AnyclassStrategy)) $3 $4 }
    | 'newtype' 'instance' overlap type
      { b_standaloneD (Just (uL $1 NewtypeStrategy)) $3 $4 }
    | 'stock' 'instance' overlap type
      { b_standaloneD (Just (uL $1 StockStrategy)) $3 $4}
    | mb_via 'instance' overlap type
      { b_standaloneD $1 $3 $4 }

lconstr :: { HConDecl }
    : '::' conid dtype   {% b_gadtD $2 $3 }
    | 'forall' forallcon { b_forallD (fst $2) (snd $2) }
    | lqtycon            { b_qtyconD $1 }

forallcon :: { ([HTyVarBndrSpecific], (HConDecl, [HType])) }
    : qtycon           { ([], $1) }
    | tvbndr_specific forallcon { case $2 of (vs,con) -> ($1:vs,con) }

lkindtv :: { HTyVarBndr }
    : '::' idsym type {% kindedTyVar $1 $2 $3 }

tvbndr_specific :: { HTyVarBndrSpecific }
    : idsym   { codeToUserTyVarSpecific $1 }
    | list_es {% parse p_lkindtv_specific $1 }

lkindtv_specific :: { HTyVarBndrSpecific }
    : '::' idsym type {% kindedTyVarSpecific $1 $2 $3 }

qtycon :: { (HConDecl, [HType]) }
    : list_es {% parse p_lqtycon $1 }

lqtycon :: { (HConDecl, [HType]) }
    : '=>' 'unit' lh98constr   { ($3, []) }
    | '=>' type tys_h98constr  { let (c,ts) = $3 in (c,$2:reverse ts) }
    | lh98constr               { ($1, []) }

tys_h98constr :: { (HConDecl, [HType]) }
    : h98constr          { ($1, []) }
    | type tys_h98constr { let (c,ts) = $2 in (c,$1:ts) }

h98constr :: { HConDecl }
    : conid   {% b_conOnlyD $1 }
    | list_es {% parse p_lh98constr $1 }

lh98constr :: { HConDecl }
    : conid condetails         {% b_conD $1 $2 }
    | conid '{' fielddecls '}' {% b_conD $1 $3 }

condetails :: { HConDeclDetails }
    : type_args { b_conDeclDetails $1 }

fielddecls :: { HConDeclDetails }
    : fielddecls1 { b_recFieldsD $1 }

fielddecls1 :: { [HConDeclField] }
    : rfielddecls { reverse $1 }

rfielddecls :: { [HConDeclField] }
    : fielddecl             { [$1] }
    | rfielddecls fielddecl { $2:$1 }

fielddecl :: { HConDeclField }
    : list_es mbdocprev {% parse p_field_detail $1 >>= b_recFieldD $2 }
    | docnext list_es   {% parse p_field_detail $2 >>= b_recFieldD (Just $1) }

field_detail :: { ([Code], HType) }
    : '::' fields_and_type { $2 }

fields_and_type :: { ([Code], HType) }
    : type_without_doc              { ([], $1) }
    | idsym_no_bang fields_and_type { case $2 of (ns,t) -> ($1:ns,t) }

qtycl :: { ([HType], HType) }
    : list_es {% parse p_lqtycl $1 }

lqtycl :: { ([HType], HType) }
    : '=>' 'unit' type   { ([], $3) }
    | '=>' list_es types {% parse p_types0 $2 >>= b_qtyclC . (:$3) }
    | types0_no_qtype    { ([], $1) }

cdecls :: { [HDecl] }
    : rcdecls { reverse $1 }

rcdecls :: { [HDecl] }
    : {- empty -}   { [] }
    | rcdecls cdecl { $2:$1 }

cdecl :: { HDecl }
    : 'doc^'  {% b_docprevD $1 }
    | 'doc'   {% b_docnextD $1 }
    | list_es {% parse p_lcdecl $1 }

lcdecl :: { HDecl }
    : 'type' dconhead                { b_tyfamD [] $1 $2 }
    | 'type' 'instance' finsthd type { b_tyinstD $1 $3 $4 }
    | 'data' dconhead                { b_datafamD $1 $2 }
    | 'default' list_es              {% parse p_decl_tsig $2 >>= b_dfltSigD }
    | decl                           { $1 }

idecls :: { [HDecl] }
    : ridecls { reverse $1 }

ridecls :: { [HDecl] }
    : {- empty -}   { [] }
    | ridecls idecl { $2:$1 }

idecl :: { HDecl }
    : list_es {% parse p_lidecl $1 }

lidecl :: { HDecl }
    : 'type' finsthd type    { b_tyinstD $1 $2 $3 }
    | 'data' finsthd constrs { b_datainstD $1 $2 $3 }
    | decl                   { $1 }

dconhead :: { (FastString, [HTyVarBndr], Maybe HType) }
    : list_es {% parse p_ldconhead $1 }

ldconhead :: { (FastString, [HTyVarBndr], Maybe HType)  }
    : '::' list_es type {% do { (n,tv) <- parse p_famconhd $2
                             ; return (n,tv,Just $3)} }
    | famconhd          { case $1 of (n,tv) -> (n,tv,Nothing) }

famconhd :: { (FastString, [HTyVarBndr]) }
    : conid tvbndrs {% getConId $1 >>= \n -> return (n,$2) }

tvbndrs :: { [HTyVarBndr] }
    : rtvbndrs { reverse $1 }

rtvbndrs :: { [HTyVarBndr] }
    : {- empty -}     { [] }
    | rtvbndrs tvbndr { $2:$1 }

tvbndr :: { HTyVarBndr }
    : idsym   { codeToUserTyVar $1 }
    | list_es {% parse p_lkindtv $1 }


finsthd :: { (Located FastString, [HType]) }
    : list_es {% parse p_lfinsthd $1 }

lfinsthd :: { (Located FastString, [HType]) }
    : conid types {% do { n <- getConId $1
                        ; case $1 of
                           LForm (L l _) -> return (L l n, map parTyApp $2) }}

fameqs :: { [(Located FastString, [HType], HType)] }
    : rfameqs { reverse $1 }

rfameqs :: { [(Located FastString, [HType], HType)] }
    : {- empty -}     { [] }
    | rfameqs fameq { $2:$1 }

fameq :: { (Located FastString, [HType], HType) }
    : list_es {% parse p_lfameq $1 }

lfameq :: { (Located FastString, [HType], HType) }
    : '=' finsthd type { case $2 of (c,ts) -> (c,ts,$3) }

fixity :: { FixityDirection }
    : 'infixl' { InfixL }
    | 'infixr' { InfixR }
    | 'infix'  { InfixN }

foreign :: { HDecl }
    : 'foreign' 'symbol' ccnv {- safety -} {- "" -} list_es
      {% do { (name, ty) <- parse p_sfsig $4
            ; let entity = LForm (noLoc (Atom (AString NoSourceText "")))
            ; b_ffiD $1 $2 $3 Nothing entity (name, ty) } }
    | 'foreign' 'symbol' ccnv {- safety -} fentity  list_es
      {% parse p_sfsig $5 >>= b_ffiD $1 $2 $3 Nothing $4 }
    | 'foreign' 'symbol' ccnv safety       fentity  list_es
      {% parse p_sfsig $6 >>= b_ffiD $1 $2 $3 (Just $4) $5 }

ccnv :: { HCCallConv }
    : 'symbol' {% b_callConv $1 }

safety :: { Located Safety }
    : 'symbol' {% b_safety $1 }

fentity :: { Code }
    : 'string' { $1 }

decl :: { HDecl }
    : '=' pats_and_guards    {% case $2 of (g,p) -> b_funOrPatD $1 p g }
    | decl_tsig              { $1 }
    | 'inline' actv idsym    {% b_inlineD Inline $2 $3 }
    | 'noinline' actv idsym  {% b_inlineD NoInline $2 $3 }
    | 'inlinable' actv idsym {% b_inlineD Inlinable $2 $3 }
    | 'specialize' actv list_es
      {% do { sig <- parse p_sfsig $3
            ; b_specializeD $1 $2 sig }}
    | 'specialize' 'inline' actv list_es
      {% do { sig <- parse p_sfsig $4
            ; b_specializeInlineD $1 $3 sig }}

pats_and_guards :: { (([HGRHS],[HDecl]), [HPat]) }
    : guards              { ($1, []) }
    | pat pats_and_guards { ($1:) `fmap` $2 }

decl_tsig :: { HDecl }
    : '::' idsyms_dtype {% case $2 of (ns,t) -> b_tsigD ns t }

dtype :: { ([HType], HType) }
    : 'symbol' {% (\t -> ([], t)) `fmap` b_symT $1}
    | 'unit'   { ([], b_unitT $1) }
    | 'hslist' {% do { t <- parse p_type [toListL $1]
                     ; return ([], b_listT t) }}
    | qtycl    { $1 }

idsyms_dtype :: { ([Code], ([HType], HType)) }
    : dtype              { ([],$1) }
    | idsym idsyms_dtype { case $2 of (ns,t) -> ($1:ns,t) }

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
   : {- empty -}    { [] }
   | rdecls list_es {% (:$1) `fmap` parse p_decl $2 }


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
    : 'unpack' type { b_unpackT $1 $2 }
    | '!' type      { b_bangT $1 $2 }
    | '_'           { b_anonWildT $1 }
    | 'unit'        { b_unitT $1 }
    | '~'           { b_tildeT $1 }
    | 'string'      {% b_tyLitT $1 }
    | 'integer'     {% b_tyLitT $1 }
    | 'hslist'      {% case toListL $1 of
                         LForm (L _ (List [])) -> return (b_nilT $1)
                         xs -> fmap b_listT (parse p_type [xs]) }
    | list_es       {% parse p_types0 $1 }

types0 :: { HType }
    : '=>' qtypes     { b_qualT $1 $2 }
    | types0_no_qtype { $1 }

types0_no_qtype :: { HType }
    : '->' type_args           {% b_funT $1 $2 }
    | ',' type_args            { b_tupT $1 $2 }
    | 'forall' forallty        { b_forallT $1 $2 }
    | '::' type type           { b_kindedType $1 $2 $3 }
    | ':quote' conid           {% b_prmConT $2 }
    | ':quote' 'hslist'        {% b_prmListT (parse p_types) $2 }
    | ':quote' list_es         {% b_prmTupT (parse p_types) $2 }
    | 'symbol' type_args       {% b_opOrAppT $1 $2 }
    | type_no_symbol type_args {% b_appT ($1:$2) }

forallty :: { ([HTyVarBndrSpecific], ([HType], HType)) }
    : qtycl                    { ([], $1) }
    | tvbndr_specific forallty { case $2 of (vs,ty) -> ($1:vs,ty) }

qtypes :: { ([HType], HType) }
    : type        { ([], $1) }
    | type qtypes { case $2 of (ctxts,ty) -> ($1:ctxts,ty) }

type_args :: { [HType] }
    : {- empty -}   { [] }
    | rtype_args    { reverse $1 }

rtype_args :: { [HType] }
    : type_arg            { [$1] }
    | rtype_args type_arg { $2 : $1 }

type_arg :: { HType }
    : special_id_no_bang {% b_symT $1 }
    | type               { $1 }

types :: { [HType] }
    : rtypes { reverse $1 }

rtypes :: { [HType] }
    : type        { [$1] }
    | rtypes type { $2 : $1 }

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
    | list_es {% parse p_pats0 $1 }

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
    | list_es            {% parse p_pats1 $1 }

pats1 :: { HPat }
    : ',' pats0            { b_tupP $1 $2 }
    | '@' idsym pat        {% b_asP $2 $3 }
    | conid '{' labelp '}' {% b_labeledP $1 $3 }
    | conid pats0          {% b_conP [$1] False $2 }
    | list_es pats0        {% b_conP $1 True $2 }
    | '::' pat type        { b_sigP $1 $2 $3 }

labelp :: { [PreRecField HPat] }
    : rlabelp { reverse $1 }

rlabelp :: { [PreRecField HPat] }
    : {- empty -}     { [] }
    | rlabelp '..'    { Left $2:$1 }
    | rlabelp idsym   {% fsSymbol $2 >>= \s -> pure (Right (s, Nothing):$1) }
    | rlabelp list_es {% (:$1) `fmap` parse p_label1p $2 }

label1p :: { PreRecField HPat }
    : '=' idsym pat {% fsSymbol $2 >>= \s -> pure (Right (s, Just $3)) }


-- ---------------------------------------------------------------------
--
-- Expressions
--
-- ---------------------------------------------------------------------

expr :: { HExpr }
    : atom    { $1 }
    | list_es {% parse p_exprs $1 }

expr_no_idsym :: { HExpr }
    : atom_no_idsym { $1 }
    | list_es       {% parse p_exprs $1 }

atom :: { HExpr }
    : idsym         {% b_varE $1 }
    | atom_no_idsym { $1 }

atom_no_idsym :: { HExpr }
    : 'char'    {% b_charE $1 }
    | 'string'  {% b_stringE $1 }
    | 'integer' {% b_integerE $1 }
    | 'frac'    {% b_fracE $1 }
    | 'unit'    { b_unitE $1 }
    | 'hslist'  {% b_hsListE `fmap` parse p_hlist (unListL $1) }

exprs :: { HExpr }
    : '\\' lambda            { b_lamE $2 }
    | ',' app                { b_tupE $1 (fst $2) }
    | ','                    { b_tupConE $1 }
    | 'let' lbinds expr      {% b_letE $1 $2 $3 }
    | 'if' expr expr expr    { b_ifE $1 $2 $3 $4 }
    | 'case' expr matches    { b_caseE $1 $2 $3 }
    | 'do' stmts             { b_doE $1 $2 }
    | '::' expr dtype        { b_tsigE $1 $2 $3 }
    | idsym '{' fbinds '}'   {% b_recConOrUpdE $1 $3 }
    | list_es '{' fbinds '}' {% b_recUpdE (parse p_exprs $1) $3 }
    | ':quote' form          {% b_quoteE $2 }
    | idsym app              {% b_opOrAppE $1 $2 }
    | expr_no_idsym app      { case $2 of (es,ts) -> b_appE ($1:es,ts) }
    | expr                   { $1 }

lambda :: { (HExpr,[HPat]) }
     : expr       { ($1,[]) }
     | pat lambda { fmap ($1:) $2 }

lbinds :: { [HDecl] }
    : 'unit' { [] }
    | list_es {% parse p_lbinds0 $1 }

lbinds0 :: { [HDecl] }
    : rlbinds0 { reverse $1 }

rlbinds0 :: { [HDecl] }
    : {- empty -}      { [] }
    | rlbinds0 list_es {% fmap (:$1) (parse p_decl $2) }

fbinds :: { [PreRecField HExpr] }
    : rfbinds { reverse $1 }

rfbinds :: { [PreRecField HExpr] }
    : {- empty -}     { [] }
    | rfbinds '..'    { Left $2:$1 }
    | rfbinds idsym   {% fsSymbol $2 >>= \s -> pure (Right (s, Nothing):$1) }
    | rfbinds list_es {% (:$1) `fmap` parse p_rfbind $2 }

rfbind :: { PreRecField HExpr }
    : '=' 'symbol' expr {% (\s -> (Right (s, Just $3))) `fmap` fsSymbol $2 }

app :: { ([HExpr], [HType]) }
    : rapp { case $1 of (es,ts) -> (reverse es, reverse ts) }

rapp :: { ([HExpr], [HType]) }
    : expr          { ([$1], []) }
    | '@' type      { ([], [parTyApp $2]) }
    | rapp expr     { case $1 of (es,ts) -> ($2:es,ts) }
    | rapp '@' type { case $1 of (es,ts) -> (es,parTyApp $3:ts) }

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
-- Separating the rule for 'where', list_es and atom, so that the 'guards0' rule
-- can try matching the symbol '|' before 'expr' rule, to differentiate the
-- entire form from function application of reserved symbol '|'.

guards :: { ([HGRHS],[HDecl]) }
    : 'where' {% parse p_where $1 }
    | list_es {% parse p_guards0 $1 >>= \gs -> return (gs,[]) }
    | atom    { ([L (getLoc $1) (b_GRHS [] $1)], []) }

guards0 :: { [HGRHS] }
    : '|' guards1 { $2 }
    | exprs       { [L (getLoc $1) (b_GRHS [] $1)] }

guards1 :: { [HGRHS] }
    : list_es         {% b_hgrhs [] `fmap` parse p_guard $1 }
    | list_es guards1 {% b_hgrhs $2 `fmap` parse p_guard $1 }

guard :: { (HExpr, [HGuardLStmt]) }
    : expr       { ($1, []) }
    | stmt guard { fmap ($1:) $2 }

where :: { ([HGRHS],[HDecl]) }
    : list_es lbinds0 {% parse p_guards0 $1 >>= \gs -> return (gs,$2) }
    | atom lbinds0    { ([L (getLoc $1) (b_GRHS [] $1)], $2) }

-- Quoted form

form :: { Code }
    : 'symbol'  { $1 }
    | all_syms  { $1 }
    | 'char'    { $1 }
    | 'string'  { $1 }
    | 'integer' { $1 }
    | 'frac'    { $1 }
    | 'unit'    { $1 }
    | 'list'    { $1 }
    | all_lists { $1 }
    | 'hslist'  { $1 }

all_syms :: { Code }
    : 'case' { $1 }
    | 'class' { $1 }
    | 'data' { $1 }
    | 'default' { $1 }
    | 'do' { $1 }
    | 'foreign' { $1 }
    | 'if' { $1 }
    | 'infix' { $1 }
    | 'infixl' { $1 }
    | 'infixr' { $1 }
    | 'instance' { $1 }
    | 'let' { $1 }
    | 'newtype' { $1 }
    | 'type' { $1 }

    | '!' { $1 }
    | ',' { $1 }
    | '->' { $1 }
    | '..' { $1 }
    | '::' { $1 }
    | '<-' { $1 }
    | '=' { $1 }
    | '=>' { $1 }
    | '@' { $1 }
    | '\\' { $1 }
    | '{' { $1 }
    | '|' { $1 }
    | '}' { $1 }
    | '~' { $1 }
    | '_' { $1 }

    | special_id_no_bang { $1 }

    | 'inlinable' { $1 }
    | 'inline' { $1 }
    | 'noinline' { $1 }
    | 'specialize' { $1 }

    | ':quote' { $1 }

all_lists :: { Code }
    : 'deriving' { consListWith $1 "deriving" }
    | 'import'   { consListWith $1 "import" }
    | 'module'   { consListWith $1 "module" }
    | 'where'    { consListWith $1 "where" }


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
    | list_es  {% parse p_stmt1 $1 }

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

idsym_no_bang :: { Code }
    : 'symbol'           { $1 }
    | special_id_no_bang { $1 }

special_id :: { Code }
    : '!'                { $1 }
    | special_id_no_bang { $1 }

special_id_no_bang :: { Code }
    : 'forall'            { $1 }
    | special_id_no_bg_fa { $1 }

-- special id, no bang, no forall
special_id_no_bg_fa :: { Code }
    : 'anyclass'    { $1 }
    | 'as'          { $1 }
    | 'family'      { $1 }
    | 'hiding'      { $1 }
    | 'stock'       { $1 }
    | 'via'         { $1 }
    | 'qualified'   { $1 }

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

uL :: Code -> a -> Located a
uL (LForm (L l _)) a = L l a
{-# INLINE uL #-}

-- $docforms
--
-- There are four kinds of forms for documentation comments.
--
-- [@:doc@]: The @(:doc "comment")@ form is for writing documentation with
-- @comment@ for the next element. It can appear in export entities list, or in
-- top level declarations. It is analogous to Haskell comments starting with
-- @|@.
--
-- [@:doc^@]: The @(:doc^ "comment")@ form is like /:doc/, but for previous
-- form. Unlike /:doc/, it cannot appear in export entities list. It is
-- analogous to Haskell comments starting with @^@.
--
-- [@:doc$@]: The @(:doc$ name)@ and @(:doc$ name "comment")@ form is for
-- referencing documentation. @(:doc$ name)@ is used in export entities list to
-- refer other documentation comment, and @(:doc$ name "comment")@ is for top
-- level to contain the documentation contents.  It is analogous to Haskell
-- comment starting with @$name@.
--
-- [@:dh1, :dh2, :dh3, and :dh4@]: The @(:dh1 "comment")@ is for level 1
-- documentation section header. There are four levels of section headers:
-- @:dh1@, @:dh2@, @:dh3@, and @:dh4@. It could be used in export entities list,
-- or in top level declaration when the module does not contain explicit export
-- entities. It is analogous to Haskell comments starting with @*@s.
}
