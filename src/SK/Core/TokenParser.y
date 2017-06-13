-- Happy parser for S-expression tokens.
{
module SK.Core.TokenParser
  ( sexpr
  , sexprs
  ) where

import SrcLoc

import SK.Core.Lexer
import SK.Core.Form

}

%name sexpr sexp
%name sexprs sexps

%tokentype { Located Token }
%monad { SP } { >>= } { return }
%lexer { tokenLexer } { L _ TEOF }

%token
'('       { L _ TOparen }
')'       { L _ TCparen }
'['       { L _ TObracket }
']'       { L _ TCbracket }
quote     { L _ (TSymbol "quote") }
qquote    { L _ (TSymbol "quasiquote") }
unquote   { L _ (TSymbol "unquote") }
unquotesp { L _ (TSymbol "unquote-splice") }
symbol    { L _ (TSymbol _) }
string    { L _ (TString _) }
integer   { L _ (TInteger _) }
comment   { L _ (TDocCommentNext _) }
unit      { L _ TUnit }

%%

sexp :: { LTForm Atom }
     : atom           { $1 }
     | quote sexp     { L (getLoc $1) (TList [mkQuote $1, $2]) }
     | qquote sexp    { L (getLoc $1) (TList [mkQuasiQuote $1, $2]) }
     | unquote sexp   { L (getLoc $1) (TList [mkUnquote $1, $2]) }
     | unquotesp sexp { L (getLoc $1) (TList [mkUnquoteSplice $1, $2]) }
     | '[' sexps ']'  { L (getLoc $1) (THsList $2) }
     | '(' sexps ')'  { L (getLoc $1) (TList $2) }

sexps :: { [LTForm Atom] }
      : rsexps { reverse $1 }

rsexps :: { [LTForm Atom] }
       : {- empty -} { [] }
       | rsexps sexp { $2 : $1 }

atom :: { LTForm Atom }
     : symbol  { mkASymbol $1 }
     | string  { mkAString $1 }
     | integer { mkAInteger $1 }
     | comment { mkAComment $1 }
     | unit    { mkAUnit $1 }

{
mkQuote :: Located Token -> LTForm Atom
mkQuote (L l _) = L l (TAtom (ASymbol "quote"))

mkQuasiQuote :: Located Token -> LTForm Atom
mkQuasiQuote (L l _) = L l (TAtom (ASymbol "quasiquote"))

mkUnquote :: Located Token -> LTForm Atom
mkUnquote (L l _) = L l (TAtom (ASymbol "unquote"))

mkUnquoteSplice :: Located Token -> LTForm Atom
mkUnquoteSplice (L l _) = L l (TAtom (ASymbol "unquote-splice"))

mkAUnit :: Located Token -> LTForm Atom
mkAUnit (L l TUnit) = L l (TAtom AUnit)

mkASymbol :: Located Token -> LTForm Atom
mkASymbol (L l (TSymbol x)) = L l (TAtom (ASymbol x))

mkAString :: Located Token -> LTForm Atom
mkAString (L l (TString x)) = L l (TAtom (AString x))

mkAInteger :: Located Token -> LTForm Atom
mkAInteger (L l (TInteger x)) = L l (TAtom (AInteger x))

mkAComment :: Located Token -> LTForm Atom
mkAComment (L l (TDocCommentNext x)) = L l (TAtom (AComment x))

happyError :: SP a
happyError = showErrorSP
}
