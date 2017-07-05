-- Happy parser for S-expression tokens.
{
module SK.Core.Reader
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
'('         { L _ TOparen }
')'         { L _ TCparen }
'['         { L _ TObracket }
']'         { L _ TCbracket }
'{'         { L _ TOcurly }
'}'         { L _ TCcurly }

'quote'     { L _ TQuote }
'`'         { L _ TQuasiQuote }
','         { L _ TUnquote }
',@'        { L _ TUnquoteSplice }

'symbol'  { L _ (TSymbol _) }
'char'    { L _ (TChar _) }
'string'  { L _ (TString _) }
'integer' { L _ (TInteger _) }
'frac'    { L _ (TFractional _) }
'comment' { L _ (TDocCommentNext _) }
'unit'    { L _ TUnit }

%%

sexp :: { LCode }
     : atom          { $1 }
     | 'quote' sexp  { L (getLoc $1) (TList [mkQuote $1, $2]) }
     | '`' sexp      { L (getLoc $1) (TList [mkQuasiQuote $1, $2]) }
     | ',' sexp      { L (getLoc $1) (TList [mkUnquote $1, $2]) }
     | ',@' sexp     { L (getLoc $1) (TList [mkUnquoteSplice $1, $2]) }
     | '[' sexps ']' { L (getLoc $1) (THsList $2) }
     | '(' sexps ')' { L (getLoc $1) (TList $2) }

sexps :: { [LCode] }
      : rsexps { reverse $1 }

rsexps :: { [LCode] }
       : {- empty -} { [] }
       | rsexps sexp { $2 : $1 }

atom :: { LCode }
     : 'symbol'  { mkASymbol $1 }
     | 'char'    { mkAChar $1 }
     | 'string'  { mkAString $1 }
     | 'integer' { mkAInteger $1 }
     | 'frac'    { mkAFractional $1 }
     | 'comment' { mkAComment $1 }
     | 'unit'    { mkAUnit $1 }
     | '{'       { mkOcSymbol $1 }
     | '}'       { mkCcSymbol $1 }

{
mkQuote :: Located Token -> LCode
mkQuote (L l _) = L l (TAtom (ASymbol "quote"))

mkQuasiQuote :: Located Token -> LCode
mkQuasiQuote (L l _) = L l (TAtom (ASymbol "quasiquote"))

mkUnquote :: Located Token -> LCode
mkUnquote (L l _) = L l (TAtom (ASymbol "unquote"))

mkUnquoteSplice :: Located Token -> LCode
mkUnquoteSplice (L l _) = L l (TAtom (ASymbol "unquote-splice"))

mkASymbol :: Located Token -> LCode
mkASymbol (L l (TSymbol x)) = L l (TAtom (ASymbol x))

mkAChar :: Located Token -> LCode
mkAChar (L l (TChar x)) = L l (TAtom (AChar x))

mkAString :: Located Token -> LCode
mkAString (L l (TString x)) = L l (TAtom (AString x))

mkAInteger :: Located Token -> LCode
mkAInteger (L l (TInteger x)) = L l (TAtom (AInteger x))

mkAFractional :: Located Token -> LCode
mkAFractional (L l (TFractional x)) = L l (TAtom (AFractional x))

mkAComment :: Located Token -> LCode
mkAComment (L l (TDocCommentNext x)) = L l (TAtom (AComment x))

mkAUnit :: Located Token -> LCode
mkAUnit (L l TUnit) = L l (TAtom AUnit)

mkOcSymbol :: Located Token -> LCode
mkOcSymbol (L l _) = L l (TAtom (ASymbol "{"))

mkCcSymbol :: Located Token -> LCode
mkCcSymbol (L l _) = L l (TAtom (ASymbol "}"))

happyError :: SP a
happyError = showErrorSP
}
