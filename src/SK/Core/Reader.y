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

'require' { L _ (TSymbol "require") }
'symbol'  { L _ (TSymbol _) }
'char'    { L _ (TChar _) }
'string'  { L _ (TString _) }
'integer' { L _ (TInteger _) }
'frac'    { L _ (TFractional _) }
'comment' { L _ (TDocCommentNext _) }
'unit'    { L _ TUnit }

%%

sexp :: { Code }
     : atom          { $1 }
     | 'quote' sexp  { mkQuote $1 $2 }
     | '`' sexp      { mkQuasiQuote $1 $2 }
     | ',' sexp      { mkUnquote $1 $2 }
     | ',@' sexp     { mkUnquoteSplice $1 $2 }
     | '[' sexps ']' { LForm (L (getLoc $1) (HsList $2)) }
     | '(' sexps ')' { LForm (L (getLoc $1) (List $2)) }

-- Required modules are added to SPState here. The reason is, to support
-- requiring modules in home package when compiling multiple modules
-- with "--make" command, to get the modules information before macro
-- expansion phase.

sexps :: { [Code] }
      : 'require' 'symbol' {% mkRequire $1 $2 }
      | rsexps             { reverse $1 }

rsexps :: { [Code] }
       : {- empty -} { [] }
       | rsexps sexp { $2 : $1 }

atom :: { Code }
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

sym :: SrcSpan -> String -> Code
sym l str = LForm (L l (Atom (ASymbol str)))

li :: SrcSpan -> [Code] -> Code
li l xs = LForm (L l (List xs))

mkQuote :: Located Token -> Code -> Code
mkQuote (L l _) body = li l [sym l "quote", body]

mkQuasiQuote :: Located Token -> Code -> Code
mkQuasiQuote (L l _) body = li l [sym l "quasiquote", body]

mkUnquote :: Located Token -> Code -> Code
mkUnquote (L l _) body = li l [sym l "unquote", body]

mkUnquoteSplice :: Located Token -> Code -> Code
mkUnquoteSplice (L l _) body = li l [sym l "unquote-splice", body]

mkRequire :: Located Token -> Located Token -> SP [Code]
mkRequire t1 t2@(L _ (TSymbol modName)) = do
  addRequiredModuleName modName
  return [mkASymbol t1, mkASymbol t2]

mkASymbol :: Located Token -> Code
mkASymbol (L l (TSymbol x)) = LForm (L l (Atom (ASymbol x)))

mkAChar :: Located Token -> Code
mkAChar (L l (TChar x)) = LForm (L l (Atom (AChar x)))

mkAString :: Located Token -> Code
mkAString (L l (TString x)) = LForm (L l (Atom (AString x)))

mkAInteger :: Located Token -> Code
mkAInteger (L l (TInteger x)) = LForm (L l (Atom (AInteger x)))

mkAFractional :: Located Token -> Code
mkAFractional (L l (TFractional x)) = LForm (L l (Atom (AFractional x)))

mkAComment :: Located Token -> Code
mkAComment (L l (TDocCommentNext x)) = LForm (L l (Atom (AComment x)))

mkAUnit :: Located Token -> Code
mkAUnit (L l TUnit) = LForm (L l (Atom AUnit))

mkOcSymbol :: Located Token -> Code
mkOcSymbol (L l _) = LForm (L l (Atom (ASymbol "{")))

mkCcSymbol :: Located Token -> Code
mkCcSymbol (L l _) = LForm (L l (Atom (ASymbol "}")))

happyError :: SP a
happyError = showErrorSP
}
