-- Happy parser for S-expression tokens.
{
{-# LANGUAGE OverloadedStrings #-}
module Language.SK.Reader
  ( sexpr
  , sexprs
  , psexpr
  ) where

-- base
import Data.Char (toLower)

-- Internal
import Language.SK.Builder
import Language.SK.Form
import Language.SK.GHC
import Language.SK.Lexer
import Language.SK.Syntax

-- ghc-boot
import GHC.LanguageExtensions (Extension(..))
}

%name sexpr sexp
%name sexprs sexps

%partial psexpr sexp

%tokentype { Located Token }
%monad { SP } { >>= } { return }
%lexer { tokenLexer } { L _ TEOF }

%token
'('       { L _ TOparen }
')'       { L _ TCparen }
'['       { L _ TObracket }
']'       { L _ TCbracket }
'{'       { L _ TOcurly }
'}'       { L _ TCcurly }

'quote'   { L _ TQuote }
'`'       { L _ TQuasiquote }
','       { L _ TUnquote }
',@'      { L _ TUnquoteSplice }
'#'       { L _ THash }
'require' { L _ (TSymbol "require") }

'symbol'  { L _ (TSymbol _) }
'char'    { L _ (TChar _) }
'string'  { L _ (TString _) }
'integer' { L _ (TInteger _) }
'frac'    { L _ (TFractional _) }
'comment' { L _ (TDocCommentNext _) }

%%

-- Required modules are added to SPState here. The reason is to support
-- requiring modules in home package when compiling multiple modules
-- with "--make" command, to get the modules information before macro
-- expansion phase.

sexp :: { Code }
     : atom                    { $1 }
     | 'quote' sexp            { mkQuote $1 $2 }
     | '`' sexp                { mkQuasiquote $1 $2 }
     | ',' sexp                { mkUnquote $1 $2 }
     | ',@' sexp               { mkUnquoteSplice $1 $2 }
     | '[' sexps ']'           { mkHsList $1 $2 }
     | '(' ')'                 { mkUnit $1 }
     | '(' 'require' sexps ')' {% mkRequire $1 $2 $3 }
     | '(' sexps ')'           { mkList $1 $2 }
     | '#' rmac                { $2 }

rmac :: { Code }
    : '#' sexp      {% pragma $2 }
    | 'symbol' sexp {% dispatch $1 $2 }

sexps :: { [Code] }
    : rsexps { reverse $1 }

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
     | '{'       { mkOcSymbol $1 }
     | '}'       { mkCcSymbol $1 }

{
atom :: SrcSpan -> Atom -> Code
atom l x = LForm $ L l $ Atom x
{-# INLINE atom #-}

sym :: SrcSpan -> FastString -> Code
sym l str = atom l $ ASymbol str
{-# INLINE sym #-}

li :: SrcSpan -> [Code] -> Code
li l xs = LForm $ L l $ List xs
{-# INLINE li #-}

mkQuote :: Located Token -> Code -> Code
mkQuote (L l _) body = li l [sym l "quote", body]
{-# INLINE mkQuote #-}

mkQuasiquote :: Located Token -> Code -> Code
mkQuasiquote (L l _) body = li l [sym l "quasiquote", body]
{-# INLINE mkQuasiquote #-}

mkUnquote :: Located Token -> Code -> Code
mkUnquote (L l _) body = li l [sym l "unquote", body]
{-# INLINE mkUnquote #-}

mkUnquoteSplice :: Located Token -> Code -> Code
mkUnquoteSplice (L l _) body = li l [sym l "unquote-splice", body]
{-# INLINE mkUnquoteSplice #-}

mkHsList :: Located Token -> [Code] -> Code
mkHsList (L l _) body = LForm $ L l $ HsList body
{-# INLINE mkHsList #-}

mkUnit :: Located Token -> Code
mkUnit (L l _) = atom l AUnit
{-# INLINE mkUnit #-}

mkList :: Located Token -> [Code] -> Code
mkList (L l _) body = li l body
{-# INLINE mkList #-}

mkRequire :: Located Token -> Located Token -> [Code] -> SP Code
mkRequire lref req rest = do
  case evalBuilder parseLImport rest of
    Right idecl -> addRequiredDecl idecl
    _ -> return ()
  return (mkList lref (mkASymbol req : rest))
{-# INLINE mkRequire #-}

mkASymbol :: Located Token -> Code
mkASymbol (L l (TSymbol x)) = atom l $ ASymbol x
{-# INLINE mkASymbol #-}

mkAChar :: Located Token -> Code
mkAChar (L l (TChar x)) = atom l $ AChar x
{-# INLINE mkAChar #-}

mkAString :: Located Token -> Code
mkAString (L l (TString x)) = atom l $ AString x
{-# INLINE mkAString #-}

mkAInteger :: Located Token -> Code
mkAInteger (L l (TInteger x)) = atom l $ AInteger x
{-# INLINE mkAInteger #-}

mkAFractional :: Located Token -> Code
mkAFractional (L l (TFractional x)) = atom l $ AFractional x
{-# INLINE mkAFractional #-}

mkAComment :: Located Token -> Code
mkAComment (L l (TDocCommentNext x)) = atom l $ AComment x
{-# INLINE mkAComment #-}

mkOcSymbol :: Located Token -> Code
mkOcSymbol (L l _) = sym l "{"
{-# INLINE mkOcSymbol #-}

mkCcSymbol :: Located Token -> Code
mkCcSymbol (L l _) = sym l "}"
{-# INLINE mkCcSymbol #-}

pragma ::  Code -> SP Code
pragma orig@(LForm (L l form)) =
  case form of
    -- Pragma with no arguments.
    Atom (ASymbol sym)
      -- Return the UNPACK form as is. This pragma is handled by
      -- syntax parser of data constructor field.
      | normalize sym == "unpack" -> return orig

    -- Pragma with single argument.
    List [LForm (L _ (Atom (ASymbol sym))), _a1]
      -- Inline pragmas are handled by syntax parser.
      | normalize sym `elem` inlinePragmas -> return orig

    -- Pragma with multiple arguments.
    List (LForm (L _ (Atom (ASymbol sym))):rest)
      | normalize sym == "language" -> do
        let (exts, invalids) = groupExts rest
        case invalids of
          [] -> do
            sp <- getSPState
            putSPState (sp {langExts = exts ++ langExts sp})
            return (emptyBody l)
          _  -> errorSP orig ("LANGUAGE: unsupported pragmas: " ++
                              show invalids)
    _ -> error ("unknown pragma: " ++ show form)
  where
    normalize = map toLower . unpackFS
    inlinePragmas = ["inline", "noinline", "inlinable"]

groupExts :: [Code] -> ([Extension],[Code])
groupExts = foldr f ([],[])
  where
    f form (exts, invalids) =
      case form of
        LForm (L _ (Atom (ASymbol sym)))
          | Just ext <- lookup sym supportedLangExts ->
            (ext:exts, invalids)
        _ -> (exts, form:invalids)

supportedLangExts :: [(FastString, Extension)]
supportedLangExts =
    f [ DeriveDataTypeable
      , DeriveFoldable
      , DeriveFunctor
      , DeriveGeneric
      , DeriveTraversable
      , GeneralizedNewtypeDeriving
      , OverloadedStrings
      , OverloadedLists ]
  where
    f = map (\ext -> (fsLit (show ext), ext))

addRequiredDecl :: HImportDecl -> SP ()
addRequiredDecl idecl =
  SP (\st ->
       let names = requiredModuleNames st
           name = moduleNameString (unLoc (ideclName (unLoc idecl)))
           st' = st { requiredModuleNames = name : names }
       in  return ((), st'))

emptyBody :: SrcSpan -> Code
emptyBody l = li l [sym l "begin"]

dispatch :: Located Token -> Code -> SP Code
dispatch (L _ (TSymbol sym)) form =
  case sym of
    "." -> error "dispatch: dot"
    _   -> errorSP form "dispatch"

happyError :: SP a
happyError = lexErrorSP
}
