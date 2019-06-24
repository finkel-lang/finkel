-- -*- mode: haskell; -*-
{
{-# LANGUAGE OverloadedStrings #-}
-- | S-expression reader.
--
-- Parser functions in this module are written with Happy parser
-- generator.
--
module Language.SK.Reader
  ( parseSexprs
  , sexpr
  , sexprs
  , psexpr
  ) where

-- base
import Control.Monad.Fail (MonadFail(..))
import Data.Char (toLower)
import Data.List (foldl')

-- bytestring
import qualified Data.ByteString.Lazy as BL

-- ghc
import FastString (FastString, fsLit, unpackFS)
import HsImpExp (ideclName)
import Module (moduleNameString)
import SrcLoc (GenLocated(..), Located, SrcSpan)

-- ghc-boot
import GHC.LanguageExtensions (Extension(..))

-- Internal
import Language.SK.Builder
import Language.SK.Form
import Language.SK.Lexer
import Language.SK.Syntax
}

%name sexpr_ sexp
%name sexprs_ sexps

%partial psexpr_ sexp

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
'#'       { L _ (THash _) }
'pcommas' { L _ (TPcommas _) }

'symbol'  { L _ (TSymbol _) }
'char'    { L _ (TChar _) }
'string'  { L _ (TString _) }
'integer' { L _ (TInteger _) }
'frac'    { L _ (TFractional _) }

'docn'    { L _ (TDocNext _) }
'docp'    { L _ (TDocPrev _) }
'doch'    { L _ (TDocGroup _ _) }
'dock'    { L _ (TDocNamed _ _) }

%%

-- Unit and List
-- ~~~~~~~~~~~~~
--
-- Empty list will parsed as a unit (i.e. '()' in Haskell), non-empty
-- lists are pased as 'List' value of 'Code'. Empty 'List' value of
-- 'Code' could be referred with 'Language.SK.Form.nil'.

sexp :: { Code }
    : atom                    { $1 }
    | 'quote' sexp            { mkQuote $1 $2 }
    | '`' sexp                { mkQuasiquote $1 $2 }
    | ',' sexp                { mkUnquote $1 $2 }
    | ',@' sexp               { mkUnquoteSplice $1 $2 }
    | '[' sexps ']'           { mkHsList $1 $2 }
    | 'pcommas'               { mkPcommas $1 }
    | '(' sexps ')'           { mkUnitOrList $1 $2}
    | '#' sexp                {% rmac $1 $2 }

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
    | '{'       { mkOcSymbol $1 }
    | '}'       { mkCcSymbol $1 }
    | 'docn'    { mkDocn $1 }
    | 'docp'    { mkDocp $1 }
    | 'doch'    { mkDoch $1 }
    | 'dock'    { mkDock $1 }

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
mkUnquoteSplice (L l _) body = li l [sym l "unquote_splice", body]
{-# INLINE mkUnquoteSplice #-}

mkHsList :: Located Token -> [Code] -> Code
mkHsList (L l _) body = LForm $ L l $ HsList body
{-# INLINE mkHsList #-}

mkPcommas :: Located Token -> Code
mkPcommas (L l (TPcommas n)) = li l [sym l (fsLit (replicate n ','))]
{-# INLINE mkPcommas #-}

mkUnitOrList :: Located Token -> [Code] -> Code
mkUnitOrList (L l _) body =
  case body of
    [] -> atom l AUnit
    _  -> li l body
{-# INLINE mkUnitOrList #-}

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

mkOcSymbol :: Located Token -> Code
mkOcSymbol (L l _) = sym l "{"
{-# INLINE mkOcSymbol #-}

mkCcSymbol :: Located Token -> Code
mkCcSymbol (L l _) = sym l "}"
{-# INLINE mkCcSymbol #-}

mkDocn :: Located Token -> Code
mkDocn (L l (TDocNext str)) = li l [sym l ":docn", atom l (AString str)]
{-# INLINE mkDocn #-}

mkDocp :: Located Token -> Code
mkDocp (L l (TDocPrev str)) = li l [sym l ":docp", atom l (AString str)]
{-# INLINE mkDocp #-}

mkDoch :: Located Token -> Code
mkDoch (L l (TDocGroup n s)) = li l [sym l dh, atom l (AString s)]
  where
    dh = case n of
           1 -> ":dh1"
           2 -> ":dh2"
           3 -> ":dh3"
           _ -> ":dh4"

mkDock :: Located Token -> Code
mkDock (L l (TDocNamed k mb_doc)) =
  case mb_doc of
    Nothing -> li l pre
    Just d  -> li l (pre ++ [atom l (AString d)])
  where
    pre = [sym l ":dock", atom l (ASymbol (fsLit k))]

rmac :: Located Token -> Code -> SP Code
rmac h expr =
  case h of
    L l (THash c) | c == 'p' -> pragma expr
    _ -> errorSP expr "mkHash: unsupported reader macro char"

-- Module from ghc package with codes related to language pragma:
--
-- + libraries/ghc-boot-th/GHC/LanguageExtensions/Type.hs: The file
--   containing definition of language extensions.
--
-- + compiler/main/HeaderInfo.hs: Parses header information.
--
-- + compiler/main/DynFlags.hs: Contains 'supportedExtensions ::
--   [String]'. This is a list of language extension names, and the
--   names with "No" prefix. 'xFlagsDeps' contains list of pair language
--   extension and deprecation messages.
--

pragma :: Code -> SP Code
pragma orig@(LForm (L l form)) =
  case form of
    -- Pragma with no arguments.
    List [LForm (L _ (Atom (ASymbol sym)))]
      -- Return the UNPACK form as is. This pragma is handled by
      -- syntax parser of data constructor field.
      | normalize sym `elem` noArgPragmas -> return orig

    -- Pragma with arguments.
    List (LForm (L l' (Atom (ASymbol sym))):rest)
      | normalize sym `elem` inlinePragmas -> return orig
      | normalize sym == "language" -> do
        let (exts, invalids) = groupExts rest
        case invalids of
          [] -> do
            sp <- getSPState
            putSPState (sp {langExts = exts ++ langExts sp})
            return (emptyBody l)
          _  -> errorSP orig ("Unsupported LANGUAGE pragma: " ++
                              show invalids)
      | normalize sym `elem` spcls -> do
         let specialize = LForm (L l' (Atom (ASymbol "SPECIALIZE")))
         return (LForm (L l (List (specialize:rest))))
      | normalize sym == "options_ghc" -> do
        let flags = makeOptionFlags rest
        sp <- getSPState
        putSPState (sp {ghcOptions = flags})
        return (emptyBody l)
    _ -> error ("unknown pragma: " ++ show form)
  where
    normalize = map toLower . unpackFS
    inlinePragmas = ["inline", "noinline", "inlinable"]
    spcls = ["specialize", "specialise"]

noArgPragmas :: [String]
noArgPragmas =
    [ "unpack"
    , "overlappable", "overlapping", "overlaps", "incoherent"]

groupExts :: [Code] -> ([Located String],[Code])
groupExts = foldr f ([],[])
  where
    f form (exts, invalids) =
      case form of
        LForm (L l (Atom (ASymbol sym)))
          | Just ext <- lookup sym supportedLangExts ->
            (L l ext:exts, invalids)
        _ -> (exts, form:invalids)

supportedLangExts :: [(FastString, String)]
supportedLangExts =
    f [ BangPatterns
      , DeriveDataTypeable
      , DeriveFoldable
      , DeriveFunctor
      , DeriveGeneric
      , DeriveTraversable
      , ExistentialQuantification
      , ExplicitForAll
      , FlexibleContexts
      , FlexibleInstances
      , GADTs
      , GeneralizedNewtypeDeriving
      , ImplicitPrelude
      , KindSignatures
      , MultiParamTypeClasses
      , MonoLocalBinds
      , MonomorphismRestriction
      , OverloadedStrings
      , OverloadedLists
      , RankNTypes
      , ScopedTypeVariables
      , TypeFamilies
      , TypeOperators
      , TypeSynonymInstances
      , UndecidableInstances ]
  where
    -- Adding `"No"' prefix, as done in `DynFlags.supportedExtensions'.
    -- Might worth looking up `DynFlags.xFlags' to get string
    -- representation of language extension instead of applying `show'
    -- function.
    f = concatMap g
    g ext = [(fsLit name, name), (fsLit noname, noname)]
      where
        name = show ext
        noname = "No" ++ name

makeOptionFlags :: [Code] -> [Located String]
makeOptionFlags = foldl' f []
  where
    f acc code =
      case code of
        LForm (L l (Atom (ASymbol sym))) -> L l (unpackFS sym) : acc
        _ -> acc

emptyBody :: SrcSpan -> Code
emptyBody l = li l [sym l "begin"]

dispatch :: Located Token -> Code -> SP Code
dispatch (L _ (TSymbol sym)) form =
  case sym of
    "." -> error "dispatch: dot"
    _   -> errorSP form "dispatch"

happyError :: SP a
happyError = lexErrorSP

-- | Parse sexpressions.
parseSexprs :: MonadFail m
            => Maybe FilePath -- ^ Name of input file.
            -> BL.ByteString  -- ^ Contents to parse.
            -> m ([Code], SPState)
parseSexprs mb_file contents =
  case runSP sexprs mb_file contents of
    Right a  -> return a
    Left err -> Control.Monad.Fail.fail err

-- | Parse single S-expression.
sexpr :: SP Code
sexpr = sexpr_

-- | Parse list of S-expressions.
sexprs :: SP [Code]
sexprs = sexprs_

-- | Partial S-expression parser.
psexpr :: SP Code
psexpr = psexpr_
}
