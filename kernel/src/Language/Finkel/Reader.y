-- -*- mode: haskell; -*-
{
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-- | S-expression reader.
--
-- Parser functions in this module are written with Happy parser generator.
--
module Language.Finkel.Reader
  ( parseSexprs
  , sexpr
  , sexprs
  , psexpr
  , supportedLangExts
  ) where

#include "Syntax.h"

-- base
import           Control.Monad.Fail     (MonadFail(..))
import           Data.Char              (toLower)
import           Data.List              (foldl')

-- ghc
import           BasicTypes             (SourceText(..))
import           FastString             (FastString, fsLit, unpackFS)
import           GHC_Hs_ImpExp          (ideclName)
import           Module                 (moduleNameString)
import           SrcLoc                 (GenLocated(..), Located, SrcSpan)
import           StringBuffer           (StringBuffer)

-- ghc-boot
import           GHC.LanguageExtensions (Extension(..))

-- Internal
import           Language.Finkel.Builder
import           Language.Finkel.Form
import           Language.Finkel.Lexer
import           Language.Finkel.Syntax
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
'%_'      { L _ (TPercent '_') }
'%'       { L _ (TPercent $$) }
'pcommas' { L _ (TPcommas _) }

'symbol'  { L _ (TSymbol _) }
'char'    { L _ (TChar _ _) }
'string'  { L _ (TString _ _) }
'integer' { L _ (TInteger _ _) }
'frac'    { L _ (TFractional _) }

'doc'     { L _ (TDocNext _) }
'doc^'    { L _ (TDocPrev _) }
'doch'    { L _ (TDocGroup _ _) }
'doc$'    { L _ (TDocNamed _ _) }

%%

-- Unit and List
-- ~~~~~~~~~~~~~
--
-- Empty list will parsed as a unit (i.e. '()' in Haskell), non-empty lists are
-- pased as 'List' value of 'Code'. Empty 'List' value of 'Code' could be
-- referred with 'Language.Finkel.Form.nil'.

sexp :: { Code }
    : atom                    { $1 }
    | 'quote' sexp            { mkQuote $1 $2 }
    | '`' sexp                { mkQuasiquote $1 $2 }
    | ',' sexp                { mkUnquote $1 $2 }
    | ',@' sexp               { mkUnquoteSplice $1 $2 }
    | '[' sexps ']'           { mkHsList $1 $2 }
    | 'pcommas'               { mkPcommas $1 }
    | '(' sexps ')'           { mkUnitOrList $1 $2}
    | '%' sexp                {% rmac $1 $2 }

sexps :: { [Code] }
    : rsexps { reverse $1 }

rsexps :: { [Code] }
    : {- empty -}      { [] }
    | rsexps sexp      { $2 : $1 }
    | rsexps '%_' sexp { $1 }

atom :: { Code }
    : 'symbol'  { mkASymbol $1 }
    | 'char'    { mkAChar $1 }
    | 'string'  { mkAString $1 }
    | 'integer' { mkAInteger $1 }
    | 'frac'    { mkAFractional $1 }
    | '{'       { mkOcSymbol $1 }
    | '}'       { mkCcSymbol $1 }
    | 'doc'     { mkDoc $1 }
    | 'doc^'    { mkDocp $1 }
    | 'doch'    { mkDoch $1 }
    | 'doc$'    { mkDock $1 }

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
mkQuote (L l _) body = li l [sym l ":quote", body]
{-# INLINE mkQuote #-}

mkQuasiquote :: Located Token -> Code -> Code
mkQuasiquote (L l _) body = li l [sym l ":quasiquote", body]
{-# INLINE mkQuasiquote #-}

mkUnquote :: Located Token -> Code -> Code
mkUnquote (L l _) body = li l [sym l ":unquote", body]
{-# INLINE mkUnquote #-}

mkUnquoteSplice :: Located Token -> Code -> Code
mkUnquoteSplice (L l _) body = li l [sym l ":unquote-splice", body]
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
mkAChar (L l (TChar st x)) = atom l $ AChar st x
{-# INLINE mkAChar #-}

mkAString :: Located Token -> Code
mkAString (L l (TString st x)) = atom l $ aString st x
{-# INLINE mkAString #-}

mkAInteger :: Located Token -> Code
mkAInteger (L l (TInteger st n)) = atom l $ AInteger lit
  where
    lit = IL { il_text = st
             , il_neg = n < 0
             , il_value = n }
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

mkDoc :: Located Token -> Code
mkDoc (L l (TDocNext str)) =
  li l [sym l ":doc", atom l (AString (SourceText (unpackFS str)) str)]
{-# INLINE mkDoc #-}

mkDocp :: Located Token -> Code
mkDocp (L l (TDocPrev str)) =
  li l [sym l ":doc^", atom l (AString (SourceText (unpackFS str)) str)]
{-# INLINE mkDocp #-}

mkDoch :: Located Token -> Code
mkDoch (L l (TDocGroup n s)) = li l [sym l dh, atom l (AString st s)]
  where
    dh = case n of
           1 -> ":dh1"
           2 -> ":dh2"
           3 -> ":dh3"
           _ -> ":dh4"
    st = SourceText (unpackFS s)
{-# INLINE mkDoch #-}

mkDock :: Located Token -> Code
mkDock (L l (TDocNamed k mb_doc)) =
  case mb_doc of
    Nothing -> li l pre
    Just d  -> li l (pre ++ [atom l (AString (SourceText (unpackFS d)) d)])
  where
    pre = [sym l ":doc$", atom l (ASymbol k)]
{-# INLINE mkDock #-}

rmac :: Char -> Code -> SP Code
rmac c expr =
  case c of
    'p' -> pragma expr
    _   -> errorSP expr ("rmac: unsupported char " ++ show c)
{-# INLINE rmac #-}

-- Module from ghc package with codes related to language pragma:
--
-- + libraries/ghc-boot-th/GHC/LanguageExtensions/Type.hs: The file containing
--   definition of language extensions.
--
-- + compiler/main/HeaderInfo.hs: Parses header information.
--
-- + compiler/main/DynFlags.hs: Contains 'supportedExtensions :: [String]'. This
--   is a list of language extension names, and the names with "No"
--   prefix. 'xFlagsDeps' contains list of pair language extension and
--   deprecation messages.
--

pragma :: Code -> SP Code
pragma orig@(LForm (L l form)) =
  case form of
    -- Pragma with no arguments.
    List [LForm (L _ (Atom (ASymbol sym)))]
      -- Return the UNPACK form as is. This pragma is handled by syntax parser
      -- of data constructor field.
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
    _ -> errorSP orig ("unknown pragma: " ++ show form)
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
      , DataKinds
      , DefaultSignatures
      , DeriveAnyClass
      , DeriveDataTypeable
      , DeriveFoldable
      , DeriveFunctor
      , DeriveGeneric
      , DeriveTraversable
      , DerivingStrategies
#if MIN_VERSION_ghc(8,4,0)
      , EmptyDataDeriving
#endif
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
      , OverloadedLabels
      , OverloadedLists
      , PolyKinds
      , RankNTypes
      , ScopedTypeVariables
      , StandaloneDeriving
      , TypeApplications
      , TypeFamilies
      , TypeInType
      , TypeOperators
      , TypeSynonymInstances
      , UndecidableInstances ]
  where
    -- Adding `"No"' prefix, as done in `DynFlags.supportedExtensions'.  Might
    -- worth looking up `DynFlags.xFlags' to get string representation of
    -- language extension instead of applying `show' function.
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
emptyBody l = li l [sym l ":begin"]

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
            -> StringBuffer   -- ^ Contents to parse.
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
