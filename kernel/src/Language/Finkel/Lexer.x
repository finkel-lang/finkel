-- -*- mode: haskell; -*-
{
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UnboxedTuples #-}
-- | Lexical analyser of S-expression tokens.
--
-- This file contains Alex lexical analyser for tokeninzing S-expression.
-- Lexcial analyser is used by Happy parser in S-expression reader.
module Language.Finkel.Lexer
  ( -- * Token data type
    Token(..)

    -- * Lexer function
  , tokenLexer
  , lexTokens

    -- * S-expression parser monad
  , SP(..)
  , SPState(..)
  , initialSPState
  , runSP
  , evalSP
  -- , incrSP
  , errorSP
  , lexErrorSP
  , putSPState
  , getSPState
  ) where

-- base
import           Control.Monad              (ap, liftM, msum)
import           Data.Char                  (GeneralCategory(..), chr,
                                             generalCategory, ord,
                                             isDigit, isOctDigit,
                                             isHexDigit, isSpace,
                                             toUpper)
import           Data.List                  (intercalate)
import           Data.Maybe                 (fromMaybe)
import           Data.Word                  (Word8)
import qualified GHC.Char                   as Char

-- bytestring
import qualified Data.ByteString            as W8
import qualified Data.ByteString.Char8      as C8

-- containers
import qualified Data.Map                   as Map

-- ghc
import           ApiAnnotation              (AnnotationComment(..))
import           BasicTypes                 (FractionalLit(..), SourceText(..))
import           Ctype                      (is_space)
import           Encoding                   (utf8DecodeByteString)
import           FastString                 (FastString,
                                             fsLit, headFS, nullFS,
                                             mkFastStringByteString,
                                             unpackFS)
import           Lexeme                     (startsConSym, startsVarSym)
import           SrcLoc                     (GenLocated(..), Located,
                                             RealSrcLoc, SrcLoc(..),
                                             SrcSpan(..), advanceSrcLoc,
                                             mkRealSrcLoc, mkRealSrcSpan,
                                             srcLocCol, srcLocLine)

import           StringBuffer               (StringBuffer, atEnd,
                                             byteDiff, cur, currentChar,
                                             lexemeToFastString,
                                             lexemeToString, nextChar,
                                             prevChar, stepOn)
import qualified StringBuffer               as SB
import           Util                       (readRational)

#if MIN_VERSION_ghc (8,10,0)
import           FastString                 (bytesFS)
#else
import           FastString                 (fastStringToByteString)
#endif

-- ghc-boot
import qualified GHC.LanguageExtensions     as LangExt

-- Internal
import           Language.Finkel.Builder
import           Language.Finkel.Form
}

$nl          = [\n\r\f]
$white       = [$nl\v\t\ ]
$white_no_nl = $white # $nl

$negative    = \-
$octit       = [0-7]
$digit       = [0-9]
$hexit       = [$digit A-F a-f]

$hsymhead    = [^\(\)\[\]\{\}\;\'\`\,\"\#$digit$white]
$hsymtail    = [$hsymhead\'\#$digit]
$hsymtail2   = $hsymtail # \'

@hsymbol     = $hsymhead $hsymtail*
@signed      = $negative?
@octal       = $octit+
@decimal     = $digit+
@hexadecimal = $hexit+
@exponent    = [eE] [\-\+]? @decimal
@frac        = @decimal \. @decimal @exponent? | @decimal @exponent

@contdoc     = $nl [\t\ ]* \;+ ~$nl+

tokens :-

$white+  ;

--- Comments

\;+ $white+ \^ $white_no_nl+ ~$nl+ @contdoc*          { tok_doc_prev }
\;+ $white+ \| $white_no_nl+ ~$nl+ @contdoc*          { tok_doc_next }
\;+ $white+ \*+ $white_no_nl+ .*                      { tok_doc_group }
\;+ $white+ \$ @hsymbol $white_no_nl* ~$nl* @contdoc* { tok_doc_named }

\; .*    { tok_line_comment }
\#\; .*  { tok_block_comment }

--- Character
\#\'           { tok_char }

--- Pragma
\% @hsymbol    { tok_hash }

--- Parenthesized commas, handled before parentheses
\( $white* \,+ $white* \) { tok_pcommas }

--- Parentheses
\( { tok_oparen }
\) { tok_cparen }

\[ { tok_obracket }
\] { tok_cbracket }

\{ { tok_ocurly }
\} { tok_ccurly }

-- Quote, unquote, quasiquote, and unquote splice
\'   { tok_quote }
\`   { tok_quasiquote }

\,\  { tok_comma }

\,\@ { tok_unquote_splice }
\,   { tok_unquote }

-- Lambda
\\ { tok_lambda }

--- Literal values
\"                         { tok_string }
@signed @decimal           { tok_integer }
@signed 0[oO] @octal       { tok_integer }
@signed 0[xX] @hexadecimal { tok_integer }
@signed @frac              { tok_fractional }

-- Symbols
\# $hsymtail2* { tok_symbol }
@hsymbol       { tok_symbol }

{
-- ---------------------------------------------------------------------
--
-- Parser monad
--
-- ---------------------------------------------------------------------

-- | Data type to hold states while reading source code.
data SPState = SPState
  { -- | Target file for lexical analysis.
    targetFile :: FastString
    -- | @{-# LANGUAGE ... #-}@ found in target file.
  , langExts :: [Located String]
    -- | @{-# GHC_OPTIONS ... #-}@ found in target file.
  , ghcOptions :: [Located String]
    -- | Buffer to hold current input.
  , buf :: StringBuffer
    -- | Current location of input stream.
  , currentLoc :: RealSrcLoc
  }

-- | Initial empty state for 'SP'.
initialSPState :: FastString -> Int -> Int -> SPState
initialSPState file linum colnum =
  SPState { targetFile = file
          , langExts = []
          , ghcOptions = []
          , buf = error "SPState.buf not initialized"
          , currentLoc = mkRealSrcLoc file linum colnum
          }

data SPResult a
  = SPOK {-# UNPACK #-} !SPState a
  | SPNG SrcLoc String

-- | A state monad newtype to pass around 'SPstate'.
newtype SP a = SP { unSP :: SPState -> SPResult a }

instance Functor SP where
  fmap = liftM
  {-# INLINE fmap #-}

instance Applicative SP where
  pure a = SP (\st -> SPOK st a)
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Monad SP where
  return a = SP (\st -> SPOK st a)
  {-# INLINE return #-}
  m >>= k = SP (\st -> case unSP m st of
                   SPOK st' a -> unSP (k a) st'
                   SPNG l msg -> SPNG l msg)
  {-# INLINE (>>=) #-}

data AlexInput = AlexInput RealSrcLoc StringBuffer

-- | Perform given 'SP' computation with target file name and input contents.
runSP :: SP a           -- ^ Computation to perform.
      -> Maybe FilePath -- ^ File name of target. If 'Nothing', assumed as
                        -- anonymous target.
      -> StringBuffer   -- ^ Input contents.
      -> Either String (a, SPState)
runSP sp target input =
  let st0 = initialSPState target' 1 1
      st1 = st0 {buf = input}
      target' = maybe (fsLit "anon") fsLit target
  in  case unSP sp st1 of
        SPOK sp' a -> Right (a, sp')
        SPNG _loc msg -> Left msg

-- | Like 'runSP', but discard resulting 'SPState'.
evalSP :: SP a -> Maybe FilePath -> StringBuffer -> Either String a
evalSP sp target input = fmap fst (runSP sp target input)

-- | Update current 'SPState' to given value.
putSPState :: SPState -> SP ()
putSPState st = SP (\_ -> SPOK st ())
{-# INLINE putSPState #-}

-- | Get current 'SPState' value.
getSPState :: SP SPState
getSPState = SP (\st -> SPOK st st)
{-# INLINE getSPState #-}

-- -- | Incrementally perform computation with parsed result and given
-- -- function.
-- incrSP :: SP a           -- ^ The partial parser.
--        -> (a -> b -> b)  -- ^ Function to apply.
--        -> b              -- ^ Initial argument to the function.
--        -> Maybe FilePath -- ^ Filepath of the input.
--        -> StringBuffer   -- ^ Input contents.
--        -> Either String (b, SPState)
-- incrSP sp f z target input = go st1 z
--   where
--     go st acc =
--       case unSP sp st of
--         SPNG _loc msg
--           | atEnd (buf st) -> Right (acc, st)
--           | otherwise      -> Left msg
--         SPOK st' ret       ->
--           -- let st'' = st' {buf=C8.cons (prevChar st') (buf st')}
--           let st'' = st' {} -- ... efficient way to cons prev char?
--           in  go st'' $! f ret acc
--     st0 = initialSPState target' 1 1
--     st1 = st0 {buf = input}
--     target' = maybe (fsLit "anon") fsLit target

-- | Show alex error with location of given 'Code' and error message.
errorSP :: Code   -- ^ Code for showing location information.
        -> String -- ^ Error message to show.
        -> SP a
errorSP code msg = alexError (showLoc code ++ msg)

-- | Show error message with current input.
lexErrorSP :: SP a
lexErrorSP = do
  st <- getSPState
  AlexInput loc buf <- alexGetInput
  let lno = srcLocLine loc
      cno = srcLocCol loc
      trg = unpackFS (targetFile st)
      c = prevChar buf '\n'
      msg = trg ++ ": lexer error at line " ++ show lno ++
            ", column " ++ show cno ++
            ", near " ++ show c
  alexError msg

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (AlexInput loc0 buf0) =
  if atEnd buf0
     then Nothing
     else case nextChar buf0 of
            (c, buf1) -> let w = adjustChar c
                             loc1 = advanceSrcLoc loc0 c
                         in  w `seq` loc1 `seq` buf1 `seq`
                             Just (w, AlexInput loc1 buf1)
{-# INLINE alexGetByte #-}

alexGetChar :: AlexInput -> Maybe (Char, AlexInput)
alexGetChar (AlexInput loc0 buf0) =
  if atEnd buf0
     then Nothing
     else case nextChar buf0 of
            (c, buf1) -> let loc1 = advanceSrcLoc loc0 c
                         in  c `seq` loc1 `seq` buf1 `seq`
                             Just (c, AlexInput loc1 buf1)
{-# INLINE alexGetChar #-}

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (AlexInput _ buf) = prevChar buf '\NUL'
{-# INLINE alexInputPrevChar #-}

alexError :: String -> SP a
alexError msg = SP (\st -> SPNG (RealSrcLoc (currentLoc st)) msg)
{-# INLINE alexError #-}

alexGetInput :: SP AlexInput
alexGetInput =
  SP (\st@SPState {currentLoc=l,buf=b} -> SPOK st (AlexInput l b))
{-# INLINE alexGetInput #-}

alexSetInput :: AlexInput -> SP ()
alexSetInput (AlexInput l b) =
  SP (\st -> SPOK (st {buf=b,currentLoc=l}) ())
{-# INLINE alexSetInput #-}


-- ---------------------------------------------------------------------
--
-- Token data and actions
--
-- ---------------------------------------------------------------------

-- | Data type for token.
data Token
  = TOparen
  -- ^ Open parenthesis.
  | TCparen
  -- ^ Close parenthesis.
  | TObracket
  -- ^ Open bracket.
  | TCbracket
  -- ^ Close bracket.
  | TOcurly
  -- ^ Open curly.
  | TCcurly
  -- ^ Close curly.
  | TQuote
  -- ^ Quote.
  | TQuasiquote
  -- ^ Quasi-quote.
  | TUnquote
  -- ^ Unquote.
  | TUnquoteSplice
  -- ^ Unquote-splice.
  | TComment
  -- ^ Comment.
  | TSymbol FastString
  -- ^ Symbol data.
  | TChar SourceText Char
  -- ^ Character data.
  | TString SourceText String
  -- ^ Literal string data.
  | TInteger SourceText Integer
  -- ^ Literal integer number.
  | TFractional FractionalLit
  -- ^ Literal fractional number.
  | THash Char
  -- ^ Literal @#@.
  | TPcommas Int
  -- ^ Parenthesized commas with number of repeats.
  | TDocNext FastString
  -- ^ Documentation comment for next thing.
  | TDocPrev FastString
  -- ^ Documentation comment for previous thing.
  | TDocGroup Int FastString
  -- ^ Documentation comment for section.
  | TDocNamed FastString (Maybe FastString)
  -- ^ Documentation comment for named documentation.
  | TEOF
  -- ^ End of form.
  deriving (Eq, Show)

type Action = AlexInput -> Int -> SP Token

-- Tokenizer actions for documentation
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Currently, documentation comment starting with `;'s are converted to 'Token'
-- data during lexical analysis. Once the documentation string were converted to
-- 'Token' data type, it cannot distinguish between block documentation comment
-- and line documentation comment. From this reason, the documentation comments
-- generated with "Language.Finkel.Emit" are always using single-line comment
-- syntax.

tok_oparen :: Action
tok_oparen _ _ = return TOparen
{-# INLINE tok_oparen #-}

tok_cparen :: Action
tok_cparen _ _ = return TCparen
{-# INLINE tok_cparen #-}

tok_obracket :: Action
tok_obracket _ _ = return TObracket
{-# INLINE tok_obracket #-}

tok_cbracket :: Action
tok_cbracket _ _ = return TCbracket
{-# INLINE tok_cbracket #-}

tok_ocurly :: Action
tok_ocurly _ _ = return TOcurly
{-# INLINE tok_ocurly #-}

tok_ccurly :: Action
tok_ccurly _ _ = return TCcurly
{-# INLINE tok_ccurly #-}

tok_quote :: Action
tok_quote _ _ = return TQuote
{-# INLINE tok_quote #-}

tok_quasiquote :: Action
tok_quasiquote _ _ = return TQuasiquote
{-# INLINE tok_quasiquote #-}

tok_pcommas :: Action
tok_pcommas (AlexInput _ buf) l =
  do let commas0 = lexemeToFastString buf (fromIntegral l)
         commas1 = bytesFS commas0
         commas2 = C8.filter (not . isSpace) commas1
     return $! TPcommas (fromIntegral (C8.length commas1 - 2))
{-# INLINE tok_pcommas #-}

tok_comma :: Action
tok_comma _ _ = return $ TSymbol $! fsLit ","
{-# INLINE tok_comma #-}

tok_unquote :: Action
tok_unquote _ _ = return TUnquote
{-# INLINE tok_unquote #-}

tok_unquote_splice :: Action
tok_unquote_splice _ _ = return TUnquoteSplice
{-# INLINE tok_unquote_splice #-}

tok_hash :: Action
tok_hash (AlexInput _ buf) l
  | l == 2
  , let c = currentChar (snd (nextChar buf))
  , not (startsVarSym c)
  , not (startsConSym c)
  = return $! THash c
  | otherwise
  = let fs = lexemeToFastString buf l
    in  fs `seq` return $! TSymbol fs
{-# INLINE tok_hash #-}

tok_line_comment :: Action
tok_line_comment _ _ = return TComment
{-# INLINE tok_line_comment #-}

tok_block_comment :: Action
tok_block_comment = tok_block_comment_with (const TComment) alexGetChar
{-# INLINE tok_block_comment #-}

tok_block_comment_with :: (String -> Token)
                       -> (AlexInput -> Maybe (Char, AlexInput))
                       -> Action
tok_block_comment_with tok ini inp0 _ = do
  case alexGetChar inp0 of
    Just ('#', inp1)
      | Just (';', inp2) <- alexGetChar inp1
      , Just (c, inp3) <- ini inp2
      , Just (com, inp4) <- go inp3 c ""
      -> alexSetInput inp4 >> return (tok (reverse com))
    _ -> alexError "tok_block_comment: panic"
  where
    go inp prev acc =
      case alexGetChar inp of
        Just (c, inp') | prev == ';', c == '#' -> Just (tail acc, inp')
                       | otherwise             -> go inp' c (c:acc)
        Nothing                                -> Nothing
{-# INLINE tok_block_comment_with #-}

tok_doc_prev :: Action
tok_doc_prev = tok_doc_with TDocPrev '^'
{-# INLINE tok_doc_prev #-}

tok_doc_next :: Action
tok_doc_next = tok_doc_with TDocNext '|'
{-# INLINE tok_doc_next #-}

tok_doc_with :: (FastString -> Token) -> Char -> Action
tok_doc_with constr char (AlexInput _ s) l = do
  let fs0 = takeUtf8FS l s
      bs0 = bytesFS fs0
      line0:bss = C8.lines bs0
      line1 = C8.tail (C8.dropWhile (/= char) line0)
      bs1 = C8.unlines (line1 : map (C8.dropWhile (== ';')) bss)
      fs1 = mkFastStringByteString bs1
  return $! constr fs1
{-# INLINE tok_doc_with #-}

tok_doc_named :: Action
tok_doc_named (AlexInput _ s) l =
  let fs0 = takeUtf8FS l s
      bs0 = bytesFS fs0
      line1:bss = C8.lines bs0
      line2 = C8.dropWhile isSpace (C8.dropWhile (== ';') line1)
      line3 = C8.tail line2
      key = mkFastStringByteString line3
      bs1 = map (C8.dropWhile (== ';')) bss
      fs1 = mkFastStringByteString (C8.unlines bs1)
      fs2 = case bss of
              [] -> Nothing
              _  -> Just fs1
  in  return $! TDocNamed key fs2
{-# INLINE tok_doc_named #-}

tok_doc_group :: Action
tok_doc_group (AlexInput _ s) l =
  let bs0 = bytesFS (takeUtf8FS l s)
      bs1 = C8.dropWhile isSpace (C8.dropWhile (== ';') bs0)
      (stars, bs2) = C8.span (== '*') bs1
      level = C8.length stars
      fs0 = mkFastStringByteString (C8.tail bs2)
  in  return $! TDocGroup level fs0
{-# INLINE tok_doc_group #-}

tok_lambda :: Action
tok_lambda _ _ = return $ TSymbol $! fsLit "\\"
{-# INLINE tok_lambda #-}

-- | Make token symbol.  When the given symbol starts with non-operatator
-- character, replace hyphens with underscores.
tok_symbol :: Action
tok_symbol (AlexInput _ buf) l =
  let fs0 = takeUtf8FS l buf
      fs1 | startsVarSym c || startsConSym c = fs0
          | otherwise = replaceHyphens fs0
          where c = currentChar buf
  in  fs0 `seq` fs1 `seq` return $! TSymbol fs1
{-# INLINE tok_symbol #-}

replaceHyphens :: FastString -> FastString
replaceHyphens =
  mkFastStringByteString .
  C8.map (\c -> if c == '-' then '_' else c) .
  bytesFS
{-# INLINE replaceHyphens #-}

tok_char :: Action
tok_char inp0 _ = do
  case alexGetChar inp0 of
    Just ('#', inp1) -> go0 inp1
    _                -> alexError "tok_char: panic"
  where
    go0 inp =
      case alexGetChar inp of
        Just ('\'', inp') -> go1 inp'
        _                 -> alexError "tok_char.go0: panic"
    go1 inp
      | Just (c, inp') <- alexGetChar inp =
        case c of
          '\\' -> case escapeChar inp' of
            Just (st, c', inp'') ->
              do alexSetInput inp''
                 return $! TChar st c'
            Nothing ->
              do alexSetInput inp'
                 return $! TChar (SourceText "'\\\\'") '\\'
          _    ->
            do alexSetInput inp'
               let st | c == '\'' = '\'' : '\\' : c : "'"
                      | otherwise = '\'' : c : "'"
               return $! TChar (SourceText st) c
      | otherwise = alexError "tok_char.go1: panic"
{-# INLINE tok_char #-}

tok_string :: Action
tok_string inp@(AlexInput _ buf) _l =
  -- Currently String tokenizer does not update alex input per character. This
  -- makes the code a bit more effiicient, but getting unhelpful error message
  -- on illegal escape sequence.
  case alexGetChar inp of
    Just ('"', inp1)
      | Just (TString _ str, inp2@(AlexInput _ buf2)) <- go inp1 "" ->
        -- Refill the source text with string extracted with updated buffer
        -- location.
        do alexSetInput inp2
           let src = lexemeToString buf (cur buf2 - cur buf)
           return $! TString (SourceText src) str
    _ -> lexErrorSP
  where
    go inp0 acc =
      case alexGetChar inp0 of
        Nothing -> Nothing
        Just (c1, inp1)
          | c1 == '"'  -> do
            let acc' = reverse acc
            return $! (TString NoSourceText acc', inp1)
          | c1 == '\\' ->
            case escapeChar inp1 of
              Just (_st, c1, inp2) -> go inp2 $! (c1:acc)
              _                    ->
                case alexGetChar inp1 of
                  Just (c2, inp2)
                    | c2 == '&'    -> go inp2 $! acc
                    | is_space' c2 -> string_gap inp2 acc
                  _                           -> Nothing
          | otherwise  -> go inp1 $! (c1:acc)
    string_gap inp0 acc =
      case alexGetChar inp0 of
        Just (c, inp1)
          | c == '\\'   -> go inp1 acc
          | is_space' c -> string_gap inp1 acc
        _ -> Nothing
{-# INLINE tok_string #-}

-- See "lex_stringgap" in "compiler/parser/Lexer.x".
is_space' :: Char -> Bool
is_space' c = c <= '\x7f' && is_space c
{-# INLINE is_space' #-}

escapeChar :: AlexInput -> Maybe (SourceText, Char, AlexInput)
escapeChar inp0
  | Just (c1, inp1) <- alexGetChar inp0 =
    let ret x = Just $! (SourceText (show x), x, inp1)
        esc str = SourceText ('\'':'\\':str)
        numericChar test acc0 f =
          let lp inp acc =
                case alexGetChar inp of
                  Just (c2, inp')
                    | test c2 -> lp inp' (c2:acc)
                    | otherwise ->
                      let acc' = reverse acc
                      in  Just (esc (acc'++"'"), Char.chr (read (f acc')), inp)
                  Nothing -> Nothing
          in lp inp1 acc0
        controlChar
          | Just (c2, inp2) <- alexGetChar inp1
          , c2 >= '@' && c2 <= '_' =
            Just (esc ('^':c2:"'"), chr (ord c2 - ord '@'), inp2)
          | otherwise = Nothing
        lkup cs = lookup (C8.pack cs)
        bstbl = map (\(str,c) -> (C8.pack str, c))
        tbl2 = bstbl tbl2_str
        tbl2_str =
          [ ("BS", '\BS'), ("HT", '\HT'), ("LF", '\LF'), ("VT", '\VT')
          , ("FF", '\FF'), ("CR", '\CR'), ("SO", '\SO'), ("SI", '\SI')
          , ("EM", '\EM'), ("FS", '\FS'), ("GS", '\GS'), ("RS", '\RS')
          , ("US", '\US'), ("SP", '\SP') ]
        tbl3 = bstbl tbl3_str
        tbl3_str =
          [ ("NUL", '\NUL'), ("SOH", '\SOH'), ("STX", '\STX')
          , ("ETX", '\ETX'), ("EOT", '\EOT'), ("ENQ", '\ENQ')
          , ("ACK", '\ACK'), ("BEL", '\BEL'), ("DLE", '\DLE')
          , ("DC1", '\DC1'), ("DC2", '\DC2'), ("DC3", '\DC3')
          , ("DC4", '\DC4'), ("NAK", '\NAK'), ("SYN", '\SYN')
          , ("ETB", '\ETB'), ("CAN", '\CAN'), ("SUB", '\SUB')
          , ("ESC", '\ESC') , ("DEL", '\DEL') ]
    in  case c1 of
          'a' -> ret '\a'
          'b' -> ret '\b'
          'f' -> ret '\f'
          'n' -> ret '\n'
          'r' -> ret '\r'
          't' -> ret '\t'
          'v' -> ret '\v'
          '"' -> ret '\"'
          '\'' -> ret '\''
          '\\' -> ret '\\'
          '^' -> controlChar
          'x' -> numericChar isHexDigit [c1] ('0':)
          'o' -> numericChar isOctDigit [c1] ('0':)
          _ | isDigit c1 -> numericChar isDigit [c1] id
            | Just (c2, inp2) <- alexGetChar inp1
            , Just (c3, inp3) <- alexGetChar inp2
            -> case lkup [c1,c2,c3] tbl3 of
                 Just c  -> Just (esc [c1,c2,c3,'\''], c, inp3)
                 Nothing
                   | Just c <- lkup [c1,c2] tbl2 ->
                     Just (esc [c1,c2,'\''], c, inp2)
                 _ -> Nothing
            | otherwise -> Nothing
  | otherwise = Nothing
{-# INLINE escapeChar #-}

tok_integer :: Action
tok_integer (AlexInput _ buf) l =
  let str = lexemeToString buf (fromIntegral l)
  in  return $ TInteger (SourceText str) $! read $! str
{-# INLINE tok_integer #-}

tok_fractional :: Action
tok_fractional (AlexInput _ buf) l =
  do let str = lexemeToString buf (fromIntegral l)
         rat = readRational str
#if MIN_VERSION_ghc(8,4,0)
     let stxt = SourceText str
         is_neg = if 0 < rat
                     then False
                     else True
     return $! TFractional $! FL stxt is_neg rat
#else
     return $! TFractional $! FL str rat
#endif
{-# INLINE tok_fractional #-}


-- ---------------------------------------------------------------------
--
-- Lexer
--
-- ---------------------------------------------------------------------

-- | Lexical analyzer for S-expression. Intended to be used with a parser made
-- from Happy. This functions will not pass comment tokens to continuation.
tokenLexer :: (Located Token -> SP a) -> SP a
tokenLexer cont = do
  ltok@(L _span tok) <- scanToken
  case tok of
    TComment -> SP (\st -> unSP (tokenLexer cont) st)
    _        -> cont ltok
{-# INLINE tokenLexer #-}

scanToken :: SP (Located Token)
scanToken = do
  inp0@(AlexInput loc0 _) <- alexGetInput
  let sc = 0
  case alexScan inp0 sc of
    AlexToken inp1 len act -> do
      alexSetInput inp1
      tok <- act inp0 len
      -- Getting current location again after invoking 'act', to update
      -- location information of String tokens.
      loc1 <- fmap currentLoc getSPState
      let span = RealSrcSpan $ mkRealSrcSpan loc0 loc1
      return (L span tok)
    AlexError (AlexInput loc1 buf) -> do
      sp <- getSPState
      let l = srcLocLine loc1
          c = srcLocCol loc1
          trg = unpackFS (targetFile sp)
      alexError (trg ++ ": lexical error at line " ++ show l ++
                 ", column " ++ show c ++
                 ", near " ++ show (currentChar buf))
    AlexSkip inp1 _ -> do
      alexSetInput inp1
      scanToken
    AlexEOF -> return (L undefined TEOF)
{-# INLINE scanToken #-}

-- | Lex the input to list of 'Token's.
lexTokens :: Maybe FilePath
          -> StringBuffer
          -> Either String [Located Token]
lexTokens = evalSP go
  where
     go = do
       tok <- tokenLexer return
       case tok of
         L _ TEOF -> return []
         _        -> (tok :) <$> go


-- ---------------------------------------------------------------------
--
-- Auxiliary
--
-- ---------------------------------------------------------------------

takeUtf8 :: Int -> StringBuffer -> String
takeUtf8 = go []
  where
    go acc n buf =
      if n == 0
         then reverse acc
         else case nextChar buf of
                (c, buf') -> let acc' = c: acc
                                 n' = n - 1
                             in  acc' `seq` n' `seq` go acc' n' buf'
{-# INLINE takeUtf8 #-}

takeUtf8FS :: Int -> StringBuffer -> FastString
takeUtf8FS n sb0 = lexemeToFastString sb0 diff
  where
    diff = byteDiff sb0 (step n sb0)
    step i sb =
      if i == 0
         then sb
         else let i' = i -1
                  sb' = stepOn sb
              in  i' `seq` sb' `seq` step i' sb'
{-# INLINE takeUtf8FS #-}

-- Taken from "compiler/parser/Lexer.x.source" ghc source.
adjustChar :: Char -> Word8
adjustChar c = fromIntegral $ ord adj_c
  where non_graphic     = '\x00'
        upper           = '\x01'
        lower           = '\x02'
        digit           = '\x03'
        symbol          = '\x04'
        space           = '\x05'
        other_graphic   = '\x06'
        uniidchar       = '\x07'

        adj_c
          | c <= '\x07' = non_graphic
          | c <= '\x7f' = c
          -- Alex doesn't handle Unicode, so when Unicode character is
          -- encountered we output these values with the actual character value
          -- hidden in the state.
          | otherwise =
                -- NB: The logic behind these definitions is also reflected in
                -- basicTypes/Lexeme.hs Any changes here should likely be
                -- reflected there.
                case generalCategory c of
                  UppercaseLetter       -> upper
                  LowercaseLetter       -> lower
                  TitlecaseLetter       -> upper
                  ModifierLetter        -> uniidchar -- see #10196
                  OtherLetter           -> lower -- see #1103
                  NonSpacingMark        -> uniidchar -- see #7650
                  SpacingCombiningMark  -> other_graphic
                  EnclosingMark         -> other_graphic
                  DecimalNumber         -> digit
                  LetterNumber          -> other_graphic
                  OtherNumber           -> digit -- see #4373
                  ConnectorPunctuation  -> symbol
                  DashPunctuation       -> symbol
                  OpenPunctuation       -> other_graphic
                  ClosePunctuation      -> other_graphic
                  InitialQuote          -> other_graphic
                  FinalQuote            -> other_graphic
                  OtherPunctuation      -> symbol
                  MathSymbol            -> symbol
                  CurrencySymbol        -> symbol
                  ModifierSymbol        -> symbol
                  OtherSymbol           -> symbol
                  Space                 -> space
                  _other                -> non_graphic
{-# INLINE adjustChar #-}

#if !MIN_VERSION_ghc(8,10,0)
-- | 'fastStringToByteString' is deprecated in ghc-8.10.x.
bytesFS :: FastString -> W8.ByteString
bytesFS = fastStringToByteString
#endif

}
