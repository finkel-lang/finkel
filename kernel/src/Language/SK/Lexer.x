-- S-expression lexer with alex
{
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-- | Lexical analyser of S-expression tokens.
module Language.SK.Lexer
  ( -- * Token data type
    Token(..)
  , LToken
    -- * Lexer function
  , tokenLexer
  , lexTokens
    -- * Documentation map
  , DocMap
    -- * S-expression parser monad
  , SP(..)
  , SPState(..)
  , initialSPState
  , runSP
  , evalSP
  , incrSP
  , errorSP
  , lexErrorSP
  , putSPState
  , getSPState
  ) where

-- base
import Control.Monad (ap, liftM, msum)
import Data.Char ( chr, ord, isDigit, isOctDigit, isHexDigit
                 , isSpace, toUpper )
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import qualified GHC.Char as Char

-- bytestring
import Data.ByteString.Internal (w2c)
import qualified Data.ByteString.Lazy as W8
import qualified Data.ByteString.Lazy.Char8 as C8

-- containers
import qualified Data.Map as Map

-- ghc
import ApiAnnotation (AnnotationComment(..))
import BasicTypes ( FractionalLit(..)
#if MIN_VERSION_ghc(8,4,0)
                  , SourceText(..)
#endif
                  )
import Encoding (utf8DecodeByteString)
import FastString (FastString, mkFastStringByteString, unpackFS)
import SrcLoc ( Located, RealSrcLoc, advanceSrcLoc, mkRealSrcLoc
              , mkRealSrcSpan, srcLocCol, srcLocLine )
import Util (readRational)

-- ghc-boot
import qualified GHC.LanguageExtensions as LangExt

-- transformers
import Control.Monad.Trans.Except (ExceptT(..), throwE)

-- Internal
import Language.SK.Builder
import Language.SK.Form

}

$nl          = [\n\r\f]
$whitechar   = [$nl\v\ ]
$white_no_nl = $whitechar # \n

$negative    = \-
$octit       = [0-7]
$digit       = [0-9]
$hexit       = [$digit A-F a-f]

$hsymhead    = [^\\\(\)\[\]\{\}\;\'\`\,\"\#$digit$white]
$hsymtail    = [$hsymhead\'\#$digit]

@hsymbol     = $hsymhead $hsymtail*
@signed      = $negative ?
@octal       = $octit+
@decimal     = $digit+
@hexadecimal = $hexit+
@exponent    = [eE] [\-\+]? @decimal
@frac        = @decimal \. @decimal @exponent ? | @decimal @exponent

-- Continued doc comment line, this starts with \n from previous line.
@contdoc     = \n [\t\ ]* \;+ ~\n+


tokens :-

$whitechar+  ;

--- Comments

\;+ $whitechar+ \| ~\n+ @contdoc* { tok_doc_comment_next }
\; .*                             { tok_line_comment }

\#\| $whitechar* \|   { tok_block_doc_comment_next }
\#\|                  { tok_block_comment }

--- Hashes
\# $hsymtail* { tok_hash }

--- Parenthesized commas, handled before parentheses
\( $whitechar* \,+ $whitechar* \) { tok_pcommas }

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
\\\  { tok_lambda }

--- Literal values
\\                         { tok_char }
\"                         { tok_string }
@signed @decimal           { tok_integer }
@signed 0[oO] @octal       { tok_integer }
@signed 0[xX] @hexadecimal { tok_integer }
@signed @frac              { tok_fractional }

-- Symbols
@hsymbol { tok_symbol }

{
-- ---------------------------------------------------------------------
--
-- Parser monad
--
-- ---------------------------------------------------------------------

-- | Documentation map.
type DocMap = Map.Map SrcSpan [AnnotationComment]

-- | Data type to hold states while reading source code.
data SPState = SPState
  { comments :: [Located AnnotationComment]
  , targetFile :: FastString
  , langExts :: [Located String]
  , ghcOptions :: [Located String]
  , docMap :: DocMap
  , buf :: C8.ByteString
  , currentLoc :: RealSrcLoc
  , prevChar :: Char
  } deriving (Eq)

-- | Initial empty state for 'SP'.
initialSPState :: FastString -> Int -> Int -> SPState
initialSPState file linum colnum =
  SPState { comments = []
          , targetFile = file
          , langExts = []
          , ghcOptions = []
          , docMap = Map.empty
          , buf = C8.empty
          , currentLoc = mkRealSrcLoc file linum colnum
          , prevChar = '\n'}

data SPResult a
  = SPOK SPState a
  | SPNG SrcLoc String
  deriving (Eq)

-- | A data type for State monad which wraps 'Alex' with 'SPstate'.
newtype SP a = SP { unSP :: SPState -> SPResult a }

instance Functor SP where
  fmap = liftM
  {-# INLINE fmap #-}

instance Applicative SP where
  pure = return
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

data AlexInput =
  AlexInput RealSrcLoc
            {-# UNPACK #-} !Char
            C8.ByteString
  deriving (Eq, Show)

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (AlexInput loc _ buf) =
  case W8.uncons buf of
    Nothing -> Nothing
    Just (w8, buf') ->
      let c = w2c w8
          loc' = advanceSrcLoc loc c
      in w8 `seq` c `seq` loc' `seq` buf' `seq`
         Just (w8, AlexInput loc' c buf')
{-# INLINE alexGetByte #-}

alexGetChar' :: AlexInput -> Maybe (Char, AlexInput)
alexGetChar' (AlexInput l0 _c0 bs0) =
  case W8.uncons bs0 of
    Just (w1, bs1)
      | w1 < 0x80 ->
        let c = w2c w1
            l1 = advanceSrcLoc l0 c
        in Just (c, AlexInput l1 c bs1)
      | w1 < 0xe0 -> split 1
      | w1 < 0xf0 -> split 2
      | otherwise -> split 3
      where
        split n =
          let (rest, bs2) = W8.splitAt n bs1
              c = utf8 w1 rest
              l1 = advanceSrcLoc l0 c
          in  Just (c, AlexInput l1 c bs2)
        utf8 w ws =
          head (utf8DecodeByteString (W8.toStrict (W8.cons w ws)))
    Nothing -> Nothing
{-# INLINE alexGetChar' #-}

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (AlexInput _ c _) = c
{-# INLINE alexInputPrevChar #-}

alexError :: String -> SP a
alexError msg = SP (\st -> SPNG (RealSrcLoc (currentLoc st)) msg)

alexGetInput :: SP AlexInput
alexGetInput =
  SP (\st@SPState{currentLoc=l,buf=b,prevChar=c} ->
        SPOK st (AlexInput l c b))
{-# INLINE alexGetInput #-}

alexSetInput :: AlexInput -> SP ()
alexSetInput (AlexInput l c b) =
  SP (\st -> SPOK (st {buf=b,currentLoc=l,prevChar=c}) ())
{-# INLINE alexSetInput #-}

runSP :: SP a -> Maybe FilePath -> C8.ByteString
      -> Either String (a, SPState)
runSP sp target input =
  let st0 = initialSPState target' 1 1
      st1 = st0 {buf = input}
      target' = maybe (fsLit "anon") fsLit target
  in  case unSP sp st1 of
        SPOK sp' a -> Right (a, sp')
        SPNG _loc msg -> Left msg

-- | Incrementally perform computation with parsed result and given
-- function.
incrSP :: SP a          -- ^ The partial parser.
       -> (a -> b -> b) -- ^ Function to apply.
       -> b             -- ^ Initial argument to the function.
       -> Maybe FilePath -> C8.ByteString -> Either String (b, SPState)
incrSP sp f z target input = go st1 z
  where
    go st acc =
      case unSP sp st of
        SPNG _loc msg
          | blank (buf st) -> Right (acc, st)
          | otherwise      -> Left msg
        SPOK st' ret       ->
          let st'' = st' {buf=C8.cons (prevChar st') (buf st')}
          in  go st'' $! f ret acc
    blank bl = C8.null bl || C8.all (`elem` "\n\r\t") bl
    st0 = initialSPState target' 1 1
    st1 = st0 {buf = input}
    target' = maybe (fsLit "anon") fsLit target

evalSP :: SP a -> Maybe FilePath -> C8.ByteString -> Either String a
evalSP sp target input = fmap fst (runSP sp target input)

errorSP :: Code -> String -> SP a
errorSP code msg = alexError (showLoc code ++ msg)

lexErrorSP :: SP a
lexErrorSP = do
  st <- getSPState
  AlexInput loc c _ <- alexGetInput
  let lno = srcLocLine loc
      cno = srcLocCol loc
      trg = unpackFS (targetFile st)
      msg = trg ++ ": lexer error at line " ++ show lno ++
            ", column " ++ show cno ++
            ", near " ++ show c
  alexError msg

putSPState :: SPState -> SP ()
putSPState st = SP (\_ -> SPOK st ())
{-# INLINE putSPState #-}

getSPState :: SP SPState
getSPState = SP (\st -> SPOK st st)
{-# INLINE getSPState #-}


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
  | TDocCommentNext String
  -- ^ Comment string starting with @-- |@.
  | TLineComment String
  -- ^ Non-documentation line comment string.
  | TBlockComment String
  -- ^ Non-documentation block comment string.
  | TBlockDocCommentNext String
  -- ^ Block documentation comment for next declaration.
  | TSymbol FastString
  -- ^ Symbol data.
  | TChar Char
  -- ^ Character data.
  | TString String
  -- ^ Literal string data.
  | TInteger Integer
  -- ^ Literal integer number.
  | TFractional FractionalLit
  -- ^ Literal fractional number.
  | THash Char
  -- ^ Literal @#@.
  | TPcommas Int
  -- ^ Parenthesized commas with number of repeats.
  | TEOF
  -- ^ End of form.
  deriving (Eq, Show)

type LToken = Located Token

type Action = AlexInput -> Int -> SP Token

-- Tokenizer actions for documentation
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Currently, documentation 'Token's are converted to 'AnnotatedComment'
-- during lexical analysis. Once Token data were converted to
-- 'AnnotationComment', it cannot distinguish between block
-- documentation comment and line documentation comment. From this
-- reason, the documentation comments generated with "Language.SK.Emit"
-- are always using multi-line comment syntax.

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
tok_pcommas (AlexInput _ _ s) l = do
  let cs0 = C8.take (fromIntegral l) s
      cs1 = C8.filter (not . isSpace) cs0
  return $! TPcommas (fromIntegral (C8.length cs1 - 2))
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
tok_hash (AlexInput _ _ s) l
  | l == 2, let c = C8.index s 1, c `notElem` haskellOpChars
  = return $! THash c
  | otherwise =
   let bs = C8.toStrict $! takeUtf8 (fromIntegral l) s
   in  return $! TSymbol $! mkFastStringByteString bs
{-# INLINE tok_hash #-}

tok_doc_comment_next :: Action
tok_doc_comment_next (AlexInput _ _ s) l = do
  let str = unpackUtf8 $! takeUtf8 l s
  return $! TDocCommentNext $! lc2hc_doc str
{-# INLINE tok_doc_comment_next #-}

tok_line_comment :: Action
tok_line_comment (AlexInput _ _ s) l = do
  let str = unpackUtf8 $! takeUtf8 l s
  return $! TLineComment $! lc2hc str
{-# INLINE tok_line_comment #-}

tok_block_doc_comment_next :: Action
tok_block_doc_comment_next =
  tok_block_comment_with TBlockDocCommentNext skip_spaces
  where
    skip_spaces inp =
      case alexGetChar' inp of
        Just (c, inp') | isSpace c -> skip_spaces inp'
                       | otherwise -> Just (c, inp')
        _                          -> Nothing
{-# INLINE tok_block_doc_comment_next #-}

tok_block_comment :: Action
tok_block_comment = tok_block_comment_with TBlockComment alexGetChar'
{-# INLINE tok_block_comment #-}

tok_block_comment_with :: (String -> Token)
                       -> (AlexInput -> Maybe (Char, AlexInput))
                       -> Action
tok_block_comment_with tok ini inp0 _ = do
  case alexGetChar' inp0 of
    Just ('#', inp1)
      | Just ('|', inp2) <- alexGetChar' inp1
      , Just (c, inp3) <- ini inp2
      , Just (com, inp4) <- go inp3 c ""
      -> alexSetInput inp4 >> return (tok (reverse com))
    _ -> alexError "tok_block_comment: panic"
  where
    go inp prev acc =
      case alexGetChar' inp of
        Just (c, inp') | prev == '|', c == '#' -> Just (tail acc, inp')
                       | otherwise             -> go inp' c (c:acc)
        Nothing                                -> Nothing
{-# INLINE tok_block_comment_with #-}

tok_lambda :: Action
tok_lambda _ _ = return $ TSymbol $! fsLit "\\"
{-# INLINE tok_lambda #-}

tok_symbol :: Action
tok_symbol (AlexInput _ _ s) l = do
  let bs = C8.toStrict $! takeUtf8 (fromIntegral l) s
  return $ TSymbol $! mkFastStringByteString bs
{-# INLINE tok_symbol #-}

tok_char :: Action
tok_char inp0 _ = do
  case alexGetChar' inp0 of
    Just ('\\', inp1) -> go inp1
    _                 -> alexError "tok_char: panic"
  where
    go inp
      | Just (c, inp') <- alexGetChar' inp =
        case c of
          '\\' -> case escapeChar inp' of
            Just (c', inp'') ->
              alexSetInput inp'' >> (return $! TChar c')
            Nothing ->
              alexSetInput inp' >> (return $! TChar '\\')
          _    -> do
            alexSetInput inp'
            return $! TChar c
      | otherwise = alexError "tok_char: panic"
{-# INLINE tok_char #-}

tok_string :: Action
tok_string inp _l =
  -- Currently String tokenizer does not update alex input per
  -- character. This makes the code a bit more effiicient, but getting
  -- unhelpful error message on illegal escape sequence.
  case alexGetChar' inp of
    Just ('"', inp')
      | Just (str, inp'') <- go inp' "" ->
        alexSetInput inp'' >> return str
    _ -> alexError ("lexical error in string: " ++ show inp)
  where
    go inp0 acc =
      case alexGetChar' inp0 of
        Nothing -> Nothing
        Just (c1, inp1)
          | c1 == '"'  -> return $! (TString (reverse acc), inp1)
          | c1 == '\\' ->
            case escapeChar inp1 of
              Just (c1, inp2) -> go inp2 $! (c1:acc)
              _               ->
                case alexGetChar' inp1 of
                  Just (c2, inp2) | c2 == '&' -> go inp2 $! acc
                  _                           -> Nothing
          | otherwise  -> go inp1 $! (c1 : acc)
{-# INLINE tok_string #-}

escapeChar :: AlexInput -> Maybe (Char, AlexInput)
escapeChar inp0
  | Just (c1, inp1) <- alexGetChar' inp0 =
    let ret x = return $! (x, inp1)
        numericChar test acc0 f =
          let lp inp acc =
                case alexGetChar' inp of
                  Just (c2, inp')
                    | test c2 -> lp inp' (c2:acc)
                    | otherwise ->
                      return (Char.chr (read (f (reverse acc))), inp)
                  Nothing -> Nothing
          in lp inp1 acc0
        controlChar
          | Just (c2, inp2) <- alexGetChar' inp1
          , c2 >= '@' && c2 <= '_' =
            return (chr (ord c2 - ord '@'), inp2)
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
            | Just (c2, inp2) <- alexGetChar' inp1
            , Just (c3, inp3) <- alexGetChar' inp2
            -> case lkup [c1,c2,c3] tbl3 of
                 Just c  -> Just (c, inp3)
                 Nothing
                   | Just c <- lkup [c1,c2] tbl2 -> Just (c, inp2)
                 _ -> Nothing
            | otherwise -> Nothing
  | otherwise = Nothing
{-# INLINE escapeChar #-}

tok_integer :: Action
tok_integer (AlexInput _ _ s) l = do
  let str = C8.unpack $! C8.take (fromIntegral l) s
  return $ TInteger $! read $! str
{-# INLINE tok_integer #-}

tok_fractional :: Action
tok_fractional (AlexInput _ _ s) l = do
  let str = C8.unpack (C8.take (fromIntegral l) s)
      rat = readRational str
#if !MIN_VERSION_ghc(8,4,0)
  return $ TFractional $! FL str rat
#else
  let stxt = SourceText str
      is_neg = if 0 < rat then True else False
  return $ TFractional $! FL stxt is_neg rat
#endif
{-# INLINE tok_fractional #-}


-- ---------------------------------------------------------------------
--
-- Lexer
--
-- ---------------------------------------------------------------------

-- | Lexical analyzer for S-expression. Intended to be used with a
-- parser made from Happy. This functions will not pass comment tokens
-- to continuation but add them to 'SPState'.
tokenLexer :: (Located Token -> SP a) -> SP a
tokenLexer cont = do
  st <- getSPState
  let fn = targetFile st
  ltok@(L span tok) <- scanToken fn
  let pushComment comment st = st {comments = comment : comments st}
      comment = do
        let com = L span (annotateComment tok)
        SP (\st -> unSP (tokenLexer cont) (pushComment com st))
      docComment = do
        let com = L span (annotateComment tok)
        SP (\st -> unSP (cont ltok) (pushComment com st))
  case tok of
    TLineComment _         -> comment
    TBlockComment _        -> comment
    TDocCommentNext _      -> docComment
    TBlockDocCommentNext _ -> docComment
    _                      -> cont ltok
{-# INLINE tokenLexer #-}

scanToken :: FastString -> SP (Located Token)
scanToken fn = do
  inp0@(AlexInput loc0 _ _) <- alexGetInput
  let sc = 0
  case alexScan inp0 sc of
    AlexToken inp1@(AlexInput loc1 _ _) len act -> do
      alexSetInput inp1
      tok <- act inp0 len
      let span = RealSrcSpan $ mkRealSrcSpan loc0 loc1
      return (L span tok)
    AlexError (AlexInput loc1 ch _) -> do
      let l = srcLocLine loc1
          c = srcLocCol loc1
      alexError ("lexical error at line " ++ show l ++
                 ", column" ++ show c ++
                 ", near " ++ show ch)
    AlexSkip inp1 _ -> do
      alexSetInput inp1
      scanToken fn
    AlexEOF -> return (L undefined TEOF)
{-# INLINE scanToken #-}

-- | Lex the input to list of 'Token's.
lexTokens :: C8.ByteString -> Either String [Located Token]
lexTokens input = evalSP go Nothing input
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

-- | Lisp comment to Haskell comment.
lc2hc :: String -> String
lc2hc = intercalate "\n" . map (tail' . dropWhile (== ';')) . lines
  where
    tail' xs = case xs of
      []    -> []
      _:xs' -> xs'
{-# INLINE lc2hc #-}

lc2hc_doc :: String -> String
lc2hc_doc = dropWhile (== '|') . dropWhile isSpace . lc2hc
{-# INLINE lc2hc_doc #-}

annotateComment :: Token -> AnnotationComment
annotateComment tok = case tok of
  TDocCommentNext s      -> AnnDocCommentNext s
  TBlockDocCommentNext s -> AnnDocCommentNext s
  TLineComment s         -> AnnLineComment s
  TBlockComment s        -> AnnBlockComment s
  _                      -> error ("annotateComment: " ++ show tok)
{-# INLINE annotateComment #-}

unpackUtf8 :: C8.ByteString -> String
unpackUtf8 = utf8DecodeByteString . C8.toStrict
{-# INLINE unpackUtf8 #-}

takeUtf8 :: Int -> C8.ByteString -> C8.ByteString
takeUtf8 n bs = fst (splitUtf8 n bs)
{-# INLINE takeUtf8 #-}

splitUtf8 :: Int -> C8.ByteString -> (C8.ByteString, C8.ByteString)
splitUtf8 n0 bs0 = go n0 bs0 C8.empty
  where
    go n bs acc
      | n <= 0    = (acc, bs)
      | otherwise =
         case W8.uncons bs of
           Just (w8, bs0) ->
             let (acc', bs')
                   | w8 < 0x80 = (C8.snoc acc (w2c w8), bs0)
                   | w8 < 0xe0 = split 1
                   | w8 < 0xf0 = split 2
                   | otherwise = split 3
                 split k =
                   let (pre, bs1) = C8.splitAt k bs0
                   in  (C8.append (C8.snoc acc (w2c w8)) pre, bs1)
             in  go (n - 1) bs' acc'
           Nothing -> error "takeUtf8: empty input"
{-# INLINE splitUtf8 #-}
}
