-- S-expression lexer with alex
{
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-- | Module for lexing S-expressions.
module Language.SK.Lexer
  ( -- * Token data type
    Token(..)
  , LToken
    -- * Lexer function
  , tokenLexer
  , lexTokens
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
import Control.Monad (ap, liftM)
import Data.Char (toUpper)
import Data.Maybe (fromMaybe)
import Data.Word (Word8)

-- bytestring
import Data.ByteString.Internal (w2c)
import qualified Data.ByteString.Lazy as W8
import qualified Data.ByteString.Lazy.Char8 as BL

-- ghc
import Encoding (utf8DecodeByteString)
import SrcLoc (RealSrcLoc, advanceSrcLoc, srcLocCol, srcLocLine)

-- ghc-boot
import qualified GHC.LanguageExtensions as LangExt

-- transformers
import Control.Monad.Trans.Except (ExceptT(..), throwE)

-- Internal
import Language.SK.Builder
import Language.SK.Form
import Language.SK.GHC
}

$nl          = [\n\r\f]
$whitechar   = [$nl\v\ ]
$white_no_nl = $whitechar # \n

$negative    = \-
$octit       = [0-7]
$digit       = [0-9]
$hexit       = [$digit A-F a-f]

$hsymhead    = [^\(\)\[\]\{\}\;\'\`\,\"\#$digit$white]
$hsymtail    = [$hsymhead\'\#$digit]

@hsymbol     = $hsymhead $hsymtail*
@signed      = $negative ?
@octal       = $octit+
@decimal     = $digit+
@hexadecimal = $hexit+
@exponent    = [eE] [\-\+]? @decimal
@frac        = @decimal \. @decimal @exponent ? | @decimal @exponent


tokens :-

$whitechar+  ;

--- Comments

\;+ $whitechar \| .* { tok_doc_comment_next }
\; .*                { tok_line_comment }

--- Parentheses
\(                   { tok_oparen }
\)                   { tok_cparen }

\[                   { tok_obracket }
\]                   { tok_cbracket }

\{                   { tok_ocurly }
\}                   { tok_ccurly }

-- Quote, unquote, quasiquote, and unquote splice
\'                   { tok_quote }
\`                   { tok_quasiquote }

\,\                  { tok_comma }
\,\@                 { tok_unquote_splice }
\,                   { tok_unquote }

-- Hash
\#                   { tok_hash }

--- Literal values
\\[~$white][A-Za-z]*       { tok_char }
\"                         { tok_string }
@signed @decimal           { tok_integer }
@signed 0[oO] @octal       { tok_integer }
@signed 0[xX] @hexadecimal { tok_integer }
@signed @frac              { tok_fractional }

--- Symbols
@hsymbol         { tok_symbol }

{
-- ---------------------------------------------------------------------
--
-- Parser monad
--
-- ---------------------------------------------------------------------

-- | Data type to hold states while reading source code.
data SPState = SPState
  { comments :: [Located AnnotationComment]
  , annotation_comments :: [(SrcSpan, [Located AnnotationComment])]
  , targetFile :: FastString
  , requiredModuleNames :: [String]
  , langExts :: [LangExt.Extension]
  , buf :: BL.ByteString
  , currentLoc :: RealSrcLoc
  , prevChar :: Char
  } deriving (Eq)

-- | Initial empty state for 'SP'.
initialSPState :: FastString -> Int -> Int -> SPState
initialSPState file linum colnum =
  SPState { comments = []
          , annotation_comments = []
          , targetFile = file
          , requiredModuleNames = []
          , langExts = []
          , buf = BL.empty
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
            BL.ByteString
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

alexGetChar' :: AlexInput -> Maybe (Char, AlexInput)
alexGetChar' inp0 =
  case alexGetByte inp0 of
    Just (w0, inp1)
      | w0 < 0x80 -> do
         let c = w2c w0
         c `seq` return (c, inp1)
      | w0 < 0xe0  -> do
         (w1, inp2) <- alexGetByte inp1
         let c = utf8char [w0,w1]
         c `seq` return (c, setPrevChar inp2 c)
      | w0 < 0xf0  -> do
         (w1, inp2) <- alexGetByte inp1
         (w2, inp3) <- alexGetByte inp2
         let c = utf8char [w0,w1,w2]
         c `seq` return (c, setPrevChar inp3 c)
      | otherwise -> do
         (w1, inp2) <- alexGetByte inp1
         (w2, inp3) <- alexGetByte inp2
         (w3, inp4) <- alexGetByte inp3
         let c = utf8char [w0,w1,w2,w3]
         c `seq` return (c, setPrevChar inp4 c)
      where
        utf8char w8s =
          head (utf8DecodeByteString (W8.toStrict (W8.pack w8s)))
        setPrevChar (AlexInput l _ b) c = AlexInput l c b
    Nothing -> Nothing
{-# INLINE alexGetChar' #-}

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (AlexInput _ c _) = c

alexError :: String -> SP a
alexError msg = SP (\st -> SPNG (RealSrcLoc (currentLoc st)) msg)

alexGetInput :: SP AlexInput
alexGetInput =
  SP (\st@SPState{currentLoc=l,buf=b,prevChar=c} ->
        SPOK st (AlexInput l c b))

alexSetInput :: AlexInput -> SP ()
alexSetInput (AlexInput l c b) =
  SP (\st -> SPOK (st {buf=b,currentLoc=l,prevChar=c}) ())

runSP :: SP a -> Maybe FilePath -> BL.ByteString
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
       -> Maybe FilePath -> BL.ByteString -> Either String (b, SPState)
incrSP sp f z target input = go st1 z
  where
    go st acc =
      case unSP sp st of
        SPNG _loc msg
          | blank (buf st) -> Right (acc, st)
          | otherwise       -> Left msg
        SPOK st' ret       ->
          let st'' = st' {buf=BL.cons (prevChar st') (buf st')}
          in  go st'' $! f ret acc
    blank bl = BL.null bl || BL.all (`elem` "\n\r\t") bl
    st0 = initialSPState target' 1 1
    st1 = st0 {buf = input}
    target' = maybe (fsLit "anon") fsLit target

evalSP :: SP a -> Maybe FilePath -> BL.ByteString -> Either String a
evalSP sp target input = fmap fst (runSP sp target input)

errorSP :: Code -> String -> SP a
errorSP code msg = alexError (showLoc code ++ msg)

lexErrorSP :: SP a
lexErrorSP = do
  AlexInput loc _ _ <- alexGetInput
  let lno = srcLocLine loc
      cno = srcLocCol loc
      msg = "lexer error at line " ++ show lno ++
            ", column " ++ show cno
  alexError msg

putSPState :: SPState -> SP ()
putSPState st = SP (\_ -> SPOK st ())

getSPState :: SP SPState
getSPState = SP (\st -> SPOK st st)


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
  | THash
  -- ^ Literal @#@.
  | TEOF
  -- ^ End of form.
  deriving (Eq, Show)

type LToken = Located Token

type Action = AlexInput -> Int -> SP Token

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
tok_hash _ _ =  return THash
{-# INLINE tok_hash #-}

tok_doc_comment_next :: Action
tok_doc_comment_next (AlexInput _ _ s) l = do
  let str = toString $ BL.take (fromIntegral l) s
  return $ TDocCommentNext $ lc2hc str
{-# INLINE tok_doc_comment_next #-}

tok_line_comment :: Action
tok_line_comment (AlexInput _ _ s) l = do
  let str = toString (BL.take (fromIntegral l) s)
  return (TLineComment (lc2hc str))
{-# INLINE tok_line_comment #-}

tok_symbol :: Action
tok_symbol (AlexInput _ _ s) l = do
  let bs = BL.toStrict $! takeUtf8 (fromIntegral l) s
  return $ TSymbol $! mkFastStringByteString bs
{-# INLINE tok_symbol #-}

tok_char :: Action
tok_char inp0@(AlexInput _ _ s) l
  | '\\':cs <- toString (BL.take (fromIntegral l) s)
  , Just c <- lookup (map toUpper cs) charTable =
    return $! TChar c
  | Just ('\\', inp1) <- alexGetChar' inp0
  , Just (c, inp2) <- alexGetChar' inp1 = do
    alexSetInput inp2
    return $! TChar c
  | otherwise = alexError "tok_char: panic"
  where
    charTable =
      [ ("NUL", '\NUL'), ("SOH", '\SOH'), ("STX", '\STX')
      , ("ETX", '\ETX'), ("EOT", '\EOT'), ("ENQ", '\ENQ')
      , ("ACK", '\ACK'), ("BEL", '\BEL'), ("BS", '\BS')
      , ("HT", '\HT'), ("LF", '\LF'), ("VT", '\VT'), ("FF", '\FF')
      , ("CR", '\CR'), ("SO", '\SO'), ("SI", '\SI'), ("DLE", '\DLE')
      , ("DC1", '\DC1'), ("DC2", '\DC2'), ("DC3", '\DC3')
      , ("DC4", '\DC4'), ("NAK", '\NAK'), ("SYN", '\SYN')
      , ("ETB", '\ETB'), ("CAN", '\CAN'), ("EM", '\EM')
      , ("SUB", '\SUB'), ("ESC", '\ESC'), ("FS", '\FS')
      , ("GS", '\GS'), ("RS", '\RS'), ("US", '\US'), ("SP", '\SP')
      , ("DEL", '\DEL')]
{-# INLINE tok_char #-}

tok_string :: Action
tok_string inp _l =
  case alexGetChar' inp of
    Just ('"', inp') -> alexSetInput inp' >> go ""
    _   -> alexError ("tok_string: panic, inp=" ++ show inp)
  where
    go acc = do
      inp0 <- acc `seq` alexGetInput
      case alexGetChar' inp0 of
        Nothing -> return $ accToTString acc
        Just (c0, inp1)
          | c0 == '"'  -> do
            alexSetInput inp1
            return $ accToTString acc
          | c0 == '\\' ->
            case alexGetChar' inp1 of
              Nothing -> alexError "invalid escape in string literal"
              Just (c1, inp2)
                | Just c2 <- escape c1 ->
                  putAndGo inp2 $! (c2 : acc)
                | c1 == '\n'           ->
                  putAndGo inp2 acc
                | otherwise            ->
                  putAndGo inp2 $! (c1 : acc)
          | otherwise  -> putAndGo inp1 $! (c0 : acc)
    putAndGo inp acc = alexSetInput inp >> go acc
    accToTString = TString . reverse
    escape x = lookup x tbl
      where
        tbl = [('a','\a'),('b','\b'),('f','\f'),('n','\n'),('r','\r')
              ,('t','\t'),('v','\v')]
{-# INLINE tok_string #-}

tok_integer :: Action
tok_integer (AlexInput _ _ s) l =
  return $ TInteger $! read $! toString $! BL.take (fromIntegral l) s
{-# INLINE tok_integer #-}

tok_fractional :: Action
tok_fractional (AlexInput _ _ s) l = do
  let str = toString (BL.take (fromIntegral l) s)
      rat = readRational str
  return $ TFractional $! FL str rat
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
  case tok of
    TLineComment _ -> do
      let comment = L span (annotateComment tok)
      SP (\st -> unSP (tokenLexer cont) (pushComment comment st))
    TDocCommentNext _ -> do
      let comment = L span (annotateComment tok)
      SP (\st -> unSP (cont ltok) (pushComment comment st))
    _ -> cont ltok
{-# INLINE tokenLexer #-}

pushComment :: Located AnnotationComment -> SPState -> SPState
pushComment comment st = st { comments = comment : comments st }
{-# INLINE pushComment #-}

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
    AlexError (AlexInput loc1 _ _) -> do
      let l = srcLocLine loc1
          c = srcLocCol loc1
      alexError ("lexical error at line " ++ show l ++
                 ", column" ++ show c)
    AlexSkip inp1 _ -> do
      alexSetInput inp1
      scanToken fn
    AlexEOF -> return (L undefined TEOF)
{-# INLINE scanToken #-}

-- | Lex the input to list of 'Token's.
lexTokens :: BL.ByteString -> Either String [Located Token]
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

toString :: BL.ByteString -> String
toString = BL.unpack
{-# INLINE toString #-}

-- | Lisp comment to Haskell comment.
lc2hc :: String -> String
lc2hc str = '-':'-':dropWhile (== ';') str
{-# INLINE lc2hc #-}

annotateComment :: Token -> AnnotationComment
annotateComment tok = case tok of
  TDocCommentNext s -> AnnDocCommentNext s
  TLineComment s    -> AnnLineComment s
  _                 -> error ("annotateComment: " ++ show tok)
{-# INLINE annotateComment #-}

takeUtf8 :: Int -> BL.ByteString -> BL.ByteString
takeUtf8 n bs = fst (splitUtf8 n bs)
{-# INLINE takeUtf8 #-}

splitUtf8 :: Int -> BL.ByteString -> (BL.ByteString, BL.ByteString)
splitUtf8 n0 bs0 = go n0 bs0 BL.empty
  where
    go n bs acc
      | n <= 0    = (acc, bs)
      | otherwise =
         case W8.uncons bs of
           Just (w8, bs0) ->
             let (acc', bs')
                   | w8 < 0x80 = (BL.snoc acc (w2c w8), bs0)
                   | w8 < 0xe0 = split 1
                   | w8 < 0xf0 = split 2
                   | otherwise = split 3
                 split k =
                   let (pre, bs1) = BL.splitAt k bs0
                   in  (BL.append (BL.snoc acc (w2c w8)) pre, bs1)
             in  go (n - 1) bs' acc'
           Nothing -> error "takeUtf8: empty input"
{-# INLINE splitUtf8 #-}
}
