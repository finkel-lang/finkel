-- S-expression lexer with alex
{
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-- | Module for lexing S-expressions.
module SK.Core.Lexer
  ( -- * Token data type
    Token(..)
  , LToken
    -- * Lexer function
  , tokenLexer
  , lexTokens
    -- * S-expression parser monad
  , SP(..)
  , SPState(..)
  , runSP
  , runSP'
  , evalSP
  , showErrorSP
  , addRequiredModuleName
  ) where

-- base
import Control.Monad (ap, liftM)
import Data.Char (toUpper)
import Data.Maybe (fromMaybe)

-- bytestring
import qualified Data.ByteString.Lazy.Char8 as BL

-- transformers
import Control.Monad.Trans.Except (ExceptT(..), throwE)

-- Internal
import SK.Core.GHC
}

%wrapper "monad-bytestring"

$unispace    = \x05
$nl          = [\n\r\f]
$whitechar   = [$nl\v\ $unispace]
$white_no_nl = $whitechar # \n

$alpha = [a-zA-Z]

$negative = \-
$digit    = [0-9]

$hsymhead = [^\(\)\[\]\{\}\;\'\`\,\"$white]
$hsymtail = [$hsymhead\']

@signed   = $negative ?
@decimal  = $digit+
@exponent = [eE] [\-\+]? @decimal
@frac     = @decimal \. @decimal @exponent ? | @decimal @exponent
@hsymbol  = $hsymhead $hsymtail*


tokens :-

$whitechar+  ;

--- Comments

\;+ $whitechar \| .* { tok_doc_comment_next }
\; .*                { tok_line_comment }

--- Haskell style pragmas, currently ignored.

"{-#".*          { skip }

--- Parenthesis
-- "()"             { tok_unit }

\(               { tok_oparen }
\)               { tok_cparen }

\[               { tok_obracket }
\]               { tok_cbracket }

\{               { tok_ocurly }
\}               { tok_ccurly }

-- Quote and unquote
\'               { tok_quote }
\`               { tok_quasiquote }

", "             { tok_comma }
",@"             { tok_unquote_splice }
\,               { tok_unquote }

--- Literal values
\\[~$white][A-Za-z]* { tok_char }
\"                   { tok_string }
@signed @decimal     { tok_integer }
@signed @frac        { tok_fractional }

--- Symbols
@hsymbol         { tok_symbol }

{
-- ---------------------------------------------------------------------
--
-- Parser monad
--
-- ---------------------------------------------------------------------

-- | Data type to hold states while reading source code.
data SPState = SPState {
  comments :: [Located AnnotationComment],
  annotation_comments :: [(SrcSpan, [Located AnnotationComment])],
  targetFile :: Maybe FilePath,
  requiredModuleNames :: [String]
}

-- | Initial empty state for 'SP'.
initialSPState :: SPState
initialSPState = SPState [] [] Nothing []

-- | A data type for State monad which wraps 'Alex' with 'SPstate'.
newtype SP a = SP { unSP :: SPState -> Alex (a, SPState) }

instance Functor SP where
  fmap f (SP sp) = SP (\st -> fmap (\(a,st) -> (f a, st)) (sp st))
  {-# INLINE fmap #-}

instance Applicative SP where
  pure a = a `seq` SP (\st -> pure (a,st))
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Monad SP where
  return a = a `seq` SP (\st -> return (a,st))
  {-# INLINE return #-}
  m >>= k = SP (\st -> unSP m st >>= \(a,st') -> unSP (k a) st')
  {-# INLINE (>>=) #-}

runSP :: SP a -> Maybe FilePath -> BL.ByteString
      -> Either String (a, SPState)
runSP sp target input =
  let st = initialSPState { targetFile = target }
  in  runAlex input (unSP sp st)

runSP' :: Monad m => SP a -> Maybe FilePath -> BL.ByteString
      -> ExceptT String m (a, SPState)
runSP' sp target input =
  case runSP sp target input of
    Right (a, st) -> return (a, st)
    Left err      -> throwE err

evalSP :: SP a -> Maybe FilePath -> BL.ByteString -> Either String a
evalSP sp target input = fmap fst (runSP sp target input)

showErrorSP :: SP a
showErrorSP =
  let go = do (AlexPn _ lno cno, _, _, _) <- alexGetInput
              alexError ("lexer error at line " ++
                          show lno ++ ", column " ++ show cno)
  in  SP (\_ -> go)

addRequiredModuleName :: String -> SP ()
addRequiredModuleName name =
  SP (\st ->
       let names = requiredModuleNames st
           st' = st {requiredModuleNames = name : names}
       in  return ((), st'))


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
  | TEOF
  -- ^ End of form.
  deriving (Eq, Show)

type LToken = Located Token

type Action = AlexInput -> Int64 -> Alex Token

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
tok_comma _ _ = return $! TSymbol $! fsLit ","
{-# INLINE tok_comma #-}

tok_unquote :: Action
tok_unquote _ _ = return TUnquote
{-# INLINE tok_unquote #-}

tok_unquote_splice :: Action
tok_unquote_splice _ _ = return TUnquoteSplice
{-# INLINE tok_unquote_splice #-}

tok_doc_comment_next :: Action
tok_doc_comment_next (_,_,s,_) l = do
  let str = toString (BL.take l s)
  return (TDocCommentNext (lc2hc str))
{-# INLINE tok_doc_comment_next #-}

tok_line_comment :: Action
tok_line_comment (_,_,s,_) l = do
  let str = toString (BL.take l s)
  return (TLineComment (lc2hc str))
{-# INLINE tok_line_comment #-}

tok_symbol :: Action
tok_symbol (_,_,s,_) l = do
  let bs = l `seq` s `seq` BL.toStrict $! BL.take l s
  bs `seq` return $! TSymbol $! mkFastStringByteString bs
{-# INLINE tok_symbol #-}

tok_char :: Action
tok_char (_,_,s,_) l =
  case s' of
    ['\\',c] -> return (TChar c)
    '\\':cs | Just c' <- lookup (map toUpper cs) charTable
             -> return (TChar c')
    _        -> do
     (AlexPn _ lno cno, _, _, _) <- alexGetInput
     alexError ("unknown character token `" ++ s' ++ "' at line " ++
                show lno ++ ", column " ++ show cno)
  where
    s' = toString (BL.take l s)
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
tok_string _ast0 _l = do
  inp <- alexGetInput
  case alexGetChar' inp of
    Just ('"', inp') -> alexSetInput inp' >> go BL.empty
    _                -> alexError "tok_string: panic"
  where
    go acc = do
       inp0 <- alexGetInput
       case alexGetChar' inp0 of
         Nothing -> return (accToTString acc)
         Just (c0, inp1)
           | c0 == '"'  -> return (accToTString acc)
           | c0 == '\\' ->
             case alexGetChar' inp1 of
               Nothing -> alexError "invalid escape in string literal"
               Just (c1, inp2)
                 | Just c2 <- escape c1 ->
                   putAndGo inp2 (BL.cons c2 acc)
                 | c1 == '\n'           ->
                   putAndGo inp2 acc
                 | otherwise            ->
                   putAndGo inp2 (BL.cons c1 acc)
           | otherwise  -> putAndGo inp1 (BL.cons c0 acc)
    putAndGo inp acc = alexSetInput inp >> go acc
    accToTString = TString . BL.unpack . BL.reverse
    escape x = lookup x tbl
      where
        tbl = [('a','\a'),('b','\b'),('f','\f'),('n','\n'),('r','\r')
              ,('t','\t'),('v','\v')]
{-# INLINE tok_string #-}

tok_integer :: Action
tok_integer (_,_,s,_) l =
  return (TInteger (read (toString (BL.take l s))))
{-# INLINE tok_integer #-}

tok_fractional :: Action
tok_fractional (_,_,s,_) l = do
  let str = toString (BL.take l s)
      rat = readRational str
  return (TFractional (FL str rat))
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
tokenLexer cont = SP go'
  where
    go' st =
      let fn = fsLit (fromMaybe "anon" (targetFile st))
      in  go fn st
    go fn st0 = do
      L span tok <- scanToken fn
      case tok of
        TLineComment _ -> do
          let comment = L (RealSrcSpan span) (annotateComment tok)
          go fn $! pushComment comment st0
        TDocCommentNext _ -> do
          let comment = L (RealSrcSpan span) (annotateComment tok)
          unSP (cont $! L (RealSrcSpan span) tok)
               (pushComment comment st0)
        _ -> unSP (cont $! L (RealSrcSpan span) tok) st0

pushComment :: Located AnnotationComment -> SPState -> SPState
pushComment comment st = st { comments = comment : comments st }
{-# INLINE pushComment #-}

scanToken :: FastString -> Alex (RealLocated Token)
scanToken fn = do
    inp@(AlexPn _ ln0 cn0,_,_,_) <- alexGetInput
    sc <- alexGetStartCode
    case alexScan inp sc of
      AlexEOF -> do eof <- alexEOF
                    return (L undefined eof)
      AlexError (AlexPn _ ln cn,_,_,_) ->
        alexError ("lexial error at line " ++ show ln ++ ", column "
                   ++ show cn)
      AlexSkip inp' _ -> do
        alexSetInput inp'
        scanToken fn
        -- go st
      AlexToken inp'@(AlexPn _ ln1 cn1,_,_,_) len act -> do
         alexSetInput inp'
         tok <- act (ignorePendingBytes inp) (fromIntegral len)
         let bgn = mkRealSrcLoc fn ln0 cn0
             end = mkRealSrcLoc fn ln1 cn1
             span = mkRealSrcSpan bgn end
         return $! (L span tok)
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

alexEOF :: Alex Token
alexEOF = return TEOF

alexGetChar' :: AlexInput -> Maybe (Char, AlexInput)
alexGetChar' (AlexPn pos ln col, chr, bs, consumed) =
  if BL.null bs
    then Nothing
    else Just (chr, (p', BL.head bs, BL.tail bs, consumed + 1))
  where
    p' = AlexPn (pos+1) ln' col'
    (ln', col') = if chr == '\n' then (ln+1, 0) else (ln, col+1)
{-# INLINE alexGetChar' #-}


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
}
