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
  ) where

-- From 'base'
import Control.Monad (ap, liftM)
import Data.Char (toUpper)
import Data.Maybe (fromMaybe)

import Control.Monad.Trans.Except

-- Internal
import SK.Core.GHC

}

%wrapper "monad"

$digit = 0-9
$alpha = [a-zA-Z]

$unispace    = \x05
$nl          = [\n\r\f]
$whitechar   = [$nl\v\ $unispace]
$white_no_nl = $whitechar # \n

tokens :-

$whitechar+  ;

--- Comments

\;+ $whitechar \| .* { tok_doc_comment_next }
\; .*                { tok_line_comment }

--- Haskell style pragmas
---
--- Currently ignored.

"{-#".*          { skip }

--- Parenthesis

"()"             { tok_unit }

\(               { tok_oparen }
\)               { tok_cparen }

\[               { tok_obracket }
\]               { tok_cbracket }

-- Quote and unquote

\'               { tok_quote }
\`               { tok_quasiquote }

", "             { tok_comma }
",@"             { tok_unquote_splice }
\,               { tok_unquote }

--- Literal values

\\[~$white][A-Za-z]* { tok_char }
\"[^\"]*\"           { tok_string }
$digit+              { tok_integer }


-- Binary operators
--
-- Some of the binary operators defined in Prelude are converted to
-- intermediate state to support variable number of arguments, for
-- convenience.
--
-- May worth adding Token constructor dedicated to binary operators, to
-- increase efficiency in macro expansion.

\+               { tok_plus }
\*               { tok_asterisk }

--- Symbols

[^\(\)\[\]\;\'\`\,\~$white]+ { tok_symbol }


{
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
  | TDocCommentNext String
  -- ^ Comment string starting with @-- |@.
  | TLineComment String
  -- ^ Non-documentation line comment string.
  | TSymbol String
  -- ^ Symbol data.
  | TChar Char
  -- ^ Character data.
  | TString String
  -- ^ Literal string data.
  | TInteger Integer
  -- ^ Literal integer.
  | TUnit
  -- ^ Unit type, i.e. @()@.
  | TEOF
  -- ^ End of form.
  deriving (Eq, Show)

type LToken = Located Token

type Action = AlexInput -> Int -> Alex Token

tok_unit :: Action
tok_unit _ _ = return TUnit

tok_oparen :: Action
tok_oparen _ _ = return TOparen

tok_cparen :: Action
tok_cparen _ _ = return TCparen

tok_obracket :: Action
tok_obracket _ _ = return TObracket

tok_cbracket :: Action
tok_cbracket _ _ = return TCbracket

tok_quote :: Action
tok_quote _ _ = return (TSymbol "quote")

tok_quasiquote :: Action
tok_quasiquote _ _ = return (TSymbol "quasiquote")

tok_comma :: Action
tok_comma _ _ = return (TSymbol ",")

tok_unquote :: Action
tok_unquote _ _ = return (TSymbol "unquote")

tok_unquote_splice :: Action
tok_unquote_splice _ _ = return (TSymbol "unquote-splice")

tok_doc_comment_next :: Action
tok_doc_comment_next (_,_,_,s) l =
  return (TDocCommentNext (lc2hc (take l s)))

tok_line_comment :: Action
tok_line_comment (_,_,_,s) l =
  return (TLineComment (lc2hc (take l s)))

tok_symbol :: Action
tok_symbol (_,_,_,s) l = return (TSymbol (take l s))

tok_char :: Action
tok_char (_,_,_,s) l =
  case take l s of
    ['\\',c] -> return (TChar c)
    '\\':cs | Just c' <- lookup (map toUpper cs) charTable
             -> return (TChar c')
    _        -> do
     (AlexPn _ lno cno, _, _, _) <- alexGetInput
     alexError ("unknown character token `" ++ s' ++ "' at line " ++
                show lno ++ ", column " ++ show cno)
  where
    s' = take l s
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

tok_string :: Action
tok_string (_,_,_,s) l = return (TString (tail (take (l-1) s)))

tok_integer :: Action
tok_integer (_,_,_,s) l = return (TInteger (read (take l s)))

tok_plus :: Action
tok_plus _ _ = return (TSymbol "__+")

tok_asterisk :: Action
tok_asterisk _ _ = return (TSymbol "__*")

alexEOF :: Alex Token
alexEOF = return TEOF

-- | Lisp comment to Haskell comment.
lc2hc :: String -> String
lc2hc str = '-':'-':dropWhile (== ';') str

--
-- Parser monad
--

-- | Data type to hold comments found in source code.
data SPState = SPState {
  comments :: [Located AnnotationComment],
  annotation_comments :: [(SrcSpan, [Located AnnotationComment])],
  targetFile :: Maybe FilePath
}

-- | Initial empty state for 'SP'.
initialSPState :: SPState
initialSPState = SPState [] [] Nothing

-- | A data type for State monad which wraps 'Alex' with 'SPstate'.
newtype SP a = SP { unSP :: SPState -> Alex (a, SPState) }

instance Functor SP where
  fmap = liftM

instance Applicative SP where
  pure = return
  (<*>) = ap

instance Monad SP where
  return a = a `seq` SP (\st -> return (a, st))
  m >>= k = SP (\st -> unSP m st >>= \(a, st') -> unSP (k a) st')

runSP :: SP a -> Maybe FilePath -> String -> Either String (a, SPState)
runSP sp target input =
  let st = initialSPState { targetFile = target }
  in  runAlex input (unSP sp st)

runSP' :: Monad m => SP a -> Maybe FilePath -> String
      -> ExceptT String m (a, SPState)
runSP' sp target input = case runSP sp target input of
  Right (a, st) -> return (a, st)
  Left err      -> throwE err

evalSP :: SP a -> Maybe FilePath -> String -> Either String a
evalSP sp target input = fmap fst (runSP sp target input)

showErrorSP :: SP a
showErrorSP =
  let go = do (AlexPn _ lno cno, _, _, _) <- alexGetInput
              alexError ("parse error at line " ++ show lno ++
                         ", column " ++ show cno)
  in  SP (\_ -> go)


--
-- Lexer
--

annotateComment :: Token -> AnnotationComment
annotateComment tok = case tok of
  TDocCommentNext s -> AnnDocCommentNext s
  TLineComment s    -> AnnLineComment s
  _                 -> error ("annotateComment: " ++ show tok)

pushComment :: Located AnnotationComment -> SPState -> SPState
pushComment comment st = st { comments = comment : comments st }

-- | Lexical analyzer for S-expression. Intended to be used with a
-- parser made from Happy. This functions will not pass comment tokens
-- to continuation but add them to 'SPState'.
tokenLexer :: (Located Token -> SP a) -> SP a
tokenLexer cont = SP go where
  go st0 = do
    (L span tok, st1) <- unSP scanToken st0
    case tok of
      TLineComment _ -> do
        let comment = L (RealSrcSpan span) (annotateComment tok)
        go (pushComment comment st1)
      TDocCommentNext _ -> do
        let comment = L (RealSrcSpan span) (annotateComment tok)
        unSP (cont (L (RealSrcSpan span) tok))
             (pushComment comment st1)
      _ -> unSP (cont (L (RealSrcSpan span) tok)) st1

scanToken :: SP (RealLocated Token)
scanToken = SP go where
  go st = do
    inp@(AlexPn _ ln0 cn0,_,_,_) <- alexGetInput
    sc <- alexGetStartCode
    case alexScan inp sc of
      AlexEOF -> do eof <- alexEOF
                    return (L undefined eof, st)
      AlexError (AlexPn _ ln cn,_,_,_) ->
        alexError ("lexial error at line " ++ show ln ++ ", column "
                   ++ show cn)
      AlexSkip inp' _ -> do
        alexSetInput inp'
        go st
      AlexToken inp'@(AlexPn _ ln1 cn1,_,_,_) len act -> do
         alexSetInput inp'
         tok <- act (ignorePendingBytes inp) len
         let fn = fromMaybe "anon" (targetFile st)
             bgn = mkRealSrcLoc (fsLit fn) ln0 cn0
             end = mkRealSrcLoc (fsLit fn) ln1 cn1
             span = mkRealSrcSpan bgn end
         return (L span tok, st)

-- | Lex the input to list of 'Token's.
lexTokens :: String -> Either String [Located Token]
lexTokens input = evalSP go Nothing input
  where
     go = do
       tok <- tokenLexer return
       case tok of
         L _ TEOF -> return []
         _        -> (tok :) <$> go
}
