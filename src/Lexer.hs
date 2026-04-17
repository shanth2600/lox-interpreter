{-# LANGUAGE DerivingVia #-}
module Lexer (LexResult (..), Token (..), tokenize) where

import Text.Parsec
import Token
import System.Exit (die)
import Data.Either (fromRight)
import Data.Functor (($>), (<&>))
import Text.Parsec.Error (Message(UnExpect), errorMessages)
import Text.Printf (printf)
import Control.Monad (void)
import GHC.Stack (HasCallStack)

type LineNumber = Int



data LexResult = LexToken Token | LexError LineNumber String

instance Show LexResult where
  show (LexToken tkn) = show tkn
  show (LexError ln msg) = printf "[line %d] Error: %s" ln msg


type Parser = Parsec String ()

token' :: HasCallStack => Parser LexResult
token' =
  try (string "!=" >> pure (LexToken BangEqual)) <|>
  try (string "==" >> pure (LexToken EqualEqual)) <|>
  try (string "<=" >> pure (LexToken LessEqual)) <|>
  try (string ">=" >> pure (LexToken GreaterEqual)) <|>
  (string "(" >> pure (LexToken LeftParen)) <|>
  (string ")" >> pure (LexToken RightParen)) <|>
  (string "{" >> pure (LexToken LeftBrace)) <|>
  (string "}" >> pure (LexToken RightBrace)) <|>
  (string "*" >> pure (LexToken Star)) <|>
  (string "." >> pure (LexToken Dot)) <|>
  (string "," >> pure (LexToken Comma)) <|>
  (string "+" >> pure (LexToken Plus)) <|>
  (string "-" >> pure (LexToken Minus)) <|>
  (string ";" >> pure (LexToken Semicolon)) <|>
  (string "/" >> pure (LexToken Slash)) <|>
  (string "=" >> pure (LexToken Equal)) <|>
  (string "<" >> pure (LexToken Less)) <|>
  (string ">" >> pure (LexToken Greater)) <|>
  (string "!" >> pure (LexToken Bang)) <|>
  lString <|>
  lNumber <|>
  (LexError <$> (getPosition <&> sourceLine) <*> (printf "Unexpected character: %c" <$> anyChar ))

lNumber :: Parser LexResult  
lNumber = LexToken . LNumber <$> (try decimal <|> num)
  where 
    num :: Parser String
    num = many1 digit
    decimal :: Parser String
    decimal = (many1 digit <> string "." <> many1 digit)

lString :: Parser LexResult 
lString = do
  _ <- char '"'
  line <- getPosition <&> sourceLine
  str <- manyTill anyChar (lookAhead $ (void $ char '"') <|> eof)
  res <- (char '"' $> LexToken (LString str)) <|>
         (eof $> LexError line "Unterminated string.")
         
  pure res


tokensLine :: HasCallStack => Parser [LexResult]  
tokensLine = do
  _      <- many space 
  tokens <- manyTill (token' <* many space) 
           (lookAhead (try (void comment <|> endOfLine)))
  _      <- manyTill anyChar (lookAhead endOfLine)
  pure tokens
  where
    comment :: Parser String  
    comment = string "//" <> manyTill anyChar (lookAhead endOfLine)
    endOfLine = void (string "\n") <|> eof

tokens' :: HasCallStack => Parser [LexResult]
tokens' = (:[]) <$> lNumber
  -- (concat <$> (sepBy tokensLine newline)) <> (eof $> [LexToken EOF])


tokenize :: HasCallStack => String -> IO [LexResult]
tokenize str = 
  either (error . show) return (runParser tokens' () "" str)
  

-- outputLexResult ::

-- handleError :: ParseError -> IO ()  
-- handleError e = 
--   case getUnexpected e of
--     Nothing -> die $ show e
--     Just unexp -> 
--       let ln = sourceLine $ errorPos e in
--         printf "[line %d] Error: Unexpected character: %s" ln unexp

  

-- getUnexpected :: ParseError -> Maybe String
-- getUnexpected err =
--   case [s | UnExpect s <- errorMessages err] of
--     (x:_) -> Just x
--     []    -> Nothing
