{-# LANGUAGE DerivingVia #-}
module Lexer (LexResult (..), Token (..), tokenize) where

import Text.Parsec
import Token
import System.Exit (die)
import Data.Either (fromRight)
import Data.Functor (($>), (<&>))
import Text.Parsec.Error (Message(UnExpect), errorMessages)
import Text.Printf (printf)

type LineNumber = Int



data LexResult = LexToken Token | LexError Char LineNumber

instance Show LexResult where
  show (LexToken tkn) = show tkn
  show (LexError c ln) = printf "[line %d] Error: Unexpected character: %s" ln c


type Parser = Parsec String ()

token' :: Parser LexResult
token' =
  (char '(' >> pure (LexToken LeftParen)) <|>
  (char ')' >> pure (LexToken RightParen)) <|>
  (char '{' >> pure (LexToken LeftBrace)) <|>
  (char '}' >> pure (LexToken RightBrace)) <|>
  (char '*' >> pure (LexToken Star)) <|>
  (char '.' >> pure (LexToken Dot)) <|>
  (char ',' >> pure (LexToken Comma)) <|>
  (char '+' >> pure (LexToken Plus)) <|>
  (char '-' >> pure (LexToken Minus)) <|>
  (char ';' >> pure (LexToken Semicolon)) <|>
  (char '/' >> pure (LexToken Slash)) <|>
  (LexError <$> anyChar <*> (getPosition <&> sourceLine))


tokens' :: Parser [LexResult]  
tokens' = (many1 token') <> (eof $> [LexToken EOF])

tokenize :: String -> IO [LexResult]
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
