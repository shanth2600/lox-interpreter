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
  show (LexError c ln) = printf "[line %d] Error: Unexpected character: %c" ln c


type Parser = Parsec String ()

token' :: Parser LexResult
token' =
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
  (string "==" >> pure (LexToken EqualEqual)) <|>
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
