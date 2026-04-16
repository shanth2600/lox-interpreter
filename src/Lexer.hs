module Lexer (Token (..), tokenize) where

import Text.Parsec
import Token
import System.Exit (die)
import Data.Either (fromRight)
import Data.Functor (($>))

type Parser = Parsec String ()

token' :: Parser Token
token' =
  (char '(' >> pure LeftParen) <|>
  (char ')' >> pure RightParen) <|>
  (eof >> pure EOF)

tokens' :: Parser [Token]  
tokens' = (many1 token') <|> (eof $> [])

tokenize :: String -> [Token]
tokenize str = 
  either (error . show) id (runParser tokens' () "" str)