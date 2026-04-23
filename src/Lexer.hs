{-# LANGUAGE DerivingVia #-}
module Lexer (LexResult (..), Token (..), tokenize) where

import Lib
import Text.Parsec
import Token
import System.Exit (die)
import Data.Either (fromRight)
import Data.Functor (($>), (<&>))
import Text.Parsec.Error (Message(UnExpect), errorMessages)
import Text.Printf (printf)
import Control.Monad (void)
import GHC.Stack (HasCallStack)




data LexResult = LexToken (Token SourcePos) | LexError LineNumber String

instance Show LexResult where
  show (LexToken tkn) = show tkn
  show (LexError ln msg) = printf "[line %d] Error: %s" ln msg


type Parser = Parsec String ()

posToken :: (SourcePos -> Token SourcePos) -> Parser LexResult
posToken t = LexToken . t <$> getPosition

token' :: HasCallStack => Parser LexResult
token' =
  try (string "!=" >> posToken BangEqual) <|>
  try (string "==" >> posToken EqualEqual) <|>
  try (string "<=" >> posToken LessEqual) <|>
  try (string ">=" >> posToken GreaterEqual) <|>
  -- extract (alphaNum <|> char '_') to its own parser
  try (reserved' <* notFollowedBy (alphaNum <|> char '_')) <|>
  (string "(" >> posToken LeftParen) <|>
  (string ")" >> posToken RightParen) <|>
  (string "{" >> posToken LeftBrace) <|>
  (string "}" >> posToken RightBrace) <|>
  (string "*" >> posToken Star) <|>
  (string "." >> posToken Dot) <|>
  (string "," >> posToken Comma) <|>
  (string "+" >> posToken Plus) <|>
  (string "-" >> posToken Minus) <|>
  (string ";" >> posToken Semicolon) <|>
  (string "/" >> posToken Slash) <|>
  (string "=" >> posToken Equal) <|>
  (string "<" >> posToken Less) <|>
  (string ">" >> posToken Greater) <|>
  (string "!" >> posToken Bang) <|>
  lString <|>
  lNumber <|>
  ident' <|>
  (LexError <$> (getPosition <&> sourceLine) <*> (printf "Unexpected character: %c" <$> anyChar ))

reservedWords :: [String]
reservedWords = 
  ["and",
   "class",
   "else",
   "false",
   "for",
   "fun",
   "if",
   "nil",
   "or",
   "print",
   "return",
   "super",
   "this",
   "true",
   "var",
   "while"
  ]

reserved' :: Parser LexResult
reserved' =  
  LexToken <$> 
    (Reserved <$> getPosition <*> (choice $ map string' reservedWords))

lNumber :: Parser LexResult  
lNumber = LexToken <$> (LNumber <$> getPosition <*> (try decimal <|> num))
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
  (char '"' *> (LexToken <$> (LString <$> getPosition <*> pure str))) <|> 
   (eof $> LexError line "Unterminated string.")
  

ident' :: Parser LexResult  
ident' = 
  LexToken <$> (Ident <$> getPosition <*>
    ((:) <$> (char '_' <|> letter) <*> many (char '_' <|> alphaNum)))


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
tokens' =
  (concat <$> (sepBy tokensLine newline)) <> 
    (eof >> (:[]) . LexToken . EOF <$> getPosition)



tokenize :: HasCallStack => FilePath -> String -> [LexResult]
tokenize path str = 
  either (error . show) id (runParser tokens' () path str)

testLexer :: String -> [LexResult]
testLexer str = either (error . show) id (runParser tokens' () "<input>" str)

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
