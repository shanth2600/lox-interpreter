module Token where

import Data.Maybe (maybe)
import Data.List (intercalate)

data Token = LeftParen | RightParen | LeftBrace | RightBrace | EOF
instance Show Token where
  show LeftParen  = "LEFT_PAREN ( null"
  show RightParen =  "RIGHT_PAREN ) null"
  show LeftBrace = "LEFT_BRACE { null"
  show RightBrace = "RIGHT_BRACE } null"
  show EOF = "EOF  null"