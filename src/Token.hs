module Token where

import Data.Maybe (maybe)
import Data.List (intercalate)

data Token = LeftParen | RightParen | EOF
instance Show Token where
  show LeftParen  = "LEFT_PAREN ( null"
  show RightParen =  "RIGHT_PAREN ) null"
  show EOF = "EOF  null"