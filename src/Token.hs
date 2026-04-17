{-# LANGUAGE InstanceSigs #-}
module Token where

import Data.Maybe (maybe)
import Data.List (intercalate)
import Text.Printf (printf)

data Token = 
    LeftParen 
  | RightParen 
  | LeftBrace 
  | RightBrace 
  | Star
  | Dot
  | Comma
  | Plus
  | Minus
  | Slash
  | Equal
  | EqualEqual
  | Bang
  | BangEqual
  | Less
  | LessEqual
  | Greater
  | GreaterEqual
  | Semicolon
  | LString String
  | LNumber String
  | EOF
instance Show Token where
  show :: Token -> String
  show LeftParen     = "LEFT_PAREN ( null"
  show RightParen    = "RIGHT_PAREN ) null"
  show LeftBrace     = "LEFT_BRACE { null"
  show RightBrace    = "RIGHT_BRACE } null"
  show Star          = "STAR * null"
  show Dot           = "DOT . null"
  show Comma         = "COMMA , null"
  show Plus          = "PLUS + null"
  show Minus         = "MINUS - null"
  show Slash         = "SLASH / null"
  show Semicolon     = "SEMICOLON ; null"
  show Equal         = "EQUAL = null"
  show EqualEqual    = "EQUAL_EQUAL == null"
  show BangEqual     = "BANG_EQUAL != null"
  show Bang          = "BANG ! null"
  show Less          = "LESS < null"
  show LessEqual     = "LESS_EQUAL <= null"
  show Greater       = "GREATER > null"
  show GreaterEqual  = "GREATER_EQUAL >= null"
  show (LString str) = printf "STRING \"%s\" %s" str str
  show (LNumber n)   = printf "NUMBER %s %f" n (read n :: Float)
  show EOF           = "EOF  null"

-- LESS < null
-- LESS_EQUAL <= null
-- GREATER > null
-- GREATER_EQUAL >= null  