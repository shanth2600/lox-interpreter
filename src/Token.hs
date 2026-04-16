{-# LANGUAGE InstanceSigs #-}
module Token where

import Data.Maybe (maybe)
import Data.List (intercalate)

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
  | Semicolon
  | EOF
instance Show Token where
  show :: Token -> String
  show LeftParen  = "LEFT_PAREN ( null"
  show RightParen = "RIGHT_PAREN ) null"
  show LeftBrace  = "LEFT_BRACE { null"
  show RightBrace = "RIGHT_BRACE } null"
  show Star       = "STAR * null"
  show Dot        = "DOT . null"
  show Comma      = "COMMA , null"
  show Plus       = "PLUS + null"
  show Minus      = "MINUS - null"
  show Slash      = "SLASH / null"
  show Semicolon  = "SEMICOLON ; null"
  show Equal      = "EQUAL = null"
  show EqualEqual = "EQUAL_EQUAL == null"
  show EOF        = "EOF  null"