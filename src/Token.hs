{-# LANGUAGE DeriveFunctor #-}
module Token where

import Data.Maybe (maybe)
import Data.List (intercalate, dropWhileEnd)
import Text.Printf (printf)
import Data.List.Split (splitOn)
import Data.Char (toUpper)

data Token a = 
    LeftParen a 
  | RightParen  a 
  | LeftBrace  a 
  | RightBrace  a 
  | Star a 
  | Dot a 
  | Comma a 
  | Plus a 
  | Minus a 
  | Slash a 
  | Equal a 
  | EqualEqual a 
  | Bang a 
  | BangEqual a 
  | Less a 
  | LessEqual a 
  | Greater a 
  | GreaterEqual a 
  | Semicolon a 
  | LString a String 
  | LNumber a String
  | Ident a String 
  | Reserved a String
  | EOF a 
  deriving Functor

instance Eq (Token a) where
  LeftParen _      == LeftParen _      = True
  RightParen _     == RightParen _     = True
  LeftBrace _      == LeftBrace _      = True
  RightBrace _     == RightBrace _     = True
  Star _           == Star _           = True
  Dot _            == Dot _            = True
  Comma _          == Comma _          = True
  Plus _           == Plus _           = True
  Minus _          == Minus _          = True
  Slash _          == Slash _          = True
  Equal _          == Equal _          = True
  EqualEqual _     == EqualEqual _     = True
  Bang _           == Bang _           = True
  BangEqual _      == BangEqual _      = True
  Less _           == Less _           = True
  LessEqual _      == LessEqual _      = True
  Greater _        == Greater _        = True
  GreaterEqual _   == GreaterEqual _   = True
  Semicolon _      == Semicolon _      = True
  LString _ s1     == LString _ s2     = s1 == s2
  LNumber _ n1     == LNumber _ n2     = n1 == n2
  Ident _ i1       == Ident _ i2       = i1 == i2
  Reserved _ r1    == Reserved _ r2    = r1 == r2
  EOF _            == EOF _            = True
  _                == _                = False

instance Show (Token a) where
  show (LeftParen _)    = "LEFT_PAREN ( null"
  show (RightParen _)   = "RIGHT_PAREN ) null"
  show (LeftBrace _)    = "LEFT_BRACE { null"
  show (RightBrace _)   = "RIGHT_BRACE } null"
  show (Star _)         = "STAR * null"
  show (Dot _)          = "DOT . null"
  show (Comma _)        = "COMMA , null"
  show (Plus _)         = "PLUS + null"
  show (Minus _)        = "MINUS - null"
  show (Slash _)        = "SLASH / null"
  show (Semicolon _)    = "SEMICOLON ; null"
  show (Equal _)        = "EQUAL = null"
  show (EqualEqual _)   = "EQUAL_EQUAL == null"
  show (BangEqual _)    = "BANG_EQUAL != null"
  show (Bang _)         = "BANG ! null"
  show (Less _)         = "LESS < null"
  show (LessEqual _)    = "LESS_EQUAL <= null"
  show (Greater _)      = "GREATER > null"
  show (GreaterEqual _) = "GREATER_EQUAL >= null"
  show (LString _ str)  = printf "STRING \"%s\" %s" str str
  show (Ident _ id')    = printf "IDENTIFIER %s null" id'
  show (LNumber _ n)    = printf "NUMBER %s %s" n (formatNum n)
  show (Reserved _ wd)  = intercalate " " [(toUpper <$> wd),wd,"null"]
  show (EOF _)          = "EOF  null"

tokenLiteral :: Token a -> String  
tokenLiteral t =
  case splitOn " " (show t) of
    [_,lit,_] -> lit
    _         -> error $ printf "malformed Token definition: (%s)" (show t)

formatNum :: String -> String  
formatNum nStr = 
  if '.' `elem` nStr
    then truncateZeros nStr
    else nStr ++ ".0"
  where
    truncateZeros :: String -> String
    truncateZeros decStr = 
      case splitOn "." decStr of
      [num,dec] -> 
        let dec' = dropWhileEnd (== '0') (show dec) 
        in if null dec' then num ++ ".0" else num ++ dec'

tokPos :: Token a -> a
tokPos (LeftParen a)     = a
tokPos (RightParen a)    = a
tokPos (LeftBrace a)     = a
tokPos (RightBrace a)    = a
tokPos (Star a)          = a
tokPos (Dot a)           = a
tokPos (Comma a)         = a
tokPos (Plus a)          = a
tokPos (Minus a)         = a
tokPos (Slash a)         = a
tokPos (Equal a)         = a
tokPos (EqualEqual a)    = a
tokPos (Bang a)          = a
tokPos (BangEqual a)     = a
tokPos (Less a)          = a
tokPos (LessEqual a)     = a
tokPos (Greater a)       = a
tokPos (GreaterEqual a)  = a
tokPos (Semicolon a)     = a
tokPos (LString a _)     = a
tokPos (LNumber a _)     = a
tokPos (Ident a _)       = a
tokPos (Reserved a _)    = a
tokPos (EOF a)           = a          
