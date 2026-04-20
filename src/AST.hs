{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
module AST where
import Text.Printf (printf)
import GHC.Float (int2Float)
import Data.List (intercalate, dropWhileEnd)
import Data.List.Split (splitOn)
import Text.Parsec (SourcePos)

data Op = 
    Plus 
  | Minus 
  | Mult 
  | Div 
  | Equal 
  | NotEqual
  | Greater 
  | GreaterEqual 
  | Less 
  | LessEqual
  deriving Eq

instance Show Op where
  show Plus         = "+"
  show Minus        = "-"
  show Mult         = "*"
  show Div          = "/"
  show Equal        = "=="
  show NotEqual     = "!="
  show Greater      = ">"
  show GreaterEqual = ">="
  show Less         = "<"
  show LessEqual    = "<="

  
  

data Exp n where
  ENum    :: n -> Float -> Exp n
  ENeg    :: n -> Exp n -> Exp n
  EString :: n -> String -> Exp n
  EBool   :: n -> Bool -> Exp n
  EBinOp  :: n -> Op -> Exp n -> Exp n -> Exp n
  ENot    :: n -> Exp n -> Exp n
  Ident   :: n -> String -> Exp n
  EGroup  :: n -> Exp n -> Exp n
  ENil    :: n -> Exp n

expPos :: Exp n -> n
expPos (ENum    n _)     = n
expPos (EString n _)     = n
expPos (EBool   n _)     = n
expPos (EBinOp  n _ _ _) = n
expPos (Ident   n _)     = n
expPos (EGroup   n _)    = n
expPos (ENeg   n _)      = n
expPos (ENot   n _)      = n
expPos (ENil    n)       = n

instance Show (Exp a) where
  show :: Exp n -> String
  show (ENum _ n)          = displayNum n
  show (EBinOp _ op e1 e2) = printf "(%s %s %s)" (show op) (show e1) (show e2)
  show (EBool _ True)      = "true"
  show (EBool _ False)     = "false"
  show (Ident _ id')       = id'
  show (EString _ str)     = str
  show (EGroup _ e)        = printf "(group %s)" (show e)
  show (ENot _ e)          = printf "(! %s)" (show e)
  show (ENeg _ e)          = printf "(- %s)" (show e)
  show (ENil _ )           = "nil"

displayNum :: Float -> String 
displayNum n
  | '.' `notElem` nStr = nStr ++ ".0"
  | otherwise          = 
    case splitOn "." nStr of
      [int,dec] ->
        if dec == "0"
          then int
          else intercalate "." [int, truncatedDec dec]
      _         -> error $ printf "malform number: (%s)" nStr
  where
    nStr = show n
    truncatedDec dec = 
      let dec' = dropWhileEnd (== '0') dec
      in if null dec' then "0" else dec'

