{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
module AST where
import Text.Printf (printf)

data Op = 
    Plus 
  | Minus 
  | Mult 
  | Div 
  | Equal 
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
  show Greater      = ">"
  show GreaterEqual = ">="
  show Less         = "<"
  show LessEqual    = "<="

data SomeExp n where
  SomeExp :: Exp n a -> SomeExp n

data Exp n a  where
  EInt   :: n -> Int -> Exp n Int
  EBool  :: n -> Bool -> Exp n Bool
  EBinOp :: n -> Op -> Exp n a -> Exp n a -> Exp n a
  Ident  :: n -> String -> Exp n String
  ENil   :: n -> Exp n a

expPos :: Exp n a -> n
expPos (EInt   n _)     = n
expPos (EBinOp n _ _ _) = n
expPos (Ident  n _)     = n  

instance Show (Exp n a) where
  show :: Exp n a -> String
  show (EInt _ n) = show n
  show (EBinOp _ op e1 e2) = printf "(%s %s %s)" (show op) (show e1) (show e2)
  show (EBool _ True) = "true"
  show (EBool _ False) = "false"

instance Show (SomeExp a) where
  show (SomeExp exp) = show exp