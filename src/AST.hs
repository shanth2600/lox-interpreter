{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
module AST where
import Text.Printf (printf)
import GHC.Float (int2Float)
import Data.List (intercalate, dropWhileEnd)

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
  EInt    :: n -> Int -> Exp n Int
  EFloat  :: n -> Int -> Int -> Exp n Float
  EString :: n -> String -> Exp n String
  EBool   :: n -> Bool -> Exp n Bool
  EBinOp  :: n -> Op -> Exp n a -> Exp n a -> Exp n a
  Ident   :: n -> String -> Exp n String
  ENil    :: n -> Exp n a

expPos :: Exp n a -> n
expPos (EInt    n _)     = n
expPos (EFloat  n _ _)   = n
expPos (EString n _)     = n
expPos (EBool   n _)     = n
expPos (EBinOp  n _ _ _) = n
expPos (Ident   n _)     = n
expPos (ENil    n)       = n

instance Show (Exp n a) where
  show :: Exp n a -> String
  show (EInt _ n)          = printf "%d.0" n
  show (EFloat _ n1 n2)    = intercalate "." [show n1, show n2]
  show (EBinOp _ op e1 e2) = printf "(%s %s %s)" (show op) (show e1) (show e2)
  show (EBool _ True)      = "true"
  show (EBool _ False)     = "false"
  show (Ident _ id')       = id'
  show (EString _ str)     = str
  show (ENil _ )           = "nil"

displayFloat :: Exp a Float -> String 
displayFloat (EFloat _ int dec) = 
  intercalate "." [show int, truncatedDec]
  where
    truncatedDec = 
      let dec' = dropWhileEnd (== '0') (show dec) 
      in if null dec' then "0" else dec'


instance Show (SomeExp a) where
  show (SomeExp exp) = show exp