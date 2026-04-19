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
  EInt    :: n -> Int -> Exp n
  EFloat  :: n -> Int -> Int -> Exp n
  ENeg    :: n -> Exp n -> Exp n
  EString :: n -> String -> Exp n
  EBool   :: n -> Bool -> Exp n
  EBinOp  :: n -> Op -> Exp n -> Exp n -> Exp n
  ENot    :: n -> Exp n -> Exp n
  Ident   :: n -> String -> Exp n
  EGroup  :: n -> Exp n -> Exp n
  ENil    :: n -> Exp n

expPos :: Exp n -> n
expPos (EInt    n _)     = n
expPos (EFloat  n _ _)   = n
expPos (EString n _)     = n
expPos (EBool   n _)     = n
expPos (EBinOp  n _ _ _) = n
expPos (Ident   n _)     = n
expPos (EGroup   n _)    = n
expPos (ENeg   n _)      = n
expPos (ENot   n _)      = n
expPos (ENil    n)       = n

instance Show (Exp n) where
  show :: Exp n -> String
  show (EInt _ n)          = printf "%d.0" n
  show e@(EFloat _ n1 n2)  = displayFloat e
  show (EBinOp _ op e1 e2) = printf "(%s %s %s)" (show op) (show e1) (show e2)
  show (EBool _ True)      = "true"
  show (EBool _ False)     = "false"
  show (Ident _ id')       = id'
  show (EString _ str)     = str
  show (EGroup _ e)        = printf "(group %s)" (show e)
  show (ENot _ e)          = printf "(! %s)" (show e)
  show (ENeg _ e)          = printf "(- %s)" (show e)
  show (ENil _ )           = "nil"

displayFloat :: Exp a -> String 
displayFloat (EFloat _ int dec) = 
  intercalate "." [show int, truncatedDec]
  where
    truncatedDec = 
      let dec' = dropWhileEnd (== '0') (show dec) 
      in if null dec' then "0" else dec'

