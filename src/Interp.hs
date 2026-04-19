module Interp where

import AST ( Exp (..), Op (..))
import Text.Parsec (SourcePos)

data Val = 
    VInt Int
  | VBool Bool
  deriving Eq

instance Show Val where
  show (VInt n) = show n
  show (VBool True) = "true"
  show (VBool False) = "false"

eval :: Exp SourcePos -> Val
eval (EInt _ n)   = VInt n
eval (EBool _ b)  = VBool b
eval (ENot _ e) = case eval e of
  VBool b -> VBool $ not b
  _       -> error "type error"
eval (ENeg _ n) = case eval n of
  VInt n' -> VInt (- n')
eval (EBinOp _ op e1 e2) = 
  let v1 = eval e1
      v2 = eval e2
  in case (op, v1, v2) of
    (Plus, (VInt v1'), (VInt v2')) -> VInt (v1' + v2')
    (Minus, (VInt v1'), (VInt v2')) -> VInt (v1' - v2')
    (Mult, (VInt v1'), (VInt v2')) -> VInt (v1' * v2')
    (Div, (VInt v1'), (VInt v2')) -> VInt (v1' `div` v2')