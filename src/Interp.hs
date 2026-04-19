module Interp where

import AST ( Exp (..), Op (..), displayFloat)
import Text.Parsec (SourcePos)
import Parser (testParse)

data Val = 
    VInt Int
  | VBool Bool
  | VFloat String
  | VNil
  | VString String
  deriving Eq

instance Show Val where
  show (VInt n)      = show n
  show (VBool True)  = "true"
  show (VBool False) = "false"
  show (VNil)        = "nil"
  show (VString str) = str
  show (VFloat str)  = str

eval :: Exp SourcePos -> Val
eval (ENil _)            = VNil
eval (EInt _ n)          = VInt n
eval f@(EFloat {})       = VFloat (displayFloat f)
eval (EBool _ b)         = VBool b
eval (EString _ str)     = VString str
eval (ENot _ e)          = 
  case eval e of
    VBool b -> VBool $ not b
    VInt n  -> VBool (n > 0)
    VNil    -> VBool True
    _      -> error "type error"
eval (ENeg _ n)          = case eval n of
  VInt n' -> VInt (- n')
eval (EGroup _ e)        = eval e
eval (EBinOp _ op e1 e2) = 
  let v1 = eval e1
      v2 = eval e2
  in case (op, v1, v2) of
    (Plus, (VInt v1'), (VInt v2')) -> VInt (v1' + v2')
    (Minus, (VInt v1'), (VInt v2')) -> VInt (v1' - v2')
    (Mult, (VInt v1'), (VInt v2')) -> VInt (v1' * v2')
    (Div, (VInt v1'), (VInt v2')) -> VInt (v1' `div` v2')


testEval :: String -> String    
testEval = show . eval . testParse