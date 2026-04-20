module Interp where

import AST ( Exp (..), Op (..))
import Text.Parsec (SourcePos)
import Parser (testParse)
import Data.List.Split (splitOn)
import Data.List (dropWhileEnd, intercalate)
import Text.Printf (printf)

data Val = 
    VNum Float
  | VBool Bool
  | VFloat String
  | VNil
  | VString String
  deriving Eq

instance Show Val where
  show (VNum n)      = displayNum n
  show (VBool True)  = "true"
  show (VBool False) = "false"
  show (VNil)        = "nil"
  show (VString str) = str
  show (VFloat str)  = str

displayNum :: Float -> String 
displayNum n = case splitOn "." nStr of
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
      
eval :: Exp SourcePos -> Val
eval (ENil _)            = VNil
eval (ENum _ n)          = VNum n
eval (EBool _ b)         = VBool b
eval (EString _ str)     = VString str
eval (ENot _ e)          = 
  case eval e of
    VBool b -> VBool $ not b
    VNum n  -> VBool (n == 0)
    VNil    -> VBool True
    _      -> error "type error"
eval (ENeg _ n)          = case eval n of
  VNum n' -> VNum (- n')
eval (EGroup _ e)        = eval e
eval (EBinOp _ op e1 e2) = 
  let v1 = eval e1
      v2 = eval e2
  in case (op, v1, v2) of
    (Plus, (VNum v1'), (VNum v2')) -> VNum (v1' + v2')
    (Minus, (VNum v1'), (VNum v2')) -> VNum (v1' - v2')
    (Mult, (VNum v1'), (VNum v2')) -> VNum (v1' * v2')
    (Div, (VNum v1'), (VNum v2')) -> VNum (v1' / v2')


testEval :: String -> String    
testEval = show . eval . testParse