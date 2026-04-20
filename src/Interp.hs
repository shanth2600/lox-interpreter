module Interp where

import AST ( Exp (..), Op (..))
import Text.Parsec (SourcePos)
import Parser (testParse)
import Data.List.Split (splitOn)
import Data.List (dropWhileEnd, intercalate)
import Text.Printf (printf)
import Text.Parsec.Pos (sourceLine)
import Control.Monad.Except (Except, runExcept)
import System.Exit (die)
import Control.Monad.Error.Class (throwError)

type LineNumber = Int

type Eval = Except EvalError

data EvalError = EvalError SourcePos String
  deriving Eq

instance Show EvalError where
  show (EvalError p expected) = 
    printf "Operand must be a %s.\n[line %d]" expected (sourceLine p)

data Val a = 
    VNum a Float
  | VBool a Bool
  | VFloat a String
  | VNil a
  | VString a String
  | VError a String
  deriving Eq

valPos :: Val a -> a
valPos (VNum a _)    = a
valPos (VBool a _)   = a
valPos (VFloat a _)  = a
valPos (VNil a)      = a
valPos (VString a _) = a

instance Show (Val a) where
  show (VNum _ n)      = displayNum n
  show (VBool _ True)  = "true"
  show (VBool _ False) = "false"
  show (VNil _)        = "nil"
  show (VString _ str) = str
  show (VFloat _ str)  = str

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

throwEvalErr :: SourcePos -> String -> Eval a
throwEvalErr p s = throwError $ EvalError p s

runEval :: Exp SourcePos -> Eval (Val SourcePos)
runEval (ENil p)            = return $ VNil p
runEval (ENum p n)          = return $ VNum p n
runEval (EBool p b)         = return $ VBool p b
runEval (EString p str)     = return $ VString p str
runEval (ENot p e)          = do
  e' <- runEval e 
  case e' of
    VBool p b -> return $  VBool p $ not b
    VNum p n  -> return $  VBool p (n == 0)
    VNil p    -> return $  VBool p True
    _      -> throwEvalErr p "bool"
runEval (ENeg p n)          = do
  vNum <- runEval n
  case vNum of
    VNum _ n' -> return $ VNum p (- n')
    _ -> throwEvalErr p "number"
runEval (EGroup _ e)        = runEval e
runEval (EBinOp _ op e1 e2) = do
  v1 <-  runEval e1
  v2 <- runEval e2
  case (op, v1, v2) of
    (Equal, v1, v2) -> return $ VBool (valPos v1) (v1 == v2)
    (NotEqual, v1, v2) -> return $ VBool (valPos v1) (v1 /= v2)
    (Plus, (VNum p v1'), (VNum _ v2')) -> return $ VNum p (v1' + v2')
    (Minus, (VNum p v1'), (VNum _ v2')) -> return $ VNum p (v1' - v2')
    (Mult, (VNum p v1'), (VNum _ v2')) -> return $ VNum p (v1' * v2')
    (Div, (VNum p v1'), (VNum _ v2')) -> return $ VNum p (v1' / v2')
    (Greater, (VNum p v1'), (VNum _ v2')) -> return $ VBool p (v1' > v2')
    (Less, (VNum p v1'), (VNum _ v2')) -> return $ VBool p (v1' < v2')
    (LessEqual, (VNum p v1'), (VNum _ v2')) -> return $ VBool p (v1' <= v2')
    (GreaterEqual, (VNum p v1'), (VNum _ v2')) -> return $ VBool p (v1' >= v2')
    (Plus, (VString p v1'), (VString _ v2')) -> return $ VString p (v1' ++ v2')


eval :: Exp SourcePos -> IO ()
eval exp = either (die . show) (putStrLn . show) (runExcept $ runEval exp)


testEval :: String -> String    
testEval = show . runEval . testParse