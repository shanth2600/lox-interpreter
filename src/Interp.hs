module Interp where

import AST ( Exp (..), Op (..), Statement (..))
import Text.Parsec (SourcePos)
import Parser (testParse)
import Data.List.Split (splitOn)
import Data.List (dropWhileEnd, intercalate)
import Text.Printf (printf)
import Text.Parsec.Pos (sourceLine)
import Control.Monad.Except (Except, runExcept)
import System.Exit (die, exitWith, ExitCode (..))
import Control.Monad.Error.Class (throwError)
import System.IO (hPutStrLn, stderr)

type LineNumber = Int

type Eval = Except EvalError

data EvalError = EvalError SourcePos String
  deriving Eq

instance Show EvalError where
  show (EvalError p expected) = 
    printf "Operand must be %s.\n[line %d]" expected (sourceLine p)

data Val a = 
    VNum a Float
  | VBool a Bool
  | VFloat a String
  | VNil a
  | VString a String

instance Eq (Val a) where
  (VNum _ f1)    == (VNum _ f2)    = f1 == f2
  (VBool _ b1)   == (VBool _ b2)   = b1 == b2
  (VFloat _ f1)  == (VFloat _ f2)  = f1 == f2
  (VNil _)       == (VNil _)       = True
  (VString _ s1) == (VString _ s2) = s1 == s2
  _ == _ = False

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
      _         -> error $ printf "malformed number: (%s)" nStr  
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
    _      -> throwEvalErr p "a bool"
runEval (ENeg p n)          = do
  vNum <- runEval n
  case vNum of
    VNum _ n' -> return $ VNum p (- n')
    _ -> throwEvalErr p "a number"
runEval (EGroup _ e)        = runEval e
runEval (EBinOp p op e1 e2) = do
  v1 <- runEval e1
  v2 <- runEval e2
  case (op, v1, v2) of
    (Equal, v1, v2) -> return $ VBool (valPos v1) (v1 == v2)
    (NotEqual, v1, v2) -> return $ VBool (valPos v1) (v1 /= v2)
    (Plus, (VNum p v1'), (VNum _ v2')) -> return $ VNum p (v1' + v2')
    (Plus, (VString p v1'), (VString _ v2')) -> return $ VString p (v1' ++ v2')
    (Plus, _, _) -> throwEvalErr p "two numbers or two strings"
    (Minus, (VNum p v1'), (VNum _ v2')) -> return $ VNum p (v1' - v2')
    (Minus, _, _) -> throwEvalErr p "a number"
    (Mult, (VNum p v1'), (VNum _ v2')) -> return $ VNum p (v1' * v2')
    (Mult, _, _) -> throwEvalErr p "a number"
    (Div, (VNum p v1'), (VNum _ v2')) -> return $ VNum p (v1' / v2')
    (Div, _, _) -> throwEvalErr p "a number"
    (Greater, (VNum p v1'), (VNum _ v2')) -> return $ VBool p (v1' > v2')
    (Greater, _, _) -> throwEvalErr p "numbers"
    (Less, (VNum p v1'), (VNum _ v2')) -> return $ VBool p (v1' < v2')
    (Less, _, _) -> throwEvalErr p "numbers"
    (LessEqual, (VNum p v1'), (VNum _ v2')) -> return $ VBool p (v1' <= v2')
    (LessEqual, _, _) -> throwEvalErr p "numbers"
    (GreaterEqual, (VNum p v1'), (VNum _ v2')) -> return $ VBool p (v1' >= v2')
    (GreaterEqual, _, _) -> throwEvalErr p "numbers"


eval :: Exp SourcePos -> IO ()
eval exp = either (\e -> hPutStrLn stderr (show e) >> exitWith (ExitFailure 70)) (putStrLn . show) (runExcept $ runEval exp)

interp :: [Statement SourcePos] -> IO ()
interp (st:sts) =
  case st of
    (Print e) -> putStrLn (show e) >> interp sts


testEval :: String -> String    
testEval = either show show . runExcept . runEval . testParse