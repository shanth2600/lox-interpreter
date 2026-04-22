{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE InstanceSigs #-}
module Interp where

import Lib
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Env as E
import Numeric (showFFloat)

import AST ( Exp (..), Op (..), Statement (..))
import Text.Parsec (SourcePos)
import Parser (testParse, testProgParse)
import Data.List.Split (splitOn)
import Data.List (dropWhileEnd, intercalate)
import Text.Printf (printf)
import Text.Parsec.Pos (sourceLine)
import Control.Monad.Except (Except, runExcept, ExceptT, runExceptT)
import System.Exit (die, exitWith, ExitCode (..))
import Control.Monad.Error.Class (throwError)
import System.IO (hPutStrLn, stderr)
import qualified Data.Map as M
import Control.Monad.State.Strict (State, runState, evalState)
import Control.Monad.State (modify, MonadIO (liftIO), MonadState (get, put))
import Data.Functor (($>), (<&>))
import Control.Monad.State.Strict (StateT)
import Control.Monad.State.Strict (evalStateT)
import Control.Monad.State.Strict (gets)
import Debug.Trace (trace)
import Data.Maybe (fromMaybe)


-- data InterpError = VarNotFound SourcePos Ident

type Env = E.Env Ident (Val SourcePos)

data EvalError = 
    EvalError SourcePos String
  | VarNotFound SourcePos Ident
  deriving Eq

instance Show EvalError where
  show (EvalError p expected) = 
    printf "Operand must be %s.\n[line %d]" expected (sourceLine p)
  show (VarNotFound p var) = 
    printf "Undefined variable '%s'.\n[line %d]" var (sourceLine p)




type Interp a = ExceptT EvalError (StateT Env IO) a

throwEvalErr :: SourcePos -> String -> Interp a
throwEvalErr p s = throwError $ EvalError p s

throwVarError :: SourcePos -> Ident -> Interp a
throwVarError p id' = throwError $ VarNotFound p id'

addBinding :: Ident -> Val SourcePos -> Interp ()
addBinding id' = modify . E.pushValue id'

modifyBinding :: Ident -> Val SourcePos -> Interp ()
modifyBinding id' val = do
  guardVariableExists (valPos val) id'
  modify (E.popValue id')
  addBinding id' val

purgeVarsFromScope :: [Ident] -> Interp ()
purgeVarsFromScope localVars = modify (E.popValues localVars)

withLocalScope :: Env -> Interp a -> Interp a
withLocalScope env action = do
  currEnv <- get
  put env
  res <- action
  put currEnv
  return res

lookupVar :: SourcePos -> Ident -> Interp (Val SourcePos)
lookupVar p id' = do
  res <- gets (E.peekValue id')
  case res of
    Just val -> return val
    _        -> throwVarError p id'

guardVariableExists :: SourcePos -> Ident -> Interp ()
guardVariableExists p id' = lookupVar p id' >> return ()

data Val a = 
    VNum a Double
  | VBool a Bool
  | VFloat a String
  | VNil a
  | VClosure a Ident [Ident] (Statement a) Env
  | VString a String

truthy :: Val a -> Bool
truthy (VBool _ False) = False
truthy (VNil _)        = False
truthy _               = True

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
  show :: Val a -> String
  show (VNum _ n)      = displayNum $ showFFloat Nothing n ""
  show (VBool _ True)  = "true"
  show (VBool _ False) = "false"
  show (VNil _)        = "nil"
  show (VClosure _ funId _ _ _) = printf "<fn %s>" funId
  show (VString _ str) = str
  show (VFloat _ str)  = str

displayNum :: String -> String 
displayNum nStr = case splitOn "." nStr of
      [int,dec] ->
        if dec == "0"
          then int
          else intercalate "." [int, truncatedDec dec]
      _         -> error $ printf "malformed number: (%s)" nStr  
  where
    truncatedDec dec = 
      let dec' = dropWhileEnd (== '0') dec
      in if null dec' then "0" else dec'

runEval :: Exp SourcePos -> Interp (Val SourcePos)
runEval (EVar p id')        = lookupVar p id'
runEval (ENil p)            = return $ VNil p
runEval (ENum p n)          = return $ VNum p n
runEval (EBool p b)         = return $ VBool p b
runEval (EString p str)     = return $ VString p str
runEval (EFunCall p "clock" []) = do
  t <- liftIO $ getPOSIXTime
  return (VNum p (realToFrac t))
runEval (EFunCall p funId _) = do
  closure <- lookupVar p funId
  case closure of
    (VClosure p _ args body env) -> do
      (withLocalScope env $
        interpStatement body) >> (return $ VNil p)
    _ -> error "function not found"
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
runEval (EGroup _ e)          = runEval e
runEval (EBinOp p Assign l r) = do
  r' <- runEval r
  case l of
    EVar p id' -> do
      modifyBinding id' r'
      return r'
    _ -> throwEvalErr p "variable"
  return r'
runEval (EBinOp p And e1 e2) = do
  v1 <- runEval e1
  case v1 of
    v1 | not (truthy v1) -> return v1
       | otherwise       -> runEval e2
runEval (EBinOp p Or e1 e2) = do
  v1 <- runEval e1
  case v1 of
    v1 | truthy v1 -> return v1
       | otherwise -> do
        v2 <- runEval e2
        if (truthy v2) then return v2 else return (VBool p False)
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
eval exp =
  (flip evalStateT (M.empty) . runExceptT $ runEval exp) >>=
    either 
      (\e -> hPutStrLn stderr (show e) >> exitWith (ExitFailure 70))
      (putStrLn . show)
      


interp :: [Statement SourcePos] -> IO ()
interp sts = 
  (flip evalStateT (M.empty) . runExceptT $ mapM_ interpStatement sts) >>=
    either 
      (\e -> hPutStrLn stderr (show e) >> exitWith (ExitFailure 70))
      return

  

interpStatement :: Statement SourcePos -> Interp ()
interpStatement (Print _ e) = 
  runEval e >>= liftIO . putStrLn . show
interpStatement (ExpSt _ e) = 
  runEval e $> ()
interpStatement (VarDecl p id' e) =
  (maybe (return $ VNil p) runEval e >>= addBinding id') $> ()
interpStatement (Block p sts) = interpBlock [] sts
interpStatement (If p pred then' else') = do
  pred' <- runEval pred
  if (truthy pred') 
    then interpStatement then' 
    else (maybe (return ()) interpStatement else')
interpStatement (While p pred body) = go
  where
    go = do
      pred' <- runEval pred
      if(truthy pred') 
        then interpStatement body >> go
        else return ()
interpStatement (For p (init,pred,step) body) =
  maybe (return ()) interpStatement init >> go
  where
    go = do
      pred' <- runEval pred
      if truthy pred' 
      then 
        interpStatement body >> 
        (maybe (return $ VNil p) runEval step) >> go
      else return ()
interpStatement (FunDecl n funId args body) = do
  env <- get
  addBinding funId (VClosure n funId args body env)

interpBlock :: [Ident] -> [Statement SourcePos] -> Interp ()
interpBlock localVars [] = purgeVarsFromScope localVars
interpBlock localVars (decl@(VarDecl _ id' _): rest) = 
  interpStatement decl >> interpBlock (id' : localVars) rest
interpBlock localVars (st:rest) =
  interpStatement st >> interpBlock localVars  rest
      


testEval :: String -> IO ()
testEval = eval . testParse

testInterp :: String -> IO ()
testInterp =  interp . testProgParse