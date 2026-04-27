{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
module Interp where

import Lib
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Env as E
import Numeric (showFFloat)

import qualified Data.List.NonEmpty as NE

import AST ( Exp (..), Op (..), Statement (..), expVars)
import Text.Parsec (SourcePos)
import Parser (testParse, testProgParse)
import Data.List.Split (splitOn)
import Data.List (dropWhileEnd, intercalate, intersect, group, sort)
import Text.Printf (printf)
import Text.Parsec.Pos (sourceLine)
import Control.Monad.Except (Except, runExcept, ExceptT (..), runExceptT)
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
import Data.Maybe (fromMaybe, catMaybes)
import Control.Monad (when)
import Data.Bifunctor (first, second, Bifunctor (bimap))
import Control.Monad.Extra (ifM)
import Control.Monad ((>=>))
import Data.Either (fromRight)
import Control.Monad (void)
import GHC.Integer (leInteger)




data EvalError = 
    EvalError SourcePos String
  | VarNotFound SourcePos Ident
  | DeclError SourcePos Ident String
  | ExpectedFunError SourcePos
  | MsgError SourcePos String
  | RawError String
  deriving Eq

instance Show EvalError where
  show (EvalError p expected) = 
    printf "Operand must be %s.\n[line %d]" expected (sourceLine p)
  show (VarNotFound p var) = 
    printf "Undefined variable '%s'.\n[line %d]" var (sourceLine p)
  show (DeclError p var msg) = 
    printf "Error at '%s': %s.\n[line %d]" var msg (sourceLine p)
  show (MsgError p str) = 
    printf "%s.\n[line %d]" str (sourceLine p)
  show (RawError str) = str


type Env = E.Env Ident (Val SourcePos)
type Scope = E.Scope Ident (Val SourcePos)


type Interp a = ExceptT EvalError (StateT Env IO) a

throwEvalErr :: SourcePos -> String -> Interp a
throwEvalErr p s = throwError $ EvalError p s

throwDeclErr :: SourcePos -> Ident -> String -> Interp a
throwDeclErr p var msg = throwError $ DeclError p var msg

throwFunErr :: SourcePos -> Interp a
throwFunErr p = throwError $ MsgError p "Can only call functions and classes"

throwMsgErr :: SourcePos -> String -> Interp a
throwMsgErr p = throwError . MsgError p

throwRawErr :: String -> Interp a
throwRawErr = throwError . RawError

throwVarError :: SourcePos -> Ident -> Interp a
throwVarError p id' = throwError $ VarNotFound p id'

defineVariable :: Ident -> Val SourcePos -> Interp ()
defineVariable id' val = modify (E.declareVariable id' val)

defineVariables :: [(Ident, Val SourcePos)] -> Interp ()
defineVariables = modify . E.declareVariables

getGlobalScope :: Interp (Scope)
getGlobalScope = gets E.globalScope

modifyGlobalScope :: Scope -> Interp ()
modifyGlobalScope scope = modify (E.insertGlobalScope scope)

enterNewScope :: Interp ()
enterNewScope = modify (E.pushNewScope E.emptyScope)

leaveScope :: Interp ()
leaveScope = do
  gets E.popScope >>=
    maybe (throwRawErr "Trying to leave global scope.")
           put

amInGlobalScope :: Interp Bool
amInGlobalScope = do
  (E.Env scopes) <- get 
  return (NE.length scopes == 1)

inLocalScope :: Interp a -> Interp a
inLocalScope act = do
  enterNewScope
  res <- act
  leaveScope
  return res



assignVariable :: Ident -> Val SourcePos -> Interp ()
assignVariable id' val =
  gets (E.assignToVariable id' val) >>=
    maybe (throwVarError (valPos val) id')
          put
        


lookupVar :: SourcePos -> Ident -> Interp (Val SourcePos)
lookupVar p id' = 
  gets (E.lookupVariable id') >>=
    maybe (throwVarError p id')
          return

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
valPos (VNum a _)             = a
valPos (VBool a _)            = a
valPos (VFloat a _)           = a
valPos (VNil a)               = a
valPos (VString a _)          = a
valPos (VClosure a _ _ _ _)   = a

instance Show (Val a) where
  show :: Val a -> String
  show (VNum _ n)      = displayNum $ showFFloat Nothing n ""
  show (VBool _ True)  = "true"
  show (VBool _ False) = "false"
  show (VNil _)        = "nil"
  show (VClosure _ funId _ body _) = printf "<fn %s>" funId
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
runEval (EFunCall p (EVar _ "clock") []) = do
  t <- liftIO $ getPOSIXTime
  return (VNum p (realToFrac t))
runEval (EFunCall p fun args) = do
  closure <- runEval fun
  env <- get
  case closure of
    (VClosure p funId params body env) -> do
      args' <- mapM runEval args 
      when (length params /= length args) (throwFunErr p)
      (v,env') <- withFunctionEnv env $ do
            defineVariables (zip params args')
            interpStatement body
      assignVariable funId  (VClosure p funId params body env')
      either return (const $ return (VNil p)) v
    _ -> throwFunErr p
  where
    withFunctionEnv :: Env -> Interp a -> Interp (a, Env)
    withFunctionEnv closureEnv action = do
      oldEnv <- get
      put (E.insertGlobalScope (E.globalScope oldEnv) closureEnv)
      result <- action
      closureEnv' <- get
      let newEnv = E.insertGlobalScope (E.globalScope closureEnv') oldEnv
      put newEnv
      pure (result, closureEnv')
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
      assignVariable id' r'
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


continue ::  Interp (Either (Val SourcePos) ())
continue = return $ Right ()


interpStatement :: Statement SourcePos -> Interp (Either (Val SourcePos) ())
interpStatement (Return p Nothing)  = return $ Left (VNil p)
interpStatement (Return p (Just e)) = Left <$> runEval e
interpStatement (Print p e) = 
  runEval e >>= liftIO . putStrLn . show >> continue
interpStatement (ExpSt p e) = 
  runEval e >> continue
interpStatement (VarDecl p id' e) = do
  v <- maybe (return $ VNil p) runEval e
  case v of
    (VClosure p funId args body env) ->
      defineVariable id' (VClosure p id' args body env) >> continue
    _ ->
      defineVariable id' v >> continue
interpStatement (Block _ sts) = inLocalScope $ interpStatements sts
interpStatement (If p pred then' else') = inLocalScope $ do
  pred' <- runEval pred
  if (truthy pred') 
    then interpStatement then' 
    else (maybe continue interpStatement else')
interpStatement (While p pred body) = inLocalScope go
  where
    go = do
      pred' <- runEval pred
      if(truthy pred') 
        then handleReturn (interpStatement body) go
        else continue
interpStatement (For p (init,pred,step) body) =
  inLocalScope $ maybe continue interpStatement init >> go
  where
    go = do
      pred' <- runEval pred
      if truthy pred' 
      then handleReturn 
            (interpStatement body) 
            ((maybe (return $ VNil p) runEval step) >> go)
      else continue
interpStatement (FunDecl p funId args body) = do
  env <- get
  defineVariable funId (VClosure p funId args body env)
  continue

lintStatemnts :: [Statement SourcePos] -> Interp ()
lintStatemnts sts = mapM_ lintStatement sts >> put (E.emptyEnv)

lintStatement :: Statement SourcePos -> ExceptT EvalError (StateT Env IO) ()
lintStatement (VarDecl p id' e) = do
  guardCirularInit id' e
  guardRedeclaration id'
  defineVariable id' (VNil p)
  where
    guardRedeclaration :: Ident -> Interp ()
    guardRedeclaration id' =
      ifM ((&&) <$> (gets $ E.existsInCurrentScope id') <*> (not <$> amInGlobalScope))
          (throwDeclErr p id' "Already a variable with this name in this scope")
          (return ())
    exprContains :: Ident -> Exp SourcePos -> Bool
    exprContains id' ex = id' `elem` (expVars ex)
    guardCirularInit :: Ident -> Maybe (Exp SourcePos) -> Interp ()
    guardCirularInit l r = do
      amInGlobalScope 
      ifM (not <$> amInGlobalScope)
          (case r of
            Just e -> 
              if l `exprContains` e 
                then throwDeclErr p id' "Can't read local variable in its own initializer" 
                else return ()
            Nothing -> return ())
          (return ())
lintStatement (FunDecl p funId args body) = do
  guardArgNameConflict
  guardDeclaration
  where
    funVarDecls :: Statement SourcePos -> [Ident]
    funVarDecls (VarDecl _ id' _) = [id']
    funVarDecls (Block n sts) = [ id' | (VarDecl _ id' _) <- sts]
    guardArgNameConflict :: Interp ()
    guardArgNameConflict = 
      if (not . null) argConflicts
        then throwDeclErr p (head argConflicts) "Already a variable with this name in this scope"
        else return ()
        where
          argConflicts = concat $ catMaybes $ map tailMay (group $ sort args)
    guardDeclaration :: Interp ()
    guardDeclaration = 
      if (not . null) conflicts
        then throwDeclErr p (head conflicts) "Already a variable with this name in this scope"
        else return ()
      where
        conflicts = args `intersect` (funVarDecls body)
lintStatement (Block p sts) = 
  inLocalScope $ mapM_ lintStatement sts
lintStatement _ = return ()

  

interpStatements :: [Statement SourcePos] -> Interp (Either (Val SourcePos) ())
interpStatements = runExceptT . mapM_ (ExceptT . interpStatement)
  
  
handleReturn :: Interp (Either (Val SourcePos) ()) -> Interp (Either (Val SourcePos) ()) -> Interp (Either (Val SourcePos) ())
handleReturn action cont = action >>= (either (return . Left) (const $ cont))



eval :: Exp SourcePos -> IO ()
eval exp =
  (flip evalStateT E.emptyEnv . runExceptT $ runEval exp) >>=
    either 
      (\e -> hPutStrLn stderr (show e) >> exitWith (ExitFailure 70))
      (putStrLn . show)

runInterp :: [Statement SourcePos] -> ExceptT EvalError (StateT Env IO) ()
runInterp sts =  lintStatemnts sts  >> mapM_ interpStatement sts
      


interp :: [Statement SourcePos] -> IO ()
interp sts = do
  (flip evalStateT E.emptyEnv . runExceptT $ runInterp sts) >>=
    either 
      (\e -> hPutStrLn stderr (show e) >> exitOn e)
      return

exitOn :: EvalError -> IO ()
exitOn = \case
  DeclError _ _ _ -> exitWith (ExitFailure 65)
  _ -> exitWith (ExitFailure 70)



testEval :: String -> IO ()
testEval = eval . testParse

testInterp :: String -> IO ()
testInterp =  interp . testProgParse

interpFile :: FilePath -> IO ()
interpFile fp = readFile fp >>= testInterp