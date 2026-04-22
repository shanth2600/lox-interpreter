{-# LANGUAGE RankNTypes #-}
module Parser where

import Lib

import Text.Parsec
import Text.Parsec.Pos
import Text.Parsec.Expr

import qualified Token as T
import qualified Lexer as L
import AST
import Data.Functor (void, ($>))
import Data.List.Split (splitOn)
import Debug.Trace (trace)
import Text.Read (readMaybe)
import Text.Parsec.Expr (OperatorTable)
import Control.Monad.Identity
import Text.Parsec.Error (errorMessages, messageString)
import Text.Printf (printf)
import Data.Either.Extra (mapLeft)

type ExpS = Exp SourcePos


data LoxParseError = LoxParseError LineNumber String String

instance Show LoxParseError where
  show (LoxParseError ln tk msg) 
    = printf "[line %d] Error at '%s': %s." ln tk msg


type Parser = Parsec [T.Token SourcePos] ()

statement' :: Parser (Statement SourcePos)
statement' = 
  (singleStatment <* token' T.Semicolon) <|>
  ifStatement <|>
  whileLoop <|>
  forLoop <|>
  block'

singleStatment :: Parser (Statement SourcePos)
singleStatment = (do
  _ <- reserved' "print"
  Print <$> getPosition <*> expr) <|>
  try varDecl <|>
  ExpSt <$> getPosition <*> expr

ifStatement :: Parser (Statement SourcePos)
ifStatement = do
  (p,_) <- reserved' "if"
  pred <- eGroup
  then' <- statement'
  else' <- optionMaybe $ do 
            _ <- reserved' "else"
            statement'
  return $ If p pred then' else'

whileLoop :: Parser (Statement SourcePos)
whileLoop = do
 (p,_) <- reserved' "while"
 pred <- eGroup
 body <- statement'
 return $ While p pred body

forLoop :: Parser (Statement SourcePos)
forLoop = do
  (p,_) <- reserved' "for"
  c@(init,cond,step) <- 
    between 
      (token' T.LeftParen)
      (token' T.RightParen)
      ((,,) <$> 
        (optionMaybe singleStatment) <* token' T.Semicolon <*> 
        expr <* token' T.Semicolon  <*> 
        optionMaybe expr)
  body <- statement'
  return $ For p c body

block' :: Parser (Statement SourcePos)
block' = do
  open <- (lookAhead $ token' T.LeftBrace)
  Block (T.tokPos open) <$>
    (between (token' T.LeftBrace) 
             (token' T.RightBrace)
             (many statement'))



varDecl :: Parser (Statement SourcePos)
varDecl = do
  _ <- reserved' "var"
  (EVar p id') <- eVar
  e <- ((token' T.Equal *> (Just <$> expr)) <|> pure Nothing)
  return $ VarDecl p id' e

expr :: Parser ExpS
expr =
  assmtExp <|> 
  eNum        <|>
  eBool       <|> 
  eNil        <|>
  eVar        <|>
  eString     <|>
  eNot        <|>
  eNegExp     <|>
  eGroup


binOperand :: Parser ExpS
binOperand =
  try eNegExp <|>
  eBool       <|>
  eVar        <|>
  eString     <|>
  eNum        <|>
  eNil        <|>
  eGroup

 

addBinOp :: SourcePos -> Parser (ExpS -> ExpS -> ExpS)
addBinOp n =  EBinOp n <$> addOp

mulBinOp :: SourcePos -> Parser (ExpS -> ExpS -> ExpS)
mulBinOp n =  EBinOp n <$> mulOp

relBinOp :: SourcePos -> Parser (ExpS -> ExpS -> ExpS)
relBinOp n =  EBinOp n <$> relOp

boolBinOp :: SourcePos -> Parser (ExpS -> ExpS -> ExpS)
boolBinOp n = EBinOp n <$> boolOp

assmtBinOp :: SourcePos -> Parser (ExpS -> ExpS -> ExpS)
assmtBinOp n =  EBinOp n <$> ((token' T.Equal) $> Assign)

addOp :: Parser Op
addOp = 
  (token' T.Plus $> Plus) <|>
  (token' T.Minus $> Minus)

mulOp :: Parser Op
mulOp = 
  (token' T.Star $> Mult) <|>
  (token' T.Slash $> Div)

relOp :: Parser Op  
relOp =
  (token' T.BangEqual  $> NotEqual) <|>
  (token' T.EqualEqual $> Equal) <|>
  (token' T.Greater $> Greater) <|>
  (token' T.GreaterEqual $> GreaterEqual) <|>
  (token' T.Less $> Less) <|>
  (token' T.LessEqual $> LessEqual)

boolOp :: Parser Op
boolOp = 
  (reserved' "and" $> And) <|>
  (reserved' "or" $> Or)

boolExp :: Parser ExpS
boolExp = do
  lhs' <- (lookAhead lhs)
  lhs `chainr1` (boolBinOp (expPos lhs'))
    where
    lhs = binOperand


arithAddExp :: Parser ExpS
arithAddExp = do
  lhs' <- lookAhead lhs
  lhs `chainl1` (addBinOp (expPos lhs'))
  where
    lhs = arithMulExp <|> binOperand

arithMulExp :: Parser ExpS
arithMulExp = do
  lhs' <- lookAhead lhs
  lhs `chainl1` (mulBinOp (expPos lhs'))
  where
    lhs = boolExp <|> binOperand

arithRelExp :: Parser ExpS
arithRelExp = do
  lhs' <- (lookAhead lhs)
  lhs `chainl1` (relBinOp (expPos lhs'))
    where
    lhs = arithAddExp <|> binOperand

assmtExp :: Parser ExpS
assmtExp = do
  lhs' <- (lookAhead lhs)
  lhs `chainr1` (assmtBinOp (expPos lhs'))
    where
    lhs = arithRelExp <|> (eVar <|> eNum)

  
eString :: Parser ExpS
eString =  token show T.tokPos getStr
  where
    getStr (T.LString p str) = Just $ EString p str
    getStr _ = Nothing

eVar :: Parser ExpS
eVar =  token show T.tokPos getId
  where
    getId (T.Ident p id') = Just $ EVar p id'
    getId _ = Nothing

eGroup :: Parser ExpS    
eGroup = do
  lp <- token' T.LeftParen
  e <- expr
  _ <- token' T.RightParen
  pure $ EGroup (T.tokPos lp) e

eNot :: Parser ExpS
eNot = do
  t <- token' T.Bang
  b <- expr
  pure $ ENot (T.tokPos t) b


eNegExp :: Parser ExpS
eNegExp = do
  m <- token' T.Minus
  num <- try (eNum <|> eGroup)
  pure $ ENeg (T.tokPos m) num

endExp :: Parser ()
endExp = eof <|> (void $ token' T.Semicolon)

eNum :: Parser ExpS
eNum = token show T.tokPos getNum
  where
    getNum (T.LNumber p nStr) = Just $ ENum p (read nStr)
    getNum _                  = Nothing

reserved' :: String -> Parser (SourcePos, String)
reserved' rsvd = do 
  t <- token' $ flip T.Reserved rsvd 
  return (T.tokPos t, rsvd)

eNil :: Parser ExpS
eNil = ENil <$> (fst <$> reserved' "nil")

eBool :: Parser ExpS    
eBool = do 
  (n,b) <- (reserved' "true") <|> (reserved' "false")
  pure $ EBool n (litToBool b)
  where
    litToBool :: String -> Bool
    litToBool "true"  = True
    litToBool "false" = False
    litToBool _       = undefined

token' :: (() -> T.Token ()) -> Parser (T.Token SourcePos)
token' t = token show T.tokPos getToken
  where
    getToken t' = if (t ()) == void t' then Just t' else Nothing

eof' :: Parser ()
eof' = void $ token' T.EOF



testParse :: String -> Exp SourcePos
testParse str = either (error . show) id (runParser (expr) () "" lexTokens)
  where
    lexTokens = [ tk | (L.LexToken tk) <- L.tokenize "" str]

testProgParse :: String -> [Statement SourcePos]
testProgParse str = either (error . show) id (runParser (many statement') () "" lexTokens)
  where
    lexTokens = [ tk | (L.LexToken tk) <- L.tokenize "" str]

parseTokens :: [T.Token SourcePos] -> Either LoxParseError ExpS
parseTokens tks = mapLeft toLoxParseError (runParser expr () "" tks)

parseProgram :: [T.Token SourcePos] -> Either LoxParseError [Statement SourcePos]
parseProgram tks = mapLeft toLoxParseError (runParser (many statement') () "" tks)


toLoxParseError :: ParseError -> LoxParseError
toLoxParseError pErr = 
    case errorMessages (pErr) of
      (m:_) -> 
        let char = T.tokenLiteral' $ messageString m
            lnNo = sourceLine $ errorPos pErr
        in LoxParseError lnNo char "Expect expression"
      [] -> error "unkown parse error at: " (show $ errorPos pErr)
