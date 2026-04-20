{-# LANGUAGE RankNTypes #-}
module Parser where

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

type LineNumber = Int

data LoxParseError = LoxParseError LineNumber String String

instance Show LoxParseError where
  show (LoxParseError ln tk msg) 
    = printf "[line %d] Error at '%s': %s." ln tk msg


type Parser = Parsec [T.Token SourcePos] ()

expr :: Parser ExpS
expr =
  arithAddExp     <|>
  eNum            <|>
  eBool           <|> 
  eNil            <|>
  eString         <|>
  eNot            <|>
  eNegExp         <|>
  eGroup


binOperand :: Parser ExpS
binOperand =
  try eNegExp <|>
  eBool       <|>
  eString     <|>
  eNum        <|>
  eGroup
 

addBinOp :: SourcePos -> Parser (ExpS -> ExpS -> ExpS)
addBinOp n =  EBinOp n <$> addOp

mulBinOp :: SourcePos -> Parser (ExpS -> ExpS -> ExpS)
mulBinOp n =  EBinOp n <$> mulOp

relBinOp :: SourcePos -> Parser (ExpS -> ExpS -> ExpS)
relBinOp n =  EBinOp n <$> relOp

addOp :: Parser Op
addOp = 
  (token' (T.Plus ()) $> Plus) <|>
  (token' (T.Minus ()) $> Minus)

mulOp :: Parser Op
mulOp = 
  (token' (T.Star ()) $> Mult) <|>
  (token' (T.Slash ()) $> Div)

relOp :: Parser Op  
relOp =
  (token' (T.BangEqual ())  $> NotEqual) <|>
  (token' (T.EqualEqual ()) $> Equal) <|>
  (token' (T.Greater ()) $> Greater) <|>
  (token' (T.GreaterEqual ()) $> GreaterEqual) <|>
  (token' (T.Less ()) $> Less) <|>
  (token' (T.LessEqual ()) $> LessEqual)


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
    lhs = arithRelExp <|> binOperand

arithRelExp :: Parser ExpS
arithRelExp = do
  opr <- (lookAhead binOperand)
  binOperand `chainl1` (relBinOp (expPos opr))

  
eString :: Parser ExpS
eString =  token show T.tokPos getStr
  where
    getStr (T.LString p str) = Just $ EString p str
    getStr _ = Nothing

eGroup :: Parser ExpS    
eGroup = do
  lp <- token' $ T.LeftParen ()
  e <- expr
  _ <- token' $ T.RightParen ()
  pure $ EGroup (T.tokPos lp) e

eNot :: Parser ExpS
eNot = do
  t <- token' $ T.Bang ()
  b <- expr
  pure $ ENot (T.tokPos t) b


eNegExp :: Parser ExpS
eNegExp = do
  m <- token' $ T.Minus ()
  num <- try (eNum <|> eGroup)
  pure $ ENeg (T.tokPos m) num

-- eNegExpFloat :: Parser ExpS
-- eNegExpFloat = do
--   m <- token' $ T.Minus ()
--   num <- try eFloat
--   pure $ ENeg (T.tokPos m) num


eNum :: Parser ExpS
eNum = token show T.tokPos getNum
  where
    getNum (T.LNumber p nStr) = Just $ ENum p (read nStr)
    getNum _                  = Nothing

reserved' :: String -> Parser (SourcePos, String)
reserved' rsvd = do 
  t <- token' $ T.Reserved () rsvd 
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

token' :: T.Token () -> Parser (T.Token SourcePos)
token' t = token show T.tokPos isToken
  where
    isToken t' = if t == void t' then Just t' else Nothing




testParse :: String -> Exp SourcePos
testParse str = either (error . show) id (runParser (expr) () "" lexTokens)
  where
    lexTokens = [ tk | (L.LexToken tk) <- L.tokenize "" str]

parseTokens :: [T.Token SourcePos] -> Either LoxParseError ExpS
parseTokens tks = mapLeft toLoxParseError (runParser expr () "" tks)

-- toLoxParseError :: ParseError ->
toLoxParseError pErr = 
    case errorMessages (pErr) of
      (m:_) -> 
        let char = T.tokenLiteral' $ messageString m
            lnNo = sourceLine $ errorPos pErr
        in LoxParseError lnNo char "Expect expression"
      [] -> error "unkown parse error at: " (show $ errorPos pErr)
  -- error $ show $ messageString $ head $ errorMessages (pErr)