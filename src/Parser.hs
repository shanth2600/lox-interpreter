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

type ExpS = Exp SourcePos


type Parser = Parsec [T.Token SourcePos] ()

expr :: Parser ExpS
expr =
  try arithExp <|>
  eFloat       <|>
  eInt         <|> 
  eBool        <|> 
  eNil         <|>
  eString      <|>
  eNot         <|>
  eNegExpInt   <|>
  eNegExpFloat <|>
  eGroup

exprInt :: Parser ExpS
exprInt = (try numBinOp) <|> eInt

numBinOp :: Parser ExpS
numBinOp = do
  e1 <- eInt
  op' <- op
  e2 <- eInt
  return $ EBinOp (expPos e1) op' e1 e2

binOp :: SourcePos -> Parser (ExpS -> ExpS -> ExpS)
binOp n =  EBinOp n <$> op

op :: Parser Op
op =
  (token' (T.Plus ()) $> Plus) <|>
  (token' (T.Minus ()) $> Minus) <|>
  (token' (T.Star ()) $> Mult) <|>
  (token' (T.Slash ()) $> Div) <|>
  (token' (T.EqualEqual ()) $> Equal) <|>
  (token' (T.Greater ()) $> Greater) <|>
  (token' (T.GreaterEqual ()) $> GreaterEqual) <|>
  (token' (T.Less ()) $> Less) <|>
  (token' (T.LessEqual ()) $> LessEqual)

binOperand :: Parser ExpS
binOperand =
  try eNegExpFloat <|>
  try eNegExpInt   <|>
  eString          <|>
  numLit           <|>
  eGroup
 

numLit :: Parser ExpS
numLit = eInt <|> eFloat

arithExp :: Parser ExpS
arithExp = do
  opr <- (lookAhead binOperand)
  binOperand `chainl1` (binOp (expPos opr))

-- addOp :: Parser Op
-- addOp = 
--   (token' (T.Plus ()) $> Plus) <|>
--   (token' (T.Minus ()) $> Minus)

-- mulOp :: Parser Op
-- mulOp = 
--   (token' (T.Star ()) $> Mult) <|>
--   (token' (T.Slash ()) $> Div)

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

eNegExpInt :: Parser ExpS
eNegExpInt = do
  m <- token' $ T.Minus ()
  num <- try eInt
  pure $ ENeg (T.tokPos m) num

eNegExpFloat :: Parser ExpS
eNegExpFloat = do
  m <- token' $ T.Minus ()
  num <- try eFloat
  pure $ ENeg (T.tokPos m) num


eInt :: Parser ExpS
eInt = token show T.tokPos getInt
  where
    getInt (T.LNumber p nStr) = 
      case splitOn "." nStr of
        [int] -> 
          case readMaybe int of
            Nothing -> error ("cannot read int: " ++ (show int))
            Just i -> return $ EInt p i
        _     -> Nothing
    getInt _                  = Nothing

eFloat :: Parser ExpS
eFloat = token show T.tokPos getFloat
  where
    getFloat (T.LNumber p nStr) = 
      case splitOn "." nStr of
        [int,dec] -> 
          case ((map readMaybe [int,dec]) :: [Maybe Int]) of
            [Just int',Just dec'] ->
             return $ EFloat p int' dec'
        _     -> Nothing
    getFloat _                  = Nothing

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
testParse str = either (error . show) id (runParser arithExp () "" lexTokens)
  where
    lexTokens = [ tk | (L.LexToken tk) <- L.tokenize "" str]

parseTokens :: [T.Token SourcePos] -> Either ParseError ExpS
parseTokens = runParser expr () ""

