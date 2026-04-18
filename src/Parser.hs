{-# LANGUAGE RankNTypes #-}
module Parser where

import Text.Parsec
import Text.Parsec.Pos

import qualified Token as T
import qualified Lexer as L
import AST
import Data.Functor (void, ($>))
import Data.List.Split (splitOn)
import Debug.Trace (trace)
import Text.Read (readMaybe)


type Parser = Parsec [T.Token SourcePos] ()

expr :: Parser (SomeExp SourcePos)
expr = 
  exprInt <|> 
  (SomeExp <$> eBool) <|> 
  (SomeExp <$> eNil) <|>
  (SomeExp <$> eFloat) <|>
  (SomeExp <$> eString) <|>
  (SomeExp <$> eNot) <|>
  (SomeExp <$> eNegExpInt) <|>
  (SomeExp <$> eNegExpFloat) <|>
  eGroup

exprInt :: Parser (SomeExp SourcePos)
exprInt = SomeExp <$> ((try numBinOp) <|> eInt)

numBinOp :: Parser (Exp SourcePos Int)
numBinOp = do
  e1 <- eInt
  op' <- op
  e2 <- eInt
  return $ EBinOp (expPos e1) op' e1 e2

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

eString :: Parser (Exp SourcePos String)
eString =  token show T.tokPos getStr
  where
    getStr (T.LString p str) = Just $ EString p str
    getStr _ = Nothing

eGroup :: Parser (SomeExp SourcePos)    
eGroup = do
  (T.LeftParen p) <- token' $ T.LeftParen ()
  (SomeExp e) <- expr
  (T.RightParen p) <- token' $ T.RightParen ()
  pure $ SomeExp $ EGroup p e

eNot :: Parser (Exp SourcePos Bool)
eNot = do
  t <- token' $ T.Bang ()
  b <- try eBool <|> eNot
  pure $ ENot (T.tokPos t) b

eNegExpInt :: Parser (Exp SourcePos Int)
eNegExpInt = do
  m <- token' $ T.Minus ()
  num <- try eInt
  pure $ ENeg (T.tokPos m) num

eNegExpFloat :: Parser (Exp SourcePos Float)
eNegExpFloat = do
  m <- token' $ T.Minus ()
  num <- try eFloat
  pure $ ENeg (T.tokPos m) num


eInt :: Parser (Exp SourcePos Int)
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

eFloat :: Parser (Exp SourcePos Float)
eFloat = token show T.tokPos getInt
  where
    getInt (T.LNumber p nStr) = 
      case splitOn "." nStr of
        [int,dec] -> 
          case ((map readMaybe [int,dec]) :: [Maybe Int]) of
            [Just int',Just dec'] ->
             return $ EFloat p int' dec'
        _     -> Nothing
    getInt _                  = Nothing

reserved' :: String -> Parser (SourcePos, String)
reserved' rsvd = do 
  (T.Reserved n _) <- token' $ T.Reserved () rsvd 
  return (n,rsvd)

eNil :: Parser (Exp SourcePos a)
eNil = ENil <$> (fst <$> reserved' "nil")

eBool :: Parser (Exp SourcePos Bool)    
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




testParse :: String -> T.Token SourcePos
testParse str = either (error . show) id (runParser (token' (T.EOF ())) () "" lexTokens)
  where
    lexTokens = [ tk | (L.LexToken tk) <- L.tokenize "" str]

parseTokens :: [T.Token SourcePos] -> Either ParseError (SomeExp SourcePos)
parseTokens = runParser expr () ""

