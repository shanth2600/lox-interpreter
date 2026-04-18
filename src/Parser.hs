{-# LANGUAGE RankNTypes #-}
module Parser where

import Text.Parsec
import Text.Parsec.Pos

import qualified Token as T
import qualified Lexer as L
import AST
import Data.Functor (void, ($>))


type Parser = Parsec [T.Token SourcePos] ()

expr :: Parser (SomeExp SourcePos)
expr = exprInt <|> (SomeExp <$> eBool) <|> (SomeExp <$> eNil)

exprInt :: Parser (SomeExp SourcePos)
exprInt = SomeExp <$> (numBinOp <|> eInt)

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

eInt :: Parser (Exp SourcePos Int)
eInt = token show T.tokPos getInt
  where
    getInt (T.LNumber p nStr) = Just (EInt p (read nStr))
    getInt _                  = Nothing

reserved' :: String -> Parser (SourcePos, String)
reserved' rsvd = do 
  (T.Reserved n _) <- token' $ T.Reserved () rsvd 
  return (n,rsvd)

eNil :: Parser (Exp SourcePos a)
eNil = ENil <$> (fst <$> reserved' "nil")

eBool :: Parser (Exp SourcePos Bool)    
eBool = do 
  (n,b) <- (reserved' "true") <|> (reserved' "true")
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

