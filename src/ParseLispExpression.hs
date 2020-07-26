module ParseLispExpression where

import           Text.Parsec
import           Text.Parsec.String
import           Data.List                      ( foldl' )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromMaybe )
import           Control.Monad                  ( guard )

type Assignment = (String, Expr)
type AssignmentMap = Map String Integer

data Expr = Let [Assignment] Expr
          | Add Expr Expr
          | Mul Expr Expr
          | Var String
          | Num Integer
          deriving (Show)

parseEval :: String -> Either ParseError (Maybe Integer)
parseEval s = eval <$> parse expr "" s

eval :: Expr -> Maybe Integer
eval = go Map.empty
 where
  go :: AssignmentMap -> Expr -> Maybe Integer
  go m ex = case ex of
    Let as e  -> go (foldl' assign m as) e
    Add e1 e2 -> (+) <$> go m e1 <*> go m e2
    Mul e1 e2 -> (*) <$> go m e1 <*> go m e2
    Var x     -> Map.lookup x m
    Num n     -> Just n

  assign :: AssignmentMap -> Assignment -> AssignmentMap
  assign m (k, v) = fromMaybe m (Map.insert <$> Just k <*> go m v <*> Just m)

expr :: Parser Expr
expr = letExpr <|> addExpr <|> multExpr <|> varExpr <|> numExpr <|> parens expr

letExpr :: Parser Expr
letExpr = keyword "let" *> (Let <$> many1 assignment <*> expr)

addExpr :: Parser Expr
addExpr = keyword "add" *> (Add <$> expr <*> expr)

multExpr :: Parser Expr
multExpr = keyword "mult" *> (Mul <$> expr <*> expr)

varExpr :: Parser Expr
varExpr = Var <$> identifier

numExpr :: Parser Expr
numExpr = Num <$> num

assignment :: Parser Assignment
assignment = try $ (,) <$> identifier <*> expr

identifier :: Parser String
identifier = lexeme $ (:) <$> lower <*> many (lower <|> digit)

keyword :: String -> Parser String
keyword s = try $ lexeme $ do
  ls <- many1 letter
  guard (s == ls)
  return ls

parens :: Parser a -> Parser a
parens = between (symbol '(') (symbol ')')

num :: Parser Integer
num = lexeme $ read <$> many1 digit

symbol :: Char -> Parser Char
symbol c = lexeme $ char c

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces
