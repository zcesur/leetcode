module ParsingABooleanExpression where

import           Text.Parsec
import           Text.Parsec.String
import           Data.Functor

data Expr = Not Expr
          | And [Expr]
          | Or [Expr]
          | Bool Bool
          deriving (Show)

-- | Return the result of evaluating a given boolean expression, represented as a string.
parseEval :: String -> Either ParseError Bool
parseEval s = eval <$> parse expr "" s

eval :: Expr -> Bool
eval ex = case ex of
  Not  e  -> not (eval e)
  And  es -> all eval es
  Or   es -> any eval es
  Bool b  -> b

expr :: Parser Expr
expr = notExpr <|> andExpr <|> orExpr <|> boolExpr <|> withParens expr

exprs :: Parser [Expr]
exprs = (:) <$> expr <*> many (sep *> expr)

notExpr :: Parser Expr
notExpr = Not <$> (char '!' *> withParens expr)

andExpr :: Parser Expr
andExpr = And <$> (char '&' *> withParens exprs)

orExpr :: Parser Expr
orExpr = Or <$> (char '|' *> withParens exprs)

boolExpr :: Parser Expr
boolExpr = Bool <$> (true <|> false)

true :: Parser Bool
true = char 't' $> True

false :: Parser Bool
false = char 'f' $> False

withParens :: Parser a -> Parser a
withParens p = char '(' *> p <* char ')'

sep :: Parser Char
sep = char ','
