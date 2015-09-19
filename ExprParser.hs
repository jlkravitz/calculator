{-# OPTIONS_GHC -Wall #-}
module ExprParser (parseExpr, evalExpr) where

import Data.List
import Control.Applicative ((<$>), (*>), (<*))
import Text.ParserCombinators.Parsec
import Data.Maybe


---------- Data Types ----------

data Operator = Add | Sub | Mul | Div deriving Show
data Expr = Literal Integer
          | Op Operator Expr Expr
          deriving Show


---------- Parsing Expressions with Parsec ----------

parseExpr :: SourceName -> String -> Either String Expr
parseExpr srcName s =
  case parse exprParser srcName s of
    (Left msg) -> Left $ show msg
    (Right val) -> Right val

exprParser :: Parser Expr
exprParser = skipMany space *> expr <* eof

-- Note: Assumes there is no leading whitespace.
expr :: Parser Expr
expr = do
  first <- literal <|> parenthesizedExpr
  skipMany space
  pairs <- many pair
  let operands  = map snd pairs
      operators = map fst pairs
  return $ buildExpr operands operators [first] []

literal :: Parser Expr
literal = (Literal . read) <$> many1 digit

parenthesizedExpr :: Parser Expr
parenthesizedExpr =
  char '(' *>
    skipMany space *>
      expr
    <* skipMany space
  <* char ')'

pair :: Parser (Operator, Expr)
pair = do
  o <- op
  skipMany space
  e <- literal <|> parenthesizedExpr
  skipMany space
  return (o, e)

op :: Parser Operator
op = chToOp <$> (char '+' <|> char '-' <|> char '*' <|> char '/')
  where chToOp :: Char -> Operator
        chToOp c = snd $ fromJust $ find ((== c) . fst) table
        table = [('+', Add), ('-', Sub), ('*', Mul), ('/', Div)]


----------  Building Expressions with the Shunting-Yard algorithm ----------

buildExpr :: [Expr] -> [Operator] -> [Expr] -> [Operator] -> Expr
buildExpr (x:xs) (y:ys) litStack opStack = buildExpr xs ys (x:litStack') (y:opStack')
  where (litStack', opStack') = combineHigherPrecedence litStack opStack y
buildExpr [] [] litStack opStack = combineExpressions litStack opStack
buildExpr _ _ _ _ = error "Something went horribly wrong!"

-- Combine all expressions involving precedences higher than the given operator.
-- Return the new expression and operator stacks.
combineHigherPrecedence :: [Expr] -> [Operator] -> Operator -> ([Expr], [Operator])
combineHigherPrecedence (a:b:xs) (y:ys) curOp
  | precedence y >= precedence curOp = combineHigherPrecedence (Op y a b:xs) ys curOp
combineHigherPrecedence xs ys _ = (xs, ys)

precedence :: Operator -> Integer
precedence Add = 0
precedence Sub = 0
precedence Mul = 1
precedence Div = 1

combineExpressions :: [Expr] -> [Operator] -> Expr
combineExpressions (a:b:exprs) (y:ys) = combineExpressions ((Op y a b):exprs) ys
combineExpressions (x:[]) [] = x
combineExpressions _ _ = error "Something went horribly wrong!"


---------- Evalutating Expressions ----------

evalExpr :: Expr -> Integer
evalExpr (Literal x) = x
evalExpr (Op y a b) = performOperation y (evalExpr a) (evalExpr b)

performOperation :: Operator -> Integer -> Integer -> Integer
performOperation Add x y = y + x
performOperation Sub x y = y - x
performOperation Mul x y = y * x
performOperation Div x y = y `quot` x

