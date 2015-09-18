{-# OPTIONS_GHC -Wall #-}
module ExprParser (expr, evalExpr) where

import Data.List
import Control.Applicative ((<$>))
import Text.ParserCombinators.Parsec
import Data.Maybe

-- type Operator = Expr -> Expr -> Expr
data Operator = Add | Sub | Mul | Div deriving Show
data Expr = Const Integer
          | Op Operator Expr Expr
          deriving Show

expr :: Parser Expr
expr = do
  skipMany space
  first <- literal
  pairs <- many pair
  eof
  let literals  = Const first:map (Const . snd) pairs
      operators = map fst pairs
  return $ buildExpr literals operators

literal :: Parser Integer
literal = read <$> many1 digit

pair :: Parser (Operator, Integer)
pair = do
  skipMany space
  o <- op
  skipMany space
  i <- literal
  return (o, i)

op :: Parser Operator
op = chToOp <$> (char '+' <|> char '-' <|> char '*' <|> char '/')
  where chToOp :: Char -> Operator
        chToOp c = snd $ fromJust $ find ((== c) . fst) table
        table = [('+', Add), ('-', Sub), ('*', Mul), ('/', Div)]

buildExpr :: [Expr] -> [Operator] -> Expr
buildExpr (first:operands) operators = evalExpr' operands operators [first] []
  where evalExpr' (x:xs) (y:ys)  litStack opStack = evalExpr' xs ys (x:litStack') (y:opStack')
          where (litStack', opStack') = combineHigherPrecedence litStack opStack y
        evalExpr' [] [] litStack opStack = combineExpressions litStack opStack
        evalExpr' _ _ _ _ = error "wtf"
buildExpr _ _ = error "wtf"

combineHigherPrecedence :: [Expr] -> [Operator] -> Operator -> ([Expr], [Operator])
combineHigherPrecedence (a:b:xs) (y:ys) curOp
  | precedence y >= precedence curOp = combineHigherPrecedence (Op y a b:xs) ys curOp
combineHigherPrecedence xs ys _ = (xs, ys)

combineExpressions :: [Expr] -> [Operator] -> Expr
combineExpressions (a:b:exprs) (y:ys) = combineExpressions ((Op y a b):exprs) ys
combineExpressions (x:[]) [] = x
combineExpressions _ _ = error "wtf"

evalExpr :: Expr -> Integer
evalExpr (Const x) = x
evalExpr (Op y a b) = performOperation y (evalExpr a) (evalExpr b)

precedence :: Operator -> Integer
precedence Add = 0
precedence Sub = 0
precedence Mul = 1
precedence Div = 1

performOperation :: Operator -> Integer -> Integer -> Integer
performOperation Add x y = y + x
performOperation Sub x y = y - x
performOperation Mul x y = y * x
performOperation Div x y = y `quot` x

