{-# OPTIONS_GHC -Wall #-}
module Main where

import Data.List
import Control.Applicative ((<$>))
import Text.ParserCombinators.Parsec
import Data.Maybe

-- type Operator = Expr -> Expr -> Expr
data Operator = Add | Sub | Mul | Div deriving Show
data Expr = Const Integer
          | Op Operator Expr Expr
          deriving Show

main :: IO ()
main = do
  putStr ">> "
  s <- getLine
  if s == "quit"
  then do
    putStrLn "quitting..."
    return ()
  else do
    case evalInput s of
      (Left msg) -> putStrLn $ show msg
      (Right val) -> putStrLn $ show $ evalExpr val
    main

evalInput :: String -> Either ParseError Expr
evalInput = parse expr "calculator"

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
  where evalExpr' (x:xs) (op:ops)  litStack opStack = evalExpr' xs ops (x:litStack') (op:opStack')
          where (litStack', opStack') = combineHigherPrecedence litStack opStack op
        evalExpr' [] [] litStack opStack = combineExpressions litStack opStack
        evalExpr' _ _ _ _ = error "wtf"
buildExpr _ _ = error "wtf"

combineHigherPrecedence :: [Expr] -> [Operator] -> Operator -> ([Expr], [Operator])
combineHigherPrecedence (x:y:xs) (op:ops) curOp
  | precedence op >= precedence curOp = combineHigherPrecedence (Op op x y:xs) ops curOp
combineHigherPrecedence xs ops _ = (xs, ops)

combineExpressions :: [Expr] -> [Operator] -> Expr
combineExpressions (x:y:exprs) (op:ops) = combineExpressions ((Op op x y):exprs) ops
combineExpressions (x:[]) [] = x
combineExpressions _ _ = error "wtf"

evalExpr :: Expr -> Integer
evalExpr (Const x) = x
evalExpr (Op op a b) = performOperation op (evalExpr a) (evalExpr b)

precedence :: Operator -> Integer
precedence Add = 0
precedence Sub = 0
precedence Mul = 1
precedence Div = 1

performOperation :: Operator -> Integer -> Integer -> Integer
performOperation Add x y = x + y
performOperation Sub x y = y - x
performOperation Mul x y = x * y
performOperation Div x y = x `quot` y

