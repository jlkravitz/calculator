{-# OPTIONS_GHC -Wall #-}
module Main where

import Data.List
import Control.Applicative ((<$>))
import Text.ParserCombinators.Parsec
import Data.Maybe

type Operator = Expr -> Expr -> Expr
data Expr = Const Integer
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          deriving Show

main :: IO ()
main = do
  putStr ">> "
  s <- getLine
  case evalInput s of
    (Left msg) -> putStrLn $ show msg
    (Right val) -> putStrLn $ show val
  main

-- Assumes char is a valid operator
chToOp :: Char -> Operator
chToOp c = snd $ fromJust $ find ((== c) . fst) table
    where table = [('+', Add), ('-', Sub), ('*', Mul), ('/', Div)]

literal :: Parser Integer
literal = read <$> many1 digit

op :: Parser Operator
op = chToOp <$> (char '+' <|> char '-' <|> char '*' <|> char '/')

pair :: Parser (Operator, Expr)
pair = do
  skipMany space
  o <- op
  skipMany space
  i <- literal
  return (o, Const i)

expr :: Parser Integer
expr = do
  skipMany space
  first <- literal
  pairs <- many pair
  eof
  return $ evalExpr (generateExpr (Const first) pairs)

generateExpr :: Expr -> [(Operator, Expr)] -> Expr
generateExpr = foldl' (\acc (o, e) -> o acc e)

evalExpr :: Expr -> Integer
evalExpr (Const x) = x
evalExpr (Add a b) = evalExpr a + evalExpr b
evalExpr (Sub a b) = evalExpr a - evalExpr b
evalExpr (Mul a b) = evalExpr a * evalExpr b
evalExpr (Div a b) = (evalExpr a) `quot` (evalExpr b)

evalInput :: String -> Either ParseError Integer
evalInput = parse expr "calculator"

