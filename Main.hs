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

evalInput :: String -> Either ParseError Integer
evalInput = parse expr "calculator"

expr :: Parser Integer
expr = do
  skipMany space
  first <- literal
  pairs <- many pair
  eof
  return $ evalExpr (generateExpr (Const first) pairs)

literal :: Parser Integer
literal = read <$> many1 digit

pair :: Parser (Operator, Expr)
pair = do
  skipMany space
  o <- op
  skipMany space
  i <- literal
  return (o, Const i)

op :: Parser Operator
op = chToOp <$> (char '+' <|> char '-' <|> char '*' <|> char '/')
  where chToOp :: Char -> Operator
        chToOp c = snd $ fromJust $ find ((== c) . fst) table
        table = [('+', Add), ('-', Sub), ('*', Mul), ('/', Div)]

evalExpr :: Expr -> Integer
evalExpr (Const x) = x
evalExpr (Add a b) = evalExpr a + evalExpr b
evalExpr (Sub a b) = evalExpr a - evalExpr b
evalExpr (Mul a b) = evalExpr a * evalExpr b
evalExpr (Div a b) = (evalExpr a) `quot` (evalExpr b)

generateExpr :: Expr -> [(Operator, Expr)] -> Expr
generateExpr = foldl' (\acc (o, e) -> o acc e)

