{-# OPTIONS_GHC -Wall #-}
module Expr (parseExpr, evalExpr) where

import Control.Applicative ((<$>), (*>), (<*))
import Text.ParserCombinators.Parsec hiding (token)
import qualified Data.Map as M


---------- Data Types ----------

data Operator = Operator {
  precedence :: Int,
  token :: Char,
  binaryFn :: (Integer -> Integer -> Integer)
}
data Expr = Literal Integer
          | Op Operator Expr Expr

allOperators :: [Operator]
allOperators = [
  Operator 0 '+' (+),
  Operator 0 '-' (-),
  Operator 1 '*' (*),
  Operator 1 '/' quot,
  Operator 2 '^' (^)]

-- Return map from key of operator to the operators themselves
mapOn :: Ord a => (Operator -> a) -> M.Map a Operator
mapOn f = M.fromList $ zip (map f allOperators) allOperators

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
op = chToOp <$> (oneOf $ map token allOperators)
  where chToOp :: Char -> Operator
        chToOp c = mapOn token M.! c


----------  Building Expressions with the Shunting-Yard algorithm ----------

buildExpr :: [Expr] -> [Operator] -> [Expr] -> [Operator] -> Expr
buildExpr (x:xs) (y:ys) operandStack operatorStack = buildExpr xs ys (x:operandStack') (y:operatorStack')
  where (operandStack', operatorStack') = combineHigherPrecedence operandStack operatorStack y
buildExpr [] [] operandStack operatorStack = combineExpressions operandStack operatorStack
buildExpr _ _ _ _ = error "Something went horribly wrong!"

-- Combine all expressions involving precedences higher than the given operator.
-- Return the new expression and operator stacks.
combineHigherPrecedence :: [Expr] -> [Operator] -> Operator -> ([Expr], [Operator])
combineHigherPrecedence (a:b:xs) (y:ys) curOp
  | precedence y >= precedence curOp = combineHigherPrecedence (Op y a b:xs) ys curOp
combineHigherPrecedence xs ys _ = (xs, ys)

combineExpressions :: [Expr] -> [Operator] -> Expr
combineExpressions (a:b:exprs) (y:ys) = combineExpressions ((Op y a b):exprs) ys
combineExpressions (x:[]) [] = x
combineExpressions _ _ = error "Something went horribly wrong!"


---------- Evalutating Expressions ----------

evalExpr :: Expr -> Integer
evalExpr (Literal x) = x
evalExpr (Op y a b) = (binaryFn y) (evalExpr b) (evalExpr a)

