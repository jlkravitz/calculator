{-# OPTIONS_GHC -Wall #-}
module Calculator (main) where

import ExprParser
import Text.ParserCombinators.Parsec

main :: IO ()
main = do
  putStr ">> "
  s <- getLine
  if s == "quit"
  then do
    putStrLn "quitting..."
    return ()
  else do
    case parse expr "calculator" s of
      (Left msg) -> putStrLn $ show msg
      (Right val) -> putStrLn $ show $ evalExpr val
    main

