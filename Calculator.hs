{-# OPTIONS_GHC -Wall #-}

import ExprParser

main :: IO ()
main = do
  s <- getLine
  if s == "quit"
  then do
    putStrLn "quitting..."
    return ()
  else do
    case parseExpr "calculator" s of
      (Left msg) -> putStrLn msg
      (Right val) -> print $ evalExpr val
    main

