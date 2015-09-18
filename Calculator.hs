{-# OPTIONS_GHC -Wall #-}

import Expr

main :: IO ()
main = do
  s <- getLine
  if s == "quit"
  then return ()
  else do
    case parseExpr "calculator" s of
      (Left msg) -> putStrLn msg
      (Right val) -> print $ evalExpr val
    main

