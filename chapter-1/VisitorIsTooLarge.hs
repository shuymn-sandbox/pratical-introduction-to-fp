#!/usr/bin/env cabal

{- cabal:
build-depends: base
default-language: Haskell2010
ghc-options:      -Wall -Wincomplete-uni-patterns -Wincomplete-record-updates -Wmissing-import-lists -Wcompat
-}

-- 式
data Expr a
  = Plus (Expr a) (Expr a)
  | Square (Expr a)
  | Number a

-- 式の評価をする
evalExpr :: Expr Int -> Int
evalExpr (Plus e1 e2) = evalExpr e1 + evalExpr e2
evalExpr (Square e) = evalExpr e ^ (2 :: Int)
evalExpr (Number n) = n

-- 式を文字列にする
showExpr :: Expr Int -> String
showExpr (Plus e1 e2) = showExpr e1 ++ " + " ++ showExpr e2
showExpr (Square e) = "(" ++ showExpr e ++ ")^2"
showExpr (Number n) = show n

main :: IO ()
main = do
  -- e = 1 + (2 + 3)^2
  let e = Plus (Number 1) (Square (Plus (Number 2) (Number 3)))
  putStrLn (showExpr e)
  print (evalExpr e)