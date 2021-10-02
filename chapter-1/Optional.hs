#!/usr/bin/env cabal

{- cabal:
build-depends: base
default-language: Haskell2010
ghc-options:      -Wall -Wincomplete-uni-patterns -Wincomplete-record-updates -Wmissing-import-lists -Wcompat
-}

-- 文字列を整数に変換する。できなければ無効
toNum :: String -> Maybe Int
toNum s = case reads s of
  [(n, "")] -> Just n
  _ -> Nothing

-- 四則演算。できなければ無効
addOp :: Int -> Int -> Maybe Int
addOp a b = Just (a + b)

subOp :: Int -> Int -> Maybe Int
subOp a b = Just (a - b)

mulOp :: Int -> Int -> Maybe Int
mulOp a b = Just (a * b)

divOp :: Int -> Int -> Maybe Int
divOp _ 0 = Nothing
divOp a b = Just (a `div` b)

-- + - * / のどれかの演算子を演算に変更する
toBinOp :: String -> Maybe (Int -> Int -> Maybe Int)
toBinOp "+" = Just addOp
toBinOp "-" = Just subOp
toBinOp "*" = Just mulOp
toBinOp "/" = Just divOp
toBinOp _ = Nothing

eval :: String -> Maybe Int
eval expr = do
  -- スペースで分割する。3つに分割できなければ無効
  case words expr of
    (sa : sop : sb : _) -> do
      a <- toNum sa
      op <- toBinOp sop
      b <- toNum sb
      a `op` b
    _ -> Nothing

main :: IO ()
main = getLine >>= putStrLn . maybe "invalid" show . eval
