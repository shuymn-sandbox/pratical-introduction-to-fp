#!/usr/bin/env cabal

{- cabal:
build-depends: base, parallel
default-language: Haskell2010
ghc-options:      -Wall -Wincomplete-uni-patterns -Wincomplete-record-updates -Wmissing-import-lists -Wcompat
-}

import Control.Parallel (par, pseq)
import Data.Int (Int32)

-- 素数判定
isPrime :: Int32 -> Bool
isPrime x = all (\n -> x `mod` n /= 0) [2 .. toEnum (floor . sqrt $ (fromIntegral x :: Float))]

-- 2から1000000までの数
arr :: [Int32]
arr = [2 .. 1000000]

main :: IO ()
main = do
  let primes = reduceP (fromEnum . isPrime) (+) arr
  putStr "primes: " >> print primes

-- 計算を適用しながら集計する計算パターン
reduceP :: (b -> a) -> (a -> a -> a) -> [b] -> a
reduceP f _ [x] = f x
reduceP f (<+>) xs = (ys `par` zs) `pseq` (ys <+> zs)
  where
    len = length xs
    (ys', zs') = splitAt (len `div` 2) xs
    ys = reduceP f (<+>) ys'
    zs = reduceP f (<+>) zs'