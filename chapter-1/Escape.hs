#!/usr/bin/env cabal

{- cabal:
build-depends: base
default-language: Haskell2010
ghc-options:      -Wall -Wincomplete-uni-patterns -Wincomplete-record-updates -Wmissing-import-lists -Wcompat
-}

escape :: String -> String
escape str = str >>= escapeAmp >>= escapeOther
  where
    escapeAmp '&' = "&amp;"
    escapeAmp c = [c]
    escapeOther '<' = "&lt;"
    escapeOther '>' = "&gt;"
    escapeOther '"' = "&quot;"
    escapeOther c = [c]

main :: IO ()
main = do
  rawString <- getLine
  let escapedString = escape rawString
  putStrLn escapedString