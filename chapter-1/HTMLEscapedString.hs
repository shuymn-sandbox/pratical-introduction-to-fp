#!/usr/bin/env cabal

{- cabal:
build-depends: base
default-language: Haskell2010
ghc-options:      -Wall -Wincomplete-uni-patterns -Wincomplete-record-updates -Wmissing-import-lists -Wcompat
-}

newtype HTMLEscapedString = HTMLEscapedString String

escape :: String -> HTMLEscapedString
escape str = HTMLEscapedString (str >>= escapeAmp >>= escapeOther)
  where
    escapeAmp '&' = "&amp;"
    escapeAmp c = [c]
    escapeOther '<' = "&lt;"
    escapeOther '>' = "&gt;"
    escapeOther '"' = "&quot;"
    escapeOther c = [c]

putHtmlEscapedStrLn :: HTMLEscapedString -> IO ()
putHtmlEscapedStrLn (HTMLEscapedString str) = putStrLn str

main :: IO ()
main = do
  rawString <- getLine
  let escapedString = escape rawString
  putHtmlEscapedStrLn escapedString
