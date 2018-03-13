module Main where

main :: IO ()
main = interact wordCount
    {- wordCount input = show (length (words input)) ++ "\n" -}
    {- wordCount input = show (length (lines input)) ++ "\n" -}
  where
    wordCount input = show (length input) ++ "\n"
