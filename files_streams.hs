module Main where

import Control.Monad
import Data.Char

-- 1
{- main :: IO () -}
{- main = -}
{-   forever $ do -}
{-     putStr "Give me some input: " -}
{-     l <- getLine -}
{-     putStrLn $ map toUpper l -}
-- 2
{- main :: IO () -}
{- main = do -}
{-   contents <- getContents -}
{-   putStr $ map toUpper contents -}
-- 3
{- main :: IO () -}
{- main = do -}
{-   contents <- getContents -}
{-   putStr $ shortLinesOnly contents -}
{-  -}
{- shortLinesOnly :: String -> String -}
{- shortLinesOnly input = -}
{-   let allLines = lines input -}
{-       shortLines = filter (\line -> length line < 10) allLines -}
{-   in unlines shortLines -}
-- 4
{- main :: IO () -}
{- main = interact shortLinesOnly -}
{-  -}
{- shortLinesOnly :: String -> String -}
{- shortLinesOnly input = -}
{-   let allLines = lines input -}
{-       shortLines = filter (\line -> length line < 10) allLines -}
{-   in unlines shortLines -}
-- 5
{- main :: IO () -}
{- main = interact $ unlines . filter ((< 10) . length) . lines -}
-- 6
main :: IO ()
main = interact respondPalindrome

{- respondPalindrome :: String -> String -}
{- respondPalindrome contents = -}
{-   unlines -}
{-     (map -}
{-        (\xs -> -}
{-           if isPalindrome xs -}
{-             then "palindrome" -}
{-             else "not palindrome") -}
{-        (lines contents)) -}
{-   where -}
{-     isPalindrome xs = xs == reverse xs -}
respondPalindrome :: String -> String
respondPalindrome =
  unlines .
  map
    (\xs ->
       if isPalindrome xs
         then "palindrome"
         else "not palindrome") .
  lines
  where
    isPalindrome xs = xs == reverse xs
