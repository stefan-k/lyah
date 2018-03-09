module Main where

import Control.Monad
import Data.Char
import Data.List
import System.Directory
import System.IO

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
{- main :: IO () -}
{- main = interact respondPalindrome -}
{-  -}
{- {- respondPalindrome :: String -> String -} -}
{- {- respondPalindrome contents = -} -}
{- {-   unlines -} -}
{- {-     (map -} -}
{- {-        (\xs -> -} -}
{- {-           if isPalindrome xs -} -}
{- {-             then "palindrome" -} -}
{- {-             else "not palindrome") -} -}
{- {-        (lines contents)) -} -}
{- {-   where -} -}
{- {-     isPalindrome xs = xs == reverse xs -} -}
{- respondPalindrome :: String -> String -}
{- respondPalindrome = -}
{-   unlines . -}
{-   map -}
{-     (\xs -> -}
{-        if isPalindrome xs -}
{-          then "palindrome" -}
{-          else "not palindrome") . -}
{-   lines -}
{-   where -}
{-     isPalindrome xs = xs == reverse xs -}
-- 7
{- main :: IO () -}
{- main = do -}
{-   handle <- openFile "girlfriend.txt" ReadMode -}
{-   contents <- hGetContents handle -}
{-   putStr contents -}
{-   hClose handle -}
-- 8
{- main :: IO () -}
{- main = -}
{-   withFile -}
{-     "girlfriend.txt" -}
{-     ReadMode -}
{-     (\handle -> do -}
{-        contents <- hGetContents handle -}
{-        putStr contents) -}
-- 9
{- main :: IO () -}
{- main = do -}
{-   contents <- readFile "girlfriend.txt" -}
{-   writeFile "girlfriendcaps.txt" $ map toUpper contents -}
-- 10
{- main :: IO () -}
{- main = do -}
{-   todoItem <- getLine -}
{-   appendFile "todo.txt" $ todoItem ++ "\n" -}
-- 11
main :: IO ()
main = do
  handle <- openFile "todo.txt" ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents handle
  let todoTasks = lines contents
      numberedTasks =
        zipWith (\n line -> show n ++ " - " ++ line) [0 :: Integer ..] todoTasks
  putStrLn "these are your todo items: "
  putStr $ unlines numberedTasks
  putStrLn "Which one do you want to delete?"
  numberString <- getLine
  let number = read numberString
      newTodoItems = delete (todoTasks !! number) todoTasks
  hPutStr tempHandle $ unlines newTodoItems
  hClose handle
  hClose tempHandle
  removeFile "todo.txt"
  renameFile tempName "todo.txt"
