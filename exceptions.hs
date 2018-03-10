module Main where

import Control.Exception
import System.Directory
import System.Environment
import System.IO
import System.IO.Error

-- 1
{- main :: IO () -}
{- main = do -}
{-   (fileName:_) <- getArgs -}
{-   contents <- readFile fileName -}
{-   putStrLn $ "the file has " ++ show (length $ lines contents) ++ " lines." -}
-- 2
{- main :: IO () -}
{- main = do -}
{-   (fileName:_) <- getArgs -}
{-   fileExists <- doesFileExist fileName -}
{-   if fileExists -}
{-     then do -}
{-       contents <- readFile fileName -}
{-       putStrLn $ "the file has " ++ show (length $ lines contents) ++ " lines." -}
{-     else putStrLn "file does not exist" -}
-- 3
main :: IO ()
main = toTry `catch` handler

toTry :: IO ()
toTry = do
  (fileName:_) <- getArgs
  contents <- readFile fileName
  putStrLn $ "The file has " ++ show (length $ lines contents) ++ " lines!"

handler :: IOError -> IO ()
handler e
  | isDoesNotExistError e =
    case ioeGetFileName e of
      Just path -> putStrLn $ "File does not exist at: " ++ path
      Nothing -> putStrLn "file does not exist"
  | otherwise = ioError e
