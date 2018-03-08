module Main where

import Control.Monad
import Data.Char

-- 1
{- main :: IO () -}
{- main = do -}
{-   putStrLn "Hello, what's your name?" -}
{-   name <- getLine -}
{-   putStrLn $ "Hey " ++ name -}
-- 2
{- main :: IO () -}
{- main = do -}
{-   putStrLn "What's your first name?" -}
{-   firstName <- getLine -}
{-   putStrLn "What's your last name?" -}
{-   lastName <- getLine -}
{-   let bigFirstName = map toUpper firstName -}
{-       bigLastName = map toUpper lastName -}
{-   putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName -}
-- 3
{- reverseWords :: String -> String -}
{- reverseWords = unwords . map reverse . words -}
{-  -}
{- main :: IO () -}
{- main = do -}
{-   line <- getLine -}
{-   if null line -}
{-     then return () -}
{-     else do -}
{-       putStrLn $ reverseWords line -}
{-       main -}
-- 4
{- main :: IO () -}
{- main = do -}
{-   c <- getChar -}
{-   {- if c /= ' ' -} -}
{-   {-   then do -} -}
{-   {-     putChar c -} -}
{-   {-     main -} -}
{-   {-   else return () -} -}
{-   when (c /= ' ') $ do -}
{-     putChar c -}
{-     main -}
-- 5
{- main :: IO () -}
{- main -}
{-   -- rs <- sequence [getLine, getLine, getLine] -}
{-   -- rs <- sequence $ replicate 3 getLine -}
{-  = do -}
{-   rs <- replicateM 3 getLine -}
{-   print rs -}
-- 6
{- main :: IO [()] -}
{- main = do -}
{-   sequence (map print [1, 2, 3, 4]) -}
-- 7
{- main :: IO () -}
{- main = mapM_ print [1, 2, 3, 4] -}
-- 8
main :: IO [()]
main = do
  colors <-
    forM
      [1 :: Integer, 2, 3, 4]
      (\a -> do
         putStrLn $
           "Which color do you associate with the number " ++ show a ++ "?"
         getLine)
  putStrLn "the collors that you associate with 1, 2, 3 and 4 are: "
  mapM putStrLn colors
