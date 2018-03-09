module Main where

import Data.List
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  putStrLn "The arguments are; "
  mapM_ putStrLn args
  putStrLn "The program name is: "
  putStrLn progName
