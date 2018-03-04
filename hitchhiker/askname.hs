module Main where

main = do
  putStr "Name? "
  input <- getLine
  putStrLn ("Hello, " ++ input)
  putStr "Favourite color? "
  input <- getLine
  putStrLn input
