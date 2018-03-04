module Main where

c = putStrLn "C!"

combine before after = do
  before
  putStrLn "in the middle"
  after

main = do
  combine c c
  let b = combine (putStrLn "Hello") (putStrLn "Bye")
      d = combine (b) (combine c c)
  putStrLn "So long"
