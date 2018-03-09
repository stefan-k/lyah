module Main where

import Control.Monad (unless)
import System.Random

main :: IO ()
main = do
  gen <- getStdGen
  askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do
  let (randNumber, newGen) = randomR (1, 10) gen :: (Int, StdGen)
  putStr "Which number in the Range from 1 to 10 am I thinking of? "
  numberString <- getLine
  unless (null numberString) $ do
    let number = read numberString
    if randNumber == number
      then putStrLn "Correct!"
      else putStrLn $ "Sorry, it was " ++ show randNumber
    askForNumber newGen
