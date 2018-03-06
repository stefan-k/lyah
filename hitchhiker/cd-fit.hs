module Main where

import qualified Data.Functor.Identity as DFI
import qualified Text.Parsec.Prim as Prim
import Text.ParserCombinators.Parsec

-- parseInput parses output of "du -sb", which consists of many 
-- lines, each of which describes a single directory
parseInput :: Prim.ParsecT String () DFI.Identity [Dir]
parseInput = do
  dirs <- many dirAndSize
  eof :: Parser ()
  return dirs

-- Datatype Dir holds information about single directory 
-- (size and name)
data Dir =
  Dir Int
      String
  deriving (Show)

dirAndSize :: Prim.ParsecT String u DFI.Identity Dir
dirAndSize = do
  size <- many1 digit
  spaces
  dir_name <- anyChar `manyTill` newline
  return (Dir (read size) dir_name)

main :: IO ()
main = do
  input <- getContents
  let dirs =
        case parse parseInput "stdin" input of
          Left err ->
            error $ "Input:\n" ++ show input ++ "\nError:\n" ++ show err
          Right result -> result
  putStrLn "DEBUG: parsed:"
  print dirs
