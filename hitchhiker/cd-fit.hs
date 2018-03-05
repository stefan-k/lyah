module Main where

import Text.ParserCombinators.Parsec

-- parseInput parses output of "du -sb", which consists of many 
-- lines, each of which describes a single directory
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

dirAndSize = do
  size <- many1 digit
  spaces
  dir_name <- anyChar `manyTill` newline
  return (Dir (read size) dir_name)

main = do
  input <- getContents
  let dirs =
        case parse parseInput "stdin" input of
          Left err ->
            error $ "Input:\n" ++ show input ++ "\nError:\n" ++ show err
          Right result -> result
  putStrLn "DEBUG: parsed:"
  print dirs
