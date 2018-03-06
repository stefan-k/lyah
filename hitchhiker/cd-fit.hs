module Main where

import qualified Data.Functor.Identity as DFI
import Data.List (sortBy)
import qualified Text.Parsec.Prim as Prim
import Text.ParserCombinators.Parsec

-- parseInput parses output of "du -sb", which consists of many 
-- lines, each of which describes a single directory
parseInput :: Prim.ParsecT String () DFI.Identity [Dir]
parseInput = do
  ddirs <- many dirAndSize
  eof :: Parser ()
  return ddirs

-- Datatype Dir holds information about single directory 
-- (size and name)
data Dir = Dir
  { dirSize :: Int
  , dirName :: String
  } deriving (Show)

data DirPack = DirPack
  { packSize :: Int
  , dirs :: [Dir]
  } deriving (Show)

mediaSize :: Int
mediaSize = 700 * 1024 * 1024

dirAndSize :: Prim.ParsecT String u DFI.Identity Dir
dirAndSize = do
  size <- many1 digit
  spaces
  dir_name <- anyChar `manyTill` newline
  return (Dir (read size) dir_name)

greedyPack :: [Dir] -> DirPack
greedyPack ddirs = foldl maybeAddDir (DirPack 0 []) $ sortBy cmpSize ddirs
  where
    cmpSize d1 d2 = compare (dirSize d1) (dirSize d2)

maybeAddDir :: DirPack -> Dir -> DirPack
maybeAddDir p d =
  let newSize = packSize p + dirSize d
      newDirs = d : dirs p
  in if newSize > mediaSize
       then p
       else DirPack newSize newDirs

main :: IO ()
main = do
  input <- getContents
  let ddirs =
        case parse parseInput "stdin" input of
          Left err ->
            error $ "Input:\n" ++ show input ++ "\nError:\n" ++ show err
          Right result -> result
  putStrLn "DEBUG: parsed:"
  print ddirs
  putStrLn "Solution: "
  print (greedyPack ddirs)
