module Main where

import Control.Monad (liftM2, replicateM)
import qualified Data.Functor.Identity as DFI
import Data.Ix (inRange)
import Data.List (maximumBy, notElem, sortBy)
import Test.QuickCheck
import qualified Text.Parsec.Prim as Prim
import Text.ParserCombinators.Parsec

-- we must teach QuickCheck how to generate arbitrary "Dir"s
instance Arbitrary Dir where
  arbitrary = liftM2 Dir genSize genName
    where
      genSize = do
        s <- choose (10, 1400)
        return (s * 2014 * 1024)
      genName = do
        n <- choose (1, 300)
        replicateM n (elements "foobar/")

-- for convenience and by tradition, all QuickCheck tests begin with prefix "prop_".
-- Assume that "ds" will be a random list of "Dir"s and coud your test
prop_GreedyPackIsFixpoint :: [Dir] -> Bool
prop_GreedyPackIsFixpoint ds =
  let pack = greedyPack ds
  in packSize pack == packSize (greedyPack (dirs pack))

prop_DynamicPackIsFixpoint :: [Dir] -> Bool
prop_DynamicPackIsFixpoint ds =
  let pack = dynamicPack mediaSize ds
  in packSize pack == packSize (dynamicPack mediaSize (dirs pack))

prop_DynamicPackSmallDisk :: [Dir] -> Bool
prop_DynamicPackSmallDisk ds =
  let pack = dynamicPack 50000 ds
  in packSize pack == packSize (dynamicPack 50000 (dirs pack))

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
  { dirSize :: Integer
  , dirName :: String
  } deriving (Show, Eq)

data DirPack = DirPack
  { packSize :: Integer
  , dirs :: [Dir]
  } deriving (Show)

mediaSize :: Integer
mediaSize = 700 * 1024 * 1024

dirAndSize :: Prim.ParsecT String u DFI.Identity Dir
dirAndSize = do
  size <- many1 digit
  spaces
  dir_name <- anyChar `manyTill` newline
  return (Dir (read size) dir_name)

greedyPack :: [Dir] -> DirPack
greedyPack ddirs = foldl maybeAddDir (DirPack 0 []) $ sortBy cmpSize' ddirs
  where
    cmpSize' d1 d2 = compare (dirSize d1) (dirSize d2)

maybeAddDir :: DirPack -> Dir -> DirPack
maybeAddDir p d =
  let newSize = packSize p + dirSize d
      newDirs = d : dirs p
  in if newSize > mediaSize
       then p
       else DirPack newSize newDirs

-- Dynamic programming solution to the knapsack problem
{- precomputeDisksFor :: [Dir] -> [DirPack] -}
{- precomputeDisksFor ddirs = -}
{-   let precomp = map bestDisk [0 ..] -}
{-       bestDisk 0 = DirPack 0 [] -}
{-       bestDisk limit = -}
{-         case [ DirPack (dirSize d + s) (d : ds) -}
{-              | d <- filter (inRange (1, limit) . dirSize) ddirs -}
{-              , dirSize d > 0 -}
{-              , let (DirPack s ds) = precomp !! fromInteger (limit - dirSize d) -}
{-              , d `notElem` ds -}
{-              ] of -}
{-           [] -> DirPack 0 [] -}
{-           packs -> maximumBy cmpSize packs -}
{-       cmpSize a b = compare (packSize a) (packSize b) -}
{-   in precomp -}
bestDisk :: Integer -> [Dir] -> DirPack
bestDisk 0 _ = DirPack 0 []
bestDisk _ [] = DirPack 0 []
bestDisk limit ddirs =
  case [ DirPack (dirSize d + s) (d : ds)
       | let smallEnough = filter (inRange (1, limit) . dirSize) ddirs
       , d <- smallEnough
       , dirSize d > 0
       , let (DirPack s ds) =
               bestDisk (limit - dirSize d) (delete d smallEnough)
       ] of
    [] -> DirPack 0 []
    packs -> maximumBy cmpSize packs

cmpSize :: DirPack -> DirPack -> Ordering
cmpSize a b = compare (packSize a) (packSize b)

dynamicPack :: Integer -> [Dir] -> DirPack
dynamicPack = bestDisk

{- dynamicPack limit ddirs = precomputeDisksFor ddirs !! fromInteger limit -}
main :: IO ()
main = quickCheck prop_DynamicPackIsFixpoint

{- main = quickCheck prop_DynamicPackSmallDisk -}
moin :: IO ()
moin = do
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
