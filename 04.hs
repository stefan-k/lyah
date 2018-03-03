import Data.Char
import Data.List (group, nub, sort, tails)
import qualified Data.Map as M
import qualified Data.Set as Set

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency = map (\l@(x:xs) -> (x, length l)) . group . sort

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
  let nlen = length needle
  in foldl (\acc x -> (take nlen x == needle) || acc) False (tails haystack)

encode :: Int -> String -> String
encode shift msg =
  let ords = map ord msg
      shifted = map (+ shift) ords
  in map chr shifted

decode :: Int -> String -> String
decode shift = encode (-shift)

phoneBook =
  [ ("betty", "55555")
  , ("bonnie", "44444")
  , ("patsy", "33333")
  , ("lucille", "222222")
  , ("wendy", "11111")
  , ("penny", "00000")
  ]

{- findKey :: (Eq k) => k -> [(k, v)] -> v -}
{- findKey key xs = snd $ head $ filter (\(k, v) -> k == key) xs -}
{- findKey :: (Eq k) => k -> [(k, v)] -> Maybe v -}
{- findKey key [] = Nothing -}
{- findKey key ((k, v):xs) = -}
{-   if key == k -}
{-     then Just v -}
{-     else findKey key xs -}
findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key [] = Nothing
findKey key xs =
  foldl
    (\acc (k, v) ->
       if k == key
         then Just v
         else acc)
    Nothing
    xs

text1 = "I just had an balblablafurz"

text2 =
  "the old man left his garbage can out and now this trash is all over my lawn!"

set1 = Set.fromList text1

set2 = Set.fromList text2
