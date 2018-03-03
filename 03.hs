compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/ 10)

divideTenBy :: (Floating a) => a -> a
divideTenBy = (10 /)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A' .. 'Z'])

mulThree :: (Num a) => a -> a -> a -> a
mulThree x y z = x * y * z

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort (filter (<= x) xs)
      biggerSorted = quicksort (filter (> x) xs)
  in smallerSorted ++ [x] ++ biggerSorted

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999 ..])
  where
    p x = x `mod` 3829 == 0

sumOfOddSquares :: (Integral a) => a
sumOfOddSquares = sum (takeWhile (< 10000) (filter odd (map (^ 2) [1 ..])))

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | even n = n : chain (n `div` 2)
  | otherwise = n : chain (n * 3 + 1)

countChains :: Int
countChains = length (filter (\xs -> length xs > 15) (map chain [1 .. 100]))

sum' :: (Num a) => [a] -> a
{- sum' xs = foldl (\acc x -> acc + x) 0 xs -}
sum' = foldl (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' y = foldl (\acc x -> acc || (x == y)) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

mapl :: (a -> b) -> [a] -> [b]
mapl f = foldl (\acc x -> acc ++ [f x]) []

maximum' :: (Ord a) => [a] -> a
maximum' =
  foldr1
    (\x acc ->
       if x > acc
         then x
         else acc)

reverse' :: [a] -> [a]
{- reverse' = foldl (\acc x -> x : acc) [] -}
reverse' = foldl (flip (:)) []

product' :: (Num a) => [a] -> a
product' = foldl1 (*)

filter' :: (a -> Bool) -> [a] -> [a]
filter' f =
  foldr
    (\x acc ->
       if f x
         then x : acc
         else acc)
    []

head' :: [a] -> a
{- head' = foldr1 (\x _ -> x) -}
head' = foldr1 const

last' :: [a] -> a
{- last' = foldl1 (\x _ -> x) -}
last' = foldl1 const

sqrtSums :: Int
{- sqrtSums = length (takeWhile (< 1000) (scanl1 (+) (map sqrt [1 ..]))) + 1 -}
sqrtSums = 1 + length (takeWhile (< 1000) $ scanl1 (+) $ map sqrt [1 ..])
