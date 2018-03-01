doubleMe x = x + x

{- doubleUs x y = x * 2 + y * 2 -}
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x =
  if x > 100
    then x
    else x * 2

doubleSmallNumber' x =
  (if x > 100
     then x
     else x * 2) +
  1

lostNumbers = [4, 8, 15, 16, 23, 42]

boomBangs xs =
  [ if x < 10
    then "Boom!"
    else "Bang!"
  | x <- xs
  , odd x
  ]

length' xs = sum [1 | _ <- xs]

removeNonUppercase :: String -> String
removeNonUppercase st = [c | c <- st, c `elem` ['A' .. 'Z']]

removeOddNumbers xxs = [[x | x <- xs, even x] | xs <- xxs]

rightTriangles x =
  [ (a, b, c)
  | c <- [1 .. x]
  , b <- [1 .. c]
  , a <- [1 .. b]
  , a ^ 2 + b ^ 2 == c ^ 2
  ]

lucky :: (Integral a) => a -> String
lucky 7 = "Lucky number seven!"
lucky x = "Sorry you're out of luck."

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

{- addVectors1 :: (Num a) => (a, a) -> (a, a) -> (a, a) -}
{- addVectors1 a b = (fst a + fst b, snd a + snd b) -}
addVectors2 :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
addVectors2 (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, x, _) = x

third :: (a, b, c) -> c
third (_, _, x) = x

head' :: [a] -> a
head' [] = error "Can't call head on an empty list"
head' (x:_) = x

length'' :: (Num b) => [a] -> b
length'' [] = 0
length'' (_:xs) = 1 + length'' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital "" = "empty string"
capital all@(x:xs) = "the first letter of " ++ all ++ " is " ++ [x]

{- bmiTell :: (RealFloat a) => a -> String -}
{- bmiTell bmi -}
{-   | bmi <= 18.5 = "underweight" -}
{-   | bmi <= 25.0 = "normal" -}
{-   | bmi <= 30.0 = "fat" -}
{-   | otherwise = "whale" -}
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= 18.5 = "underweight"
  | bmi <= 25.0 = "normal"
  | bmi <= 30.0 = "fat"
  | otherwise = "whale"
  where
    bmi = weight / height ^ 2

max' :: (Ord a) => a -> a -> a
max' x y
  | x > y = x
  | otherwise = y

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  | a > b = GT
  | a == b = EQ
  | otherwise = LT

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in sideArea + 2 * topArea
