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
