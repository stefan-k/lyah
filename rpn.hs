import Data.List

solveRPN :: String -> Float
solveRPN = head . foldl foldingFunction [] . words
  where
    foldingFunction (x:y:ys) "*" = (x * y) : ys
    foldingFunction (x:y:ys) "+" = (x + y) : ys
    foldingFunction (x:y:ys) "/" = (x / y) : ys
    foldingFunction (x:y:ys) "-" = (y - x) : ys
    foldingFunction (x:y:ys) "^" = (y ** x) : ys
    foldingFunction (x:ys) "log" = log x : ys
    foldingFunction xs "sum" = [sum xs]
    foldingFunction xs numberString = read numberString : xs
