module Main where

import System.Random

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
  let (firstCoin, newGen) = random gen
      (secondCoin, newGen') = random newGen
      (thirdCoin, _) = random newGen'
  in (firstCoin, secondCoin, thirdCoin)

randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen =
  let (value, newGen) = random gen
  in value : randoms' newGen

finiteRandoms :: (RandomGen g, Random a, Integral n) => n -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen =
  let (value, newGen) = random gen
      (restOfList, finalGen) = finiteRandoms (n - 1) newGen
  in (value : restOfList, finalGen)

main :: IO ()
main = do
  gen <- getStdGen
  putStrLn $ take 20 (randomRs ('a', 'z') gen)
  gen' <- newStdGen
  putStrLn $ take 20 (randomRs ('a', 'z') gen')
