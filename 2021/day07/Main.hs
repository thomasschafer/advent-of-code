module Main (main) where

import Aoc (splitBy)

solveFuelRequired :: (Int -> Int -> Int) -> String -> Int
solveFuelRequired fuelFromDistance positionsData =
  foldl calcFuelSpent fuelUpperBound [minPos .. maxPos]
  where
    positions = map read $ splitBy ',' positionsData :: [Int]
    minPos = minimum positions
    maxPos = maximum positions
    fuelUpperBound = fuelFromDistance maxPos minPos * length positions

    calcFuelSpent minFuel pos = min minFuel curFuelRequired
      where
        curFuelRequired = sum $ map (fuelFromDistance pos) positions

distance :: (Num a) => a -> a -> a
distance = (abs .) . (-)

solvePart1 :: String -> Int
solvePart1 = solveFuelRequired distance

solvePart2 :: String -> Int
solvePart2 = solveFuelRequired $ (fuelReq .) . distance
  where
    fuelReq dist = dist * (dist + 1) `div` 2

main :: IO ()
main = do
  realData <- getContents
  print $ solvePart1 realData
  print $ solvePart2 realData
